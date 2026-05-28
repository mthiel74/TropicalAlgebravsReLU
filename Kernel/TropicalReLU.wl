(* ::Package:: *)

BeginPackage["TropicalReLU`", {"MaxPlus`"}];

(* --- public symbols --- *)

ReLUForward::usage =
"ReLUForward[net, x] evaluates a single-hidden-layer ReLU network on input x \
using the classical real arithmetic forward pass. net is given as an \
association <|\"A\" -> matrix, \"b\" -> vector, \"c\" -> vector, \"d\" -> scalar|> \
implementing f(x) = c . max(0, A x + b) + d.";

MaxPlusForward::usage =
"MaxPlusForward[net, x] evaluates the same network using only max and + \
operations: f(x) = P(x) - Q(x) where P and Q are tropical polynomials.";

TropicalDecomposition::usage =
"TropicalDecomposition[net] returns <|\"Pmon\" -> {{slope, intercept}, ...}, \
\"Qmon\" -> {{slope, intercept}, ...}, \"Pconst\" -> r, \"Qconst\" -> s|> \
so that P(x) = Max[(slope.x + intercept), Pconst] and similarly Q.";

ActivationPattern::usage =
"ActivationPattern[net, x] returns the binary vector of which hidden units \
are active (a_j . x + b_j > 0) at x.";

LinearRegionCount::usage =
"LinearRegionCount[net, opts] counts distinct activation patterns on a grid \
over the rectangle Option \"Domain\" -> {{xmin, xmax}, {ymin, ymax}}.";

TrainReLUClassifier::usage =
"TrainReLUClassifier[data, n] trains a 2 -> n -> 1 ReLU classifier on \
labelled 2D data and returns it as the association used by ReLUForward.";

DecisionBoundaryPlot::usage =
"DecisionBoundaryPlot[net, opts] plots the decision boundary together with \
the hidden-unit crease lines.";

(* Newton polytope helper lives in MaxPlus`NewtonPolytope. *)

(* --- implementation --- *)

Begin["`Private`"];

(* ---------------- forward passes ---------------- *)

ReLUForward[net_Association, x_?VectorQ] := Module[
  {a, b, c, d, h, head, headContribution},
  {a, b, c, d} = Lookup[net, {"A", "b", "c", "d"}];
  h = Map[Max[0, #] &, a . x + b];
  head = Lookup[net, "AffineHead", None];
  headContribution = If[
    AssociationQ[head],
    Lookup[head, "Slope", ConstantArray[0., Length[x]]] . x +
      Lookup[head, "Bias", 0.],
    0.];
  c . h + d + headContribution
];

ReLUForward[net_Association, X_?MatrixQ] :=
  ReLUForward[net, #] & /@ X;

(* Tropical evaluation: only max and + appear.
   For each hidden unit j with output weight c_j:
     positive: contributes c_j * max(0, a_j.x + b_j) to P
     negative: contributes (-c_j) * max(0, a_j.x + b_j) to Q
   We pull the (positive) c_j inside the max:
     c_j * max(0, a_j.x + b_j) = max(0, c_j a_j.x + c_j b_j)        if c_j > 0
     (-c_j) * max(0, a_j.x + b_j) = max(0, -c_j a_j.x - c_j b_j)    if c_j < 0
   so every contribution is itself a max of two affine pieces. P and Q are
   then sums of such expressions plus the positive/negative parts of d.

   Then f(x) = P(x) - Q(x) is computed without negation inside any max. *)

tropicalContribution[aj_, bj_, cj_] := Module[{s},
  s = Sign[cj];
  Which[
    s == 1,  {Max[0, cj (aj . #) + cj bj]} &,
    s == -1, {Max[0, -cj (aj . #) - cj bj]} &,
    True,    {0} &
  ]
];

MaxPlusForward[net_Association, x_?VectorQ] := Module[
  {a, b, c, d, dp, dm, pTerms, qTerms, P, Q, head, headContribution},
  {a, b, c, d} = Lookup[net, {"A", "b", "c", "d"}];
  dp = Max[d, 0]; dm = Max[-d, 0];
  pTerms = Table[
    If[c[[j]] > 0,
      Max[0, c[[j]] (a[[j]] . x) + c[[j]] b[[j]]],
      0],
    {j, Length[c]}];
  qTerms = Table[
    If[c[[j]] < 0,
      Max[0, -c[[j]] (a[[j]] . x) - c[[j]] b[[j]]],
      0],
    {j, Length[c]}];
  P = Total[pTerms] + dp;
  Q = Total[qTerms] + dm;
  head = Lookup[net, "AffineHead", None];
  headContribution = If[
    AssociationQ[head],
    Lookup[head, "Slope", ConstantArray[0., Length[x]]] . x +
      Lookup[head, "Bias", 0.],
    0.];
  (P - Q) + headContribution
];

MaxPlusForward[net_Association, X_?MatrixQ] :=
  MaxPlusForward[net, #] & /@ X;

(* ---------------- structural extraction ---------------- *)

(* Each ReLU unit (a_j, b_j) with output weight c_j contributes the
   tropical monomial pair {c_j a_j, c_j b_j} (if c_j > 0, goes into P;
   if c_j < 0, sign-flipped, goes into Q). The constant terms d+ and d-
   are the implicit "zero exponent" monomials in P and Q. *)

TropicalDecomposition[net_Association] := Module[
  {a, b, c, d, pmon = {}, qmon = {}, j},
  {a, b, c, d} = Lookup[net, {"A", "b", "c", "d"}];
  Do[
    Which[
      c[[j]] > 0, AppendTo[pmon, {c[[j]] a[[j]], c[[j]] b[[j]]}],
      c[[j]] < 0, AppendTo[qmon, {-c[[j]] a[[j]], -c[[j]] b[[j]]}]
    ],
    {j, Length[c]}
  ];
  <|
    "Pmon" -> pmon,         (* list of (slope, intercept), 2D slope, scalar intercept *)
    "Qmon" -> qmon,
    "Pconst" -> Max[d, 0],  (* implicit constant monomial *)
    "Qconst" -> Max[-d, 0]
  |>
];

evalTropPoly[mon_, const_, x_] := Max @@ Append[mon[[All, 1]] . x + mon[[All, 2]], const];

(* ---------------- activation patterns ---------------- *)

ActivationPattern[net_Association, x_?VectorQ] := Module[{a, b},
  {a, b} = Lookup[net, {"A", "b"}];
  Boole[Thread[a . x + b > 0]]
];

LinearRegionCount[net_Association, opts : OptionsPattern[]] := Module[
  {dom, ngrid, xs, ys, pts, patterns},
  dom = OptionValue[LinearRegionCount, {opts}, "Domain"];
  ngrid = OptionValue[LinearRegionCount, {opts}, "Resolution"];
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], ngrid - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], ngrid - 1];
  pts = Flatten[Table[{x, y}, {x, xs}, {y, ys}], 1];
  patterns = ActivationPattern[net, #] & /@ pts;
  Length[DeleteDuplicates[patterns]]
];
Options[LinearRegionCount] = {
  "Domain" -> {{-3, 3}, {-3, 3}},
  "Resolution" -> 300
};

(* ---------------- training ---------------- *)

TrainReLUClassifier[data : {{_?NumericQ, _?NumericQ} -> _Integer ...}, n_Integer,
   opts : OptionsPattern[]] := Module[
  {netSpec, trained, A, b, c, d, layers},
  netSpec = NetChain[
    {LinearLayer[n], ElementwiseLayer["ReLU"], LinearLayer[1]},
    "Input" -> 2,
    "Output" -> "Real"
  ];
  trained = NetTrain[
    netSpec,
    data,
    LossFunction -> (MeanSquaredLossLayer[]),
    MaxTrainingRounds -> OptionValue[TrainReLUClassifier, {opts}, "Rounds"],
    BatchSize -> OptionValue[TrainReLUClassifier, {opts}, "BatchSize"],
    LearningRate -> OptionValue[TrainReLUClassifier, {opts}, "LearningRate"],
    TrainingProgressReporting -> None
  ];
  layers = NetExtract[trained, All];
  A = Normal @ NetExtract[trained, {1, "Weights"}];
  b = Normal @ NetExtract[trained, {1, "Biases"}];
  c = Flatten @ Normal @ NetExtract[trained, {3, "Weights"}];
  d = First @ Flatten @ Normal @ NetExtract[trained, {3, "Biases"}];
  <|"A" -> A, "b" -> b, "c" -> c, "d" -> d, "Net" -> trained|>
];
Options[TrainReLUClassifier] = {
  "Rounds" -> 800,
  "BatchSize" -> 64,
  "LearningRate" -> 0.01
};

(* ---------------- plotting ---------------- *)

DecisionBoundaryPlot[net_Association, opts : OptionsPattern[]] := Module[
  {dom, lines, a, b, contour, creasePlot},
  dom = OptionValue[DecisionBoundaryPlot, {opts}, "Domain"];
  {a, b} = Lookup[net, {"A", "b"}];
  lines = Table[
    With[{aj = a[[j]], bj = b[[j]]},
      ContourPlot[aj . {x1, x2} + bj == 0, {x1, dom[[1, 1]], dom[[1, 2]]},
        {x2, dom[[2, 1]], dom[[2, 2]]},
        ContourStyle -> Directive[GrayLevel[0.7], Thickness[0.002]]]
    ],
    {j, Length[b]}];
  contour = ContourPlot[
    Evaluate[ReLUForward[net, {x1, x2}]] == 0,
    {x1, dom[[1, 1]], dom[[1, 2]]},
    {x2, dom[[2, 1]], dom[[2, 2]]},
    ContourStyle -> Directive[Black, Thickness[0.005]]];
  Show[lines, contour, PlotRange -> dom, AspectRatio -> 1, Frame -> True]
];
Options[DecisionBoundaryPlot] = {
  "Domain" -> {{-3, 3}, {-3, 3}}
};

(* Newton polytope helper lives in MaxPlus`NewtonPolytope; the
   TropicalDecomposition above exposes the monomial list directly so a
   user can build their own representation. *)

End[];
EndPackage[];
