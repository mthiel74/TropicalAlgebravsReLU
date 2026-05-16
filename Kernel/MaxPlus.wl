(* ::Package:: *)

(* :Title: MaxPlus *)
(* :Context: MaxPlus` *)
(* :Author:  Marco Thiel, 2026 *)
(* :Summary:
   A self-contained Wolfram Language implementation of the max-plus
   (tropical) semiring (R ∪ {-∞}, ⊕, ⊗), with operators rendered as
   ⊕ (System`CirclePlus, Max), ⊗ (System`CircleTimes, Plus),
   ⊖ (System`CircleMinus, classical subtraction), and ⊙
   (System`CircleDot, matrix product Inner[Plus, A, B, Max]).

   The package covers:

     * scalar arithmetic on R ∪ {-∞}
     * tropical vectors and matrices, identity, power, Kleene star
     * Karp's algorithm for the maximum cycle mean
       (= tropical eigenvalue)
     * tropical determinant (= permanent)
     * tropical polynomials in one or several variables, evaluation,
       Newton polytope, regular subdivision
     * tropical curves in the plane and their plotting helpers
     * a small library of textbook applications: shortest paths,
       PERT-style scheduling, max-plus linear discrete-time systems
     * conversions between a 1-hidden-layer ReLU network and a tropical
       rational map

   The operator symbols are attached to the System` context so that the
   Wolfram Language input syntax  a ⊕ b ⊕ c, A ⊙ B, A⁰, A¹, A²  works
   directly after loading this package.

   Reference: Maclagan & Sturmfels, "Introduction to Tropical Geometry",
   AMS GSM 161 (2015); Butkovic, "Max-linear Systems: Theory and
   Algorithms", Springer (2010).
*)

BeginPackage["MaxPlus`"];

(* ===== public symbols ============================================ *)
(* operators stay in System`; we re-attach usage and DownValues to them *)

System`CirclePlus::usage =
"a ⊕ b := Max[a, b].  Listable, Flat, OneIdentity, Orderless. \
Use as the binary operator with the standard ⊕ key (\\[CirclePlus]).";

System`CircleTimes::usage =
"a ⊗ b := a + b.  Listable, Flat, OneIdentity, Orderless. \
Use as the binary operator with the standard ⊗ key (\\[CircleTimes]).";

System`CircleMinus::usage =
"a ⊖ b := a - b.  The tropical 'division', defined whenever b ≠ -∞.";

System`CircleDot::usage =
"A ⊙ B := Inner[Plus, A, B, Max].  Works on matrix-matrix, \
matrix-vector, vector-matrix and gives the tropical inner product on \
two vectors.";

TropicalZero::usage =
"TropicalZero is the additive identity of the max-plus semiring, -Infinity.";

TropicalOne::usage =
"TropicalOne is the multiplicative identity of the max-plus semiring, 0.";

MPMatrixPower::usage =
"MPMatrixPower[A, k] is the k-fold tropical product A ⊙ A ⊙ ⋯ ⊙ A.  \
MPMatrixPower[A, 0] returns the tropical identity.";

MPIdentity::usage =
"MPIdentity[n] is the n×n tropical identity: 0 on the diagonal, \
-Infinity elsewhere.";

MPZero::usage =
"MPZero[m, n] is the m×n tropical zero matrix (all -Infinity).  \
MPZero[n] returns the length-n zero vector.";

MPInverseDiagonal::usage =
"MPInverseDiagonal[d] returns the tropical inverse of a diagonal matrix \
with diagonal entries d: the new diagonal is -d, off-diagonal entries \
are -Infinity.";

KleeneStar::usage =
"KleeneStar[A] computes A* = I ⊕ A ⊕ A² ⊕ ⋯ ⊕ A^(n-1) for an n×n \
matrix A with no positive-weight cycles.  Option \"MaxOrder\" -> k \
truncates the sum at A^k.";

MPTrace::usage =
"MPTrace[A] = ⊕_i A[[i,i]] = the tropical sum (max) of the diagonal.";

MPDeterminant::usage =
"MPDeterminant[A] = max over permutations σ of  Σ_i A[[i, σ(i)]].  \
This is the permanent computed over (Max, Plus).";

MPCycleMean::usage =
"MPCycleMean[A] is the maximum mean cycle weight of the directed \
weighted graph with adjacency matrix A (Karp's algorithm).  \
Equal to the unique max-plus eigenvalue when A is irreducible.";

MPEigenvalue::usage =
"MPEigenvalue[A] is an alias for MPCycleMean[A]: the tropical \
eigenvalue of an irreducible matrix.";

(* tropical polynomials *)

TropicalPolynomial::usage =
"TropicalPolynomial[exponents, coefficients] holds the tropical \
polynomial  p(x) = max_i ( exponents[[i]] . x + coefficients[[i]] ). \
Exponents may be non-negative integers (classical) or arbitrary reals \
(signomial / Newton-Puiseux extension).";

TropicalEval::usage =
"TropicalEval[poly, x] evaluates a TropicalPolynomial at a point or \
batch of points.";

NewtonPolytope::usage =
"NewtonPolytope[poly] returns the convex hull of the exponents of poly.";

LiftedPoints::usage =
"LiftedPoints[poly] returns the (exponent, coefficient) points in \
R^(d+1) used to compute the regular subdivision of Newt(poly).";

UpperHullFaces::usage =
"UpperHullFaces[poly] returns the upper-hull facets of LiftedPoints[poly], \
as lists of original-point indices.";

TropicalCurvePlot::usage =
"TropicalCurvePlot[poly, {x, y}, {{xmin,xmax},{ymin,ymax}}] plots the \
tropical curve of a bivariate tropical polynomial (the locus where the \
maximum monomial is attained by at least two terms).";

TropicalRegionPlot::usage =
"TropicalRegionPlot[poly, {x, y}, dom] colours each linear region of poly \
by the argmax monomial.";

(* applications *)

MPShortestPathDistances::usage =
"MPShortestPathDistances[adj] returns the shortest-path distance matrix \
of the directed graph with edge weights adj (entries are real or +Infinity \
for 'no edge').  Internally uses MaxPlus on the negated weights.";

MPLongestPathDistances::usage =
"MPLongestPathDistances[adj] returns the longest-path distance matrix of \
a directed acyclic graph (entries -Infinity = no edge).";

MPSchedule::usage =
"MPSchedule[durations, prereqs] computes the earliest completion time of \
each of n tasks with given durations and a list of precedence pairs \
prereqs of the form i -> j ('task i must finish before task j starts').";

MPLinearSystemEvolve::usage =
"MPLinearSystemEvolve[A, x0, k] iterates the max-plus linear system \
x_{t+1} = A ⊙ x_t for k steps starting from x0.  Returns the trajectory \
{x0, x1, …, xk}.";

(* ReLU NN ↔ tropical rational map *)

ReLUToTropicalRational::usage =
"ReLUToTropicalRational[<|\"A\" -> A, \"b\" -> b, \"c\" -> c, \"d\" -> d|>] \
returns <|\"P\" -> TropicalPolynomial[…], \"Q\" -> TropicalPolynomial[…]|> \
representing the trained 1-hidden-layer ReLU classifier as the tropical \
rational map f(x) = P(x) ⊖ Q(x).";

TropicalRationalEval::usage =
"TropicalRationalEval[<|\"P\" -> p, \"Q\" -> q|>, x] = TropicalEval[p, x] \
- TropicalEval[q, x].";

(* analytical tools that go beyond the standard literature *)

MPRealisedPatterns::usage =
"MPRealisedPatterns[net, dom, n] returns the list of distinct \
activation patterns realised by the trained net on an n x n grid \
over the rectangle dom.";

MPRegionGradient::usage =
"MPRegionGradient[net, sigma] returns the gradient of the network \
function on the region with activation pattern sigma: \
the sum  Sum_{j : sigma[[j]] == 1} c_j a_j.";

MPExactLipschitz::usage =
"MPExactLipschitz[net, dom, n] computes the TIGHT (not just an upper \
bound on) Lipschitz constant of the trained ReLU classifier net on the \
rectangle dom, by enumerating realised activation patterns on an n x n \
grid and taking the maximum gradient norm.";

MPDeadUnits::usage =
"MPDeadUnits[net, dom] returns the set of hidden-unit indices j whose \
pre-activation a_j . x + b_j stays non-positive (or non-negative) for \
all x in the rectangle dom -- units that contribute nothing to the \
network on dom and can be removed without changing the function.";

MPPruneDeadUnits::usage =
"MPPruneDeadUnits[net, dom] returns a new network with the dead units \
removed and the constant term adjusted, so it computes EXACTLY the \
same function on dom but with fewer hidden units.";

(* ===== implementation ============================================ *)

Begin["`Private`"];

TropicalZero = -Infinity;
TropicalOne  = 0;

(* --- scalar arithmetic ------------------------------------------ *)

(* The Listable attribute alone gives elementwise behaviour on equally-
   shaped arrays.  Flat collapses a ⊕ b ⊕ c into the n-ary call.
   Orderless makes the operator commutative for pattern matching.   *)

Unprotect[System`CirclePlus, System`CircleTimes, System`CircleMinus,
           System`CircleDot];

ClearAll[System`CirclePlus, System`CircleTimes];

System`CirclePlus[]            := TropicalZero;
System`CirclePlus[a_]          := a;
System`CirclePlus[a_, b_]      := Max[a, b];
System`CirclePlus[a_, b__]     := Max[a, b];
SetAttributes[System`CirclePlus,
  {Listable, Flat, OneIdentity, Orderless}];

System`CircleTimes[]           := TropicalOne;
System`CircleTimes[a_]         := a;
System`CircleTimes[a_, b_]     := a + b;
System`CircleTimes[a_, b__]    := Plus[a, b];
SetAttributes[System`CircleTimes,
  {Listable, Flat, OneIdentity, Orderless}];

System`CircleMinus[a_, b_]     := a - b;
SetAttributes[System`CircleMinus, {Listable}];

(* --- matrix arithmetic ------------------------------------------ *)

System`CircleDot[A_?MatrixQ, B_?MatrixQ] := Inner[Plus, A, B, Max];
System`CircleDot[A_?MatrixQ, v_?VectorQ] := Inner[Plus, A, v, Max];
System`CircleDot[v_?VectorQ, A_?MatrixQ] := Inner[Plus, v, A, Max];
System`CircleDot[v_?VectorQ, w_?VectorQ] := Max @@ (v + w);
System`CircleDot[a_, b_, c__]            :=
  Fold[System`CircleDot, System`CircleDot[a, b], {c}];

Protect[System`CirclePlus, System`CircleTimes, System`CircleMinus,
        System`CircleDot];

MPIdentity[n_Integer?Positive] :=
  Normal @ SparseArray[{i_, i_} -> 0, {n, n}, -Infinity];

MPZero[m_Integer?Positive, n_Integer?Positive] :=
  ConstantArray[-Infinity, {m, n}];
MPZero[n_Integer?Positive] := ConstantArray[-Infinity, n];

MPMatrixPower[A_?MatrixQ, 0]                  := MPIdentity[Length[A]];
MPMatrixPower[A_?MatrixQ, 1]                  := A;
MPMatrixPower[A_?MatrixQ, k_Integer?Positive] :=
  Nest[System`CircleDot[A, #] &, A, k - 1];

MPInverseDiagonal[d_?VectorQ] := Module[{n = Length[d], M},
  M = MPZero[n, n];
  Do[M[[i, i]] = -d[[i]], {i, n}];
  M
];

(* --- Kleene star ------------------------------------------------- *)

Options[KleeneStar] = {"MaxOrder" -> Automatic};

KleeneStar[A_?MatrixQ, OptionsPattern[]] := Module[
  {n = Length[A], maxK, acc, term},
  maxK = OptionValue["MaxOrder"];
  If[maxK === Automatic, maxK = n - 1];
  acc  = MPIdentity[n];
  term = MPIdentity[n];
  Do[
    term = System`CircleDot[term, A];
    acc  = MapThread[Max, {acc, term}, 2],
    {maxK}
  ];
  acc
];

(* --- tropical determinant (permanent) --------------------------- *)

MPDeterminant[A_?MatrixQ] := Module[{n = Length[A], perms},
  perms = Permutations[Range[n]];
  Max @@ Map[
    Total[MapThread[A[[#1, #2]] &, {Range[n], #}]] &,
    perms]
];

MPTrace[A_?MatrixQ] := Max @@ Diagonal[A];

(* --- Karp's algorithm for the max cycle mean -------------------- *)

MPCycleMean[A_?MatrixQ] := Module[
  {n = Length[A], F, k, baseline, byNode},
  (* F[[k+1]] is the row vector of max weights of paths of length exactly
     k starting from any node and ending at v, using the convention that
     all start nodes have "weight" 0.  Then by Karp's theorem the
     maximum mean cycle weight equals
        max_v  min_{0 <= k < n}  (F[[n+1, v]] - F[[k+1, v]]) / (n - k). *)
  F = ConstantArray[0., {n + 1, n}];
  F[[1]] = ConstantArray[0., n];
  Do[F[[k + 1]] = System`CircleDot[F[[k]], A], {k, 1, n}];
  baseline = F[[n + 1]];
  byNode = Table[
    With[{good = Select[Range[0, n - 1],
                  F[[# + 1, v]] > -Infinity &]},
      If[good === {} || baseline[[v]] === -Infinity, -Infinity,
        Min[(baseline[[v]] - F[[# + 1, v]])/(n - #) & /@ good]]
    ],
    {v, n}
  ];
  Max @@ byNode
];

MPEigenvalue[A_?MatrixQ] := MPCycleMean[A];

(* --- tropical polynomials --------------------------------------- *)

TropicalEval[TropicalPolynomial[exps_, coeffs_], x_?VectorQ] :=
  Max @@ (exps . x + coeffs);

TropicalEval[TropicalPolynomial[exps_, coeffs_], X_?MatrixQ] :=
  TropicalEval[TropicalPolynomial[exps, coeffs], #] & /@ X;

NewtonPolytope[TropicalPolynomial[exps_, _]] := ConvexHullMesh[exps];

LiftedPoints[TropicalPolynomial[exps_, coeffs_]] :=
  MapThread[Append, {exps, coeffs}];

UpperHullFaces[poly_TropicalPolynomial] := Module[
  {lifted = LiftedPoints[poly], hull, coords, faces, normals, upMask,
   coordIdx},
  If[Length[lifted] < 4, Return[{Range[Length[lifted]]}]];
  hull   = ConvexHullMesh[lifted];
  coords = MeshCoordinates[hull];
  faces  = MeshCells[hull, 2][[All, 1]];
  normals = Map[
    Function[ids,
      Module[{v1, v2, n},
        v1 = coords[[ids[[2]]]] - coords[[ids[[1]]]];
        v2 = coords[[ids[[3]]]] - coords[[ids[[1]]]];
        n  = Cross[v1, v2];
        n / Norm[n]
      ]],
    faces];
  upMask = Positive[normals[[All, -1]]];
  coordIdx = AssociationThread[coords -> Range[Length[coords]]];
  Map[
    Function[face,
      (First[FirstPosition[lifted, coords[[#]]]] &) /@ face],
    Pick[faces, upMask]
  ]
];

Options[TropicalCurvePlot] = {
  "Resolution"  -> 240,
  "CurveStyle"  -> Directive[Black, Thickness[0.004]],
  "ShowRegions" -> True
};

TropicalCurvePlot[TropicalPolynomial[exps_, coeffs_], {x_Symbol, y_Symbol},
  dom : {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[]] := Module[
  {n, xs, ys, argmax, palette, bg, curveLines},
  n        = OptionValue["Resolution"];
  xs       = Subdivide[xmin, xmax, n - 1];
  ys       = Subdivide[ymin, ymax, n - 1];
  argmax   = Table[
    First @ Ordering[exps . {xv, yv} + coeffs, -1],
    {yv, ys}, {xv, xs}];
  palette  = Table[Hue[(k - 1)/Length[coeffs], 0.30, 1.0],
                   {k, Length[coeffs]}];
  bg = If[OptionValue["ShowRegions"],
    ArrayPlot[argmax, ColorRules ->
        Thread[Range[Length[coeffs]] -> palette],
      DataRange -> dom, DataReversed -> True, AspectRatio -> 1,
      Frame -> True, FrameLabel -> {x, y}, PlotRangePadding -> 0],
    Graphics[{}, PlotRange -> dom, Frame -> True, AspectRatio -> 1]];
  (* mark cells whose argmax differs from any neighbour *)
  curveLines = Reap[
    Do[
      Module[{cur = argmax[[i, j]]},
        If[i < n && argmax[[i + 1, j]] =!= cur,
          Sow[{{xs[[j]], ys[[i]] + (ys[[2]] - ys[[1]]) / 2},
               {xs[[j]] + 0, ys[[i]] + (ys[[2]] - ys[[1]]) / 2}}]];
        If[j < n && argmax[[i, j + 1]] =!= cur,
          Sow[{{xs[[j]] + (xs[[2]] - xs[[1]]) / 2, ys[[i]]},
               {xs[[j]] + (xs[[2]] - xs[[1]]) / 2, ys[[i]] + 0}}]];
      ], {i, n}, {j, n}]][[2]];
  Show[bg,
    Graphics[{OptionValue["CurveStyle"], PointSize[0.002],
              Point[Flatten[curveLines, 1]]}, PlotRange -> dom]]
];

Options[TropicalRegionPlot] = {"Resolution" -> 240};

TropicalRegionPlot[TropicalPolynomial[exps_, coeffs_], {x_Symbol, y_Symbol},
  dom_, opts : OptionsPattern[]] := Module[
  {n, xs, ys, argmax, palette},
  n  = OptionValue["Resolution"];
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], n - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], n - 1];
  argmax = Table[
    First @ Ordering[exps . {xv, yv} + coeffs, -1],
    {yv, ys}, {xv, xs}];
  palette = Table[Hue[(k - 1)/Length[coeffs], 0.30, 1.0],
                  {k, Length[coeffs]}];
  ArrayPlot[argmax,
    ColorRules -> Thread[Range[Length[coeffs]] -> palette],
    DataRange -> dom, AspectRatio -> 1, Frame -> True,
    FrameLabel -> {x, y}, DataReversed -> True, PlotRangePadding -> 0]
];

(* --- shortest / longest path applications ----------------------- *)

(* Convention: adj[[i, j]] is the edge weight i -> j, with -Infinity
   meaning "no edge" for the longest-path version and +Infinity for
   the shortest-path version.                                          *)

MPLongestPathDistances[adj_?MatrixQ] := KleeneStar[adj];

MPShortestPathDistances[adj_?MatrixQ] := -KleeneStar[-adj /. -Infinity -> -Infinity];

(* --- PERT-style task scheduling ---------------------------------- *)

MPSchedule[durations_?VectorQ, prereqs_List] := Module[
  {n = Length[durations], adj, paths, earlyStart, earlyFinish},
  adj = MPZero[n, n];
  Do[
    With[{i = r[[1, 1]], j = r[[1, 2]]},
      adj[[i, j]] = durations[[i]]],
    {r, List @@@ {prereqs}}
  ];
  paths = KleeneStar[adj, "MaxOrder" -> n];
  earlyStart  = Table[
    Max[0, Max @@ paths[[All, j]]],
    {j, n}];
  earlyFinish = earlyStart + durations;
  <|"EarlyStart" -> earlyStart,
    "EarlyFinish" -> earlyFinish,
    "Makespan"   -> Max @@ earlyFinish|>
];

(* --- discrete-time max-plus linear system ------------------------ *)

MPLinearSystemEvolve[A_?MatrixQ, x0_?VectorQ, k_Integer] := Module[
  {traj = {x0}, x = x0},
  Do[
    x = System`CircleDot[A, x];
    AppendTo[traj, x],
    {k}
  ];
  traj
];

(* --- ReLU NN ↔ tropical rational map ---------------------------- *)

ReLUToTropicalRational[net_Association] := Module[
  {a, b, c, d, dim, pexps, pcoeffs, qexps, qcoeffs, j},
  {a, b, c, d} = Lookup[net, {"A", "b", "c", "d"}];
  dim = Length[a[[1]]];
  pexps   = {ConstantArray[0., dim]};
  pcoeffs = {Max[d, 0]};
  qexps   = {ConstantArray[0., dim]};
  qcoeffs = {Max[-d, 0]};
  Do[
    Which[
      c[[j]] > 0,
        AppendTo[pexps,   c[[j]] a[[j]]];
        AppendTo[pcoeffs, c[[j]] b[[j]]],
      c[[j]] < 0,
        AppendTo[qexps,   -c[[j]] a[[j]]];
        AppendTo[qcoeffs, -c[[j]] b[[j]]]
    ],
    {j, Length[c]}
  ];
  <|
    "P" -> TropicalPolynomial[pexps,  pcoeffs],
    "Q" -> TropicalPolynomial[qexps,  qcoeffs]
  |>
];

TropicalRationalEval[<|"P" -> p_, "Q" -> q_|>, x_?VectorQ] :=
  TropicalEval[p, x] - TropicalEval[q, x];

TropicalRationalEval[<|"P" -> p_, "Q" -> q_|>, X_?MatrixQ] :=
  TropicalRationalEval[<|"P" -> p, "Q" -> q|>, #] & /@ X;

(* --- analytical tools ------------------------------------------- *)

(* Realised activation patterns on a grid. *)
MPRealisedPatterns[net_Association,
    dom_ : {{-3., 3.}, {-3., 3.}}, n_Integer : 200] := Module[
  {a, b, xs, ys, pts, sig},
  {a, b} = Lookup[net, {"A", "b"}];
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], n - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], n - 1];
  pts = Flatten[Table[{x, y}, {x, xs}, {y, ys}], 1];
  sig = Boole[Thread[a . # + b > 0]] & /@ pts;
  DeleteDuplicates[sig]
];

(* Gradient on the region with activation pattern sigma. *)
MPRegionGradient[net_Association, sigma_?VectorQ] := Module[
  {a, b, c, active},
  {a, b, c} = Lookup[net, {"A", "b", "c"}];
  active = Position[sigma, 1] // Flatten;
  If[active === {}, ConstantArray[0., Length[a[[1]]]],
    Total[c[[#]] a[[#]] & /@ active]]
];

(* Exact (tight) Lipschitz constant via enumeration of realised
   activation patterns.  For a 1-hidden-layer ReLU network the
   function f is piecewise-affine; on each region the gradient is
   sum_{j active} c_j a_j, and the global Lipschitz constant is the
   maximum of the gradient norms over realised patterns.            *)

MPExactLipschitz[net_Association,
    dom_ : {{-3., 3.}, {-3., 3.}}, n_Integer : 200] := Module[
  {patterns, grads, norms},
  patterns = MPRealisedPatterns[net, dom, n];
  grads    = MPRegionGradient[net, #] & /@ patterns;
  norms    = Norm /@ grads;
  <|"Lipschitz" -> Max[norms],
    "Patterns" -> Length[patterns],
    "ArgMaxPattern" -> patterns[[First @ Ordering[norms, -1]]],
    "GradientNorms" -> norms|>
];

(* Dead unit detection: on a bounded rectangle dom the pre-activation
   a_j.x + b_j is an affine function whose max is attained at a corner.
   The unit is dead-low (ReLU never fires) iff that max is <= 0.       *)

MPDeadUnits[net_Association,
    dom_ : {{-3., 3.}, {-3., 3.}}] := Module[
  {a, b, corners, maxes, mins, deadLow, deadHigh},
  {a, b} = Lookup[net, {"A", "b"}];
  corners = Tuples[dom];                          (* 4 corners of rect *)
  maxes = Table[Max[a[[j]] . # + b[[j]] & /@ corners], {j, Length[b]}];
  mins  = Table[Min[a[[j]] . # + b[[j]] & /@ corners], {j, Length[b]}];
  deadLow  = Pick[Range[Length[b]], NonPositive /@ maxes];
  deadHigh = Pick[Range[Length[b]], NonNegative /@ mins];
  <|"NeverActive" -> deadLow,
    "AlwaysActive" -> deadHigh|>
];

(* Construct an equivalent network with dead units removed.
     - units in "NeverActive" (max(a.x+b) <= 0 on dom):   always ReLU = 0,
       drop them entirely (they add nothing to f).
     - units in "AlwaysActive" (min(a.x+b) >= 0 on dom):  ReLU is just
       the affine pre-activation, so we can FOLD it into the bias d by
       adding c_j (a_j . x + b_j) directly to the output linear layer.
       Concretely, after removing the unit, we add  c_j a_j  to a new
       "constant slope" vector and  c_j b_j  to d.  Since the rest of
       the output is c.h + d, we need to attach an explicit affine head:

           f(x) = c_kept . h_kept(x) + (c_always . a_always.x + c_always . b_always) + d.

       For simplicity we represent this as an "affine head" in the
       returned association.                                            *)

MPPruneDeadUnits[net_Association,
    dom_ : {{-3., 3.}, {-3., 3.}}] := Module[
  {a, b, c, d, dead, drop, keep, alwaysOn, neverOn, newA, newB, newC,
   newD, headSlope, headBias},
  {a, b, c, d} = Lookup[net, {"A", "b", "c", "d"}];
  dead   = MPDeadUnits[net, dom];
  neverOn  = dead["NeverActive"];
  alwaysOn = dead["AlwaysActive"];
  drop = Union[neverOn, alwaysOn];
  keep = Complement[Range[Length[b]], drop];

  newA = a[[keep]];
  newB = b[[keep]];
  newC = c[[keep]];

  (* Roll the always-active units into a constant affine head. *)
  headSlope = If[alwaysOn === {}, ConstantArray[0., Length[a[[1]]]],
    Total[c[[#]] a[[#]] & /@ alwaysOn]];
  headBias  = If[alwaysOn === {}, 0.,
    Total[c[[#]] b[[#]] & /@ alwaysOn]];
  newD = d + headBias;

  <|"A" -> newA, "b" -> newB, "c" -> newC, "d" -> newD,
    "AffineHead" -> <|"Slope" -> headSlope, "Bias" -> 0.|>,
    "Removed" -> <|"NeverActive" -> neverOn, "AlwaysActive" -> alwaysOn|>,
    "OriginalUnits" -> Length[b],
    "KeptUnits" -> Length[keep]
  |>
];

End[];     (* `Private` *)

EndPackage[];
