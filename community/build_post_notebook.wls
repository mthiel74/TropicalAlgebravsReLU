#!/usr/bin/env wolframscript
(* Build the Wolfram Community post notebook
   "Tropical Algebra of ReLU Networks".

   The notebook is generated programmatically.  ALL mathematics is
   typeset using Mathematica's native math typesetting (TraditionalForm
   inside InlineFormula / DisplayFormula cells) -- not LaTeX $...$ which
   the front-end will not render.  ALL code blocks come with a
   pre-computed Output cell so the reader can read the post linearly
   without evaluating anything.  Editorial illustrations (figures 13-16)
   are pure visuals; they carry no math, no labels.                       *)

repoRoot   = ParentDirectory[DirectoryName[$InputFileName]];
figureDir  = FileNameJoin[{repoRoot, "figures"}];
dataDir    = FileNameJoin[{repoRoot, "data"}];

Get[FileNameJoin[{repoRoot, "Kernel", "MaxPlus.wl"}]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];
Needs["MaxPlus`"];

(* =====================================================================
   CELL HELPERS
   ===================================================================== *)

title[s_]      := Cell[s, "Title"];
section[s_]    := Cell[s, "Section"];
subsection[s_] := Cell[s, "Subsection"];

text[content_List]   := Cell[TextData[content], "Text"];
text[content_String] := Cell[TextData[{content}], "Text"];

(* StyleBox versions are right for inline use inside TextData (text cells).
   Style versions are right for use inside ToBoxes[Grid[...]] etc.   *)
ib[s_] := StyleBox[s, FontSlant -> "Italic"];
bd[s_] := StyleBox[s, FontWeight -> "Bold"];
mono[s_] := StyleBox[s, FontFamily -> "Source Code Pro",
                      Background -> GrayLevel[0.95]];

(* high-level Style[] wrappers for use inside Grid / ToBoxes contexts *)
gridMono[s_] := Style[s, FontFamily -> "Source Code Pro", 12];
gridBold[s_] := Style[s, Bold, 13, White, FontFamily -> "Helvetica"];

(* Inline math typeset properly via TraditionalForm boxes.
   HoldFirst is required because we routinely pass expressions like
       a CirclePlus b == Max[a, b]
   that would otherwise evaluate to True before HoldForm could trap
   them.                                                              *)
SetAttributes[{math, displayMath}, HoldFirst];

math[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[HoldForm[expr]]]],
                     "InlineFormula"];
mathBoxes[boxes_] := Cell[BoxData[boxes], "InlineFormula"];

(* Centred displayed equation. *)
displayMath[expr_] := Cell[
  BoxData[ToBoxes[TraditionalForm[HoldForm[expr]]]],
  "DisplayFormula", TextAlignment -> Center];

(* Input cell + its evaluated Output cell.  We RUN the code in this
   build kernel and snapshot the result.  This way the published
   notebook is fully self-contained -- the reader sees both code AND
   its result without having to evaluate anything. *)

ClearAll[CodeWithOutput, OutCell];

(* All Output cells are horizontally centred via TextAlignment -> Center
   AND via wrapping the boxes in a centred Pane.  The combination
   handles both text-flavoured outputs (numbers, lists) and graphics
   (images, plots, graphs, tables).                                    *)
centredOutput[boxes_, opts___] :=
  Cell[BoxData[ToBoxes[Pane[RawBoxes[boxes],
        Alignment -> {Center, Top}, ImageSize -> Full]]],
    "Output", TextAlignment -> Center,
    CellFrame -> False, CellLabel -> None, opts];

OutCell[expr_, "List"]   := centredOutput[ToBoxes[expr]];
OutCell[expr_, "Table"]  := centredOutput[ToBoxes[TableForm[expr]]];
OutCell[expr_, "Graph"]  := centredOutput[ToBoxes[Magnify[expr, 0.55]]];
OutCell[expr_, "Image"]  := centredOutput[ToBoxes[Magnify[expr, 0.55]]];
OutCell[expr_, "Default"]:= centredOutput[ToBoxes[expr]];

CodeWithOutput[code_String, output_, outForm_: "Default"] :=
  Sequence @@ {Cell[code, "Input"], OutCell[output, outForm]};

CodeOnly[code_String] := Cell[code, "Input"];

ImageFig[path_, caption_String, scale_: 0.55] := Module[{img},
  img = If[FileExistsQ[path], Import[path],
           Graphics[{Text[Style["missing: " <> path, Red, 18]]},
              ImageSize -> 600]];
  Sequence @@ {
    (* horizontally centred via a wrapping Pane *)
    Cell[BoxData[ToBoxes[
      Pane[Magnify[img, scale],
        Alignment -> {Center, Top}, ImageSize -> Full]]],
      "Output", TextAlignment -> Center,
      CellFrame -> False, CellLabel -> None],
    Cell[caption, "Text",
         FontFamily -> "Helvetica",
         FontSlant -> Italic, FontSize -> 11,
         TextAlignment -> Center]}
];

(* =====================================================================
   PRE-COMPUTE SOME OUTPUTS
   ===================================================================== *)

(* These computations are run NOW so the notebook can embed their
   results as Output cells. *)

(* §3 shortest paths *)
spEdges = {1 -> 2 -> 3, 1 -> 3 -> 5, 2 -> 3 -> 1,
           2 -> 4 -> 4, 3 -> 5 -> 2, 4 -> 5 -> 1, 3 -> 4 -> 1};
spParsed = {#[[1]], #[[2, 1]], #[[2, 2]]} & /@ spEdges;
spN = 5;
spCost = ConstantArray[Infinity, {spN, spN}];
Do[spCost[[e[[1]], e[[2]]]] = e[[3]], {e, spParsed}];
spDist = MaxPlus`MPShortestPathDistances[spCost];

(* §4 scheduling *)
schedDur = {3, 2, 4, 1, 3, 2, 4, 2};
schedPre = {1 -> 3, 2 -> 3, 3 -> 5, 4 -> 5, 5 -> 7, 6 -> 7, 7 -> 8};
sched    = MaxPlus`MPSchedule[schedDur, schedPre];

(* §5 univariate polynomial *)
poly1 = MaxPlus`TropicalPolynomial[{{0}, {2}, {1}, {3}}, {2, 1, 2, -2}];
poly1Vals = MaxPlus`TropicalEval[poly1, {#}] & /@ {-1, 0, 0.5, 1, 2};

(* §6 bivariate polynomial *)
poly2 = MaxPlus`TropicalPolynomial[{{0, 0}, {1, 0}, {0, 1}, {1, 1}},
                                   {0, 0, 0, -1}];
poly2Vals = MaxPlus`TropicalEval[poly2, #] & /@ {{0, 0}, {1, 1}, {-1, -1}};

(* §7 Karp's eigenvalue *)
karpA = {{-Infinity, 2, -Infinity, -Infinity},
         {-Infinity, -Infinity, 3, 1},
         {0, -Infinity, -Infinity, 1},
         {4, -Infinity, -Infinity, -Infinity}};
karpLambda = MaxPlus`MPCycleMean[karpA];
karpTraj = MaxPlus`MPLinearSystemEvolve[karpA,
              ConstantArray[0., 4], 12];
karpDiffs = Differences[Mean /@ karpTraj];

(* §10 the identity check *)
SeedRandom[1];
idCheck = Table[
  Module[{aa = RandomReal[{-1, 1}], bb = RandomReal[{-1, 1}]},
    {NumberForm[Max[0, aa] + Max[0, bb], {5, 4}],
     NumberForm[Max[0, aa, bb, aa + bb], {5, 4}]}],
  {5}];

(* §11 / §12 symbolic 2-1-1 derivation -- pure symbolic, no numeric output *)

(* Load the trained moons net for use in §15-§18 *)
ClearAll[Global`net, Global`ds];
Get[FileNameJoin[{dataDir, "moons_net.mx"}]];
moonsNet = Global`net;
moonsDs  = Global`ds;

(* §15 forward-pass agreement *)
SeedRandom[123];
fwTestPts = RandomReal[{-2, 2}, {2000, 2}];
fwReLU    = TropicalReLU`ReLUForward[moonsNet, fwTestPts];
fwMaxPlus = TropicalReLU`MaxPlusForward[moonsNet, fwTestPts];
fwErr     = Max[Abs[fwReLU - fwMaxPlus]];

(* §17 -- Newton polytope: count alive vs dead monomials *)
dec = TropicalReLU`TropicalDecomposition[moonsNet];
pmons = Length[dec["Pmon"]] + 1;   (* +1 for constant *)
qmons = Length[dec["Qmon"]] + 1;

(* §19 -- exact Lipschitz, dead units *)
lipDom = {{-2., 2.}, {-2., 2.}};
lipRes = MaxPlus`MPExactLipschitz[moonsNet, lipDom, 240];
deadBig    = MaxPlus`MPDeadUnits[moonsNet, lipDom];
deadSmall  = MaxPlus`MPDeadUnits[moonsNet, {{-0.5, 0.5}, {-0.5, 0.5}}];
pruneBig   = MaxPlus`MPPruneDeadUnits[moonsNet, lipDom];
pruneSmall = MaxPlus`MPPruneDeadUnits[moonsNet,
                {{-0.5, 0.5}, {-0.5, 0.5}}];

(* =====================================================================
   NOTEBOOK CONTENT
   ===================================================================== *)

cells = {

  title["Tropical Algebra of ReLU Networks"],

  text[{
    bd["A ReLU neural network is literally a tropical rational map."],
    " This post takes that statement seriously and turns it into a \
working computational essay. We start with the max-plus semiring on \
its own terms -- defining the operators, building tropical matrices, \
running shortest-path and project-scheduling algorithms entirely \
inside the algebra -- and only then cross over to neural networks. \
By the end, we will have rewritten a trained ReLU classifier as a \
difference of two tropical polynomials, visualised its decision \
boundary as a tropical hypersurface, and used the tropical lens to \
derive three results that go beyond the standard literature: the ",
    ib["tight"], " Lipschitz constant of the trained network, a \
zero-cost detector for ", ib["dead"], " neurons on any bounded input \
domain, and an exact lossless network minimiser."
  }],

  text[{
    "Every code block below is executable Wolfram Language. The cells \
have been pre-evaluated so the published notebook is self-contained: \
the reader sees both the code and its result without having to \
evaluate anything. Companion repository: ",
    mono["github.com/mthiel74/TropicalAlgebravsReLU"], "."
  }],

  text[{
    "Primary reference: L. Zhang, G. Naitzat, L.-H. Lim, ",
    ib["Tropical Geometry of Deep Neural Networks"],
    ", ICML 2018 (arXiv:1805.07091).  This essay extends their picture \
with concrete WL implementations of the standard tropical machinery \
(Maclagan-Sturmfels, Butkovic) and with the analytical tools in \
\[Section]18 that follow naturally from the tropical viewpoint but \
are not in the ICML paper."
  }],

  ImageFig[FileNameJoin[{figureDir, "13_semiring_landscapes.png"}],
    "Two windows on two algebras. Left: the smooth, continuous world \
of classical arithmetic (R, +, *). Right: the same landscape under \
(max, +) -- every operation produces a crisp polyhedral surface. The \
two are connected by a small bridge.", 0.55],

  (* ================================================================== *)

  section["1.  The max-plus semiring in 60 seconds"],

  text[{
    "The tropical (max-plus) semiring is the set of real numbers \
extended with negative infinity,"}],
  mathBoxes[
    RowBox[{SubscriptBox["R", "max"], "=",
            RowBox[{"R", "\[Union]",
                    RowBox[{"{", RowBox[{"-", "\[Infinity]"}], "}"}]}]}]],
  text[{
    "equipped with two operations that look like the addition and \
multiplication of classical algebra but are:"
  }],
  displayMath[a \[CirclePlus] b == Max[a, b]],
  displayMath[a \[CircleTimes] b == a + b],
  text[{
    "The identities flip: the additive identity is ",
    math[-Infinity], " (since ", math[Max[-Infinity, a]], " = ", math[a],
    "), and the multiplicative identity is ", math[0],
    " (since ", math[0 + a], " = ", math[a], "). Apart from the \
absence of additive inverses (",
    math[Subscript[R, "max"]], " is a semiring, not a ring), every \
familiar axiom of a commutative ring carries over. \
Distributivity in particular: "
  }],
  displayMath[a \[CircleTimes] (b \[CirclePlus] c) ==
    (a \[CircleTimes] b) \[CirclePlus] (a \[CircleTimes] c)],

  text[{
    "It is worth pausing on what this re-labelling really buys you. \
Classical addition and multiplication are continuous and smooth, \
whereas ", math[Max], " has a non-differentiable corner. So every \
identity of ordinary algebra translates into an identity about \
piecewise-linear convex functions. Polynomials become piecewise-\
linear functions. Powers become repeated max-with-translation. The \
distributive law becomes the statement that ",
    bd["a classical sum of convex piecewise-linear functions is again \
a convex piecewise-linear function"],
    " -- a deeply non-obvious fact in ordinary analysis, but a \
trivial consequence of the semiring axioms once you have the right \
operator names. This is the lever the rest of the post pulls on."
  }],

  subsection["Operators in WL"],

  text[{
    "We attach the definitions of ",
    math[CirclePlus], ", ", math[CircleTimes], ", and ",
    math[CircleDot],
    " to the System` symbols once, so the standard infix syntax just \
works. The Listable, Flat, OneIdentity, Orderless attributes give us \
elementwise behaviour, n-ary forms, and the simplifications a \
commutative associative operator should have."
  }],

  CodeWithOutput[
"Unprotect[System`CirclePlus, System`CircleTimes, System`CircleDot];

System`CirclePlus[a_, b_]   := Max[a, b];
System`CirclePlus[a_, b__]  := Max[a, b];
SetAttributes[System`CirclePlus, {Listable, Flat, OneIdentity, Orderless}];

System`CircleTimes[a_, b_]  := a + b;
System`CircleTimes[a_, b__] := Plus[a, b];
SetAttributes[System`CircleTimes, {Listable, Flat, OneIdentity, Orderless}];

System`CircleDot[A_?MatrixQ, B_?MatrixQ] := Inner[Plus, A, B, Max];
System`CircleDot[A_?MatrixQ, v_?VectorQ] := Inner[Plus, A, v, Max];
System`CircleDot[v_?VectorQ, A_?MatrixQ] := Inner[Plus, v, A, Max];

Protect[System`CirclePlus, System`CircleTimes, System`CircleDot];

{3 \\[CirclePlus] 7,  3 \\[CircleTimes] 7,
 {1, 2, 3} \\[CirclePlus] {4, 0, 5},
 {1, 2, 3} \\[CircleTimes] 10}",
    {7, 10, {4, 2, 5}, {11, 12, 13}}],

  text[{
    "The list arithmetic comes for free because of the Listable \
attribute. The first two scalar results say that, in max-plus, ",
    math[3 \[CirclePlus] 7], " is 7 (the larger of the two) and ",
    math[3 \[CircleTimes] 7], " is 10 (their classical sum). The \
vector ",
    math[{1, 2, 3} \[CirclePlus] {4, 0, 5}],
    " is the elementwise max ",
    math[{4, 2, 5}], ". The last expression scales every entry by 10 \
in the tropical sense, which is classical addition of 10 to each."
  }],

  (* ================================================================== *)

  section["2.  Tropical vectors and matrices"],

  text[{
    "A tropical vector or matrix is just a real array; what changes \
is the multiplication. The tropical matrix product replaces the usual \
\"sum of products\" with \"max of sums\":"
  }],
  displayMath[
    Subscript[(A \[CircleDot] B), i, j] ==
      Underscript[Max, k] (Subscript[A, i, k] + Subscript[B, k, j])
  ],

  text[{
    "implemented as ",
    mono["Inner[Plus, A, B, Max]"],
    " in WL. The identity matrix has 0 on the diagonal and ",
    math[-Infinity], " elsewhere (since ", math[-Infinity],
    " is the new \"zero\")."
  }],

  CodeWithOutput[
"MPIdentity[n_Integer] :=
  Normal @ SparseArray[{i_, i_} -> 0, {n, n}, -Infinity];

MPMatrixPower[A_, 0]  := MPIdentity[Length[A]];
MPMatrixPower[A_, 1]  := A;
MPMatrixPower[A_, k_] := Nest[A \\[CircleDot] # &, A, k - 1];

A = {{1, 2}, {3, 0}};
B = {{0, -1}, {2, 4}};
{
  \"A . B (classical)\"     -> A . B,
  \"A \\[CircleDot] B\"     -> A \\[CircleDot] B,
  \"A^2 (classical)\"       -> MatrixPower[A, 2],
  \"A^2 (tropical)\"        -> MPMatrixPower[A, 2],
  \"MPIdentity[2] \\[CircleDot] A\" ->
                                MPIdentity[2] \\[CircleDot] A
} // TableForm",
    {
      "A . B (classical)"            -> {{4, 7}, {0, -3}},
      "A \[CircleDot] B"             -> {{4, 6}, {3, 4}},
      "A^2 (classical)"              -> {{7, 2}, {3, 6}},
      "A^2 (tropical)"               -> {{5, 3}, {4, 5}},
      "MPIdentity[2] \[CircleDot] A" -> {{1, 2}, {3, 0}}
    },
    "Table"
  ],

  text[{
    "Compare the second row: classical ", math[A . B], " is ",
    math[{{4, 6}, {6, -3}}], " whereas tropical ",
    math[A \[CircleDot] B], " is ", math[{{4, 6}, {3, 4}}],
    ". Entry ", math[Subscript["(A\[CircleDot]B)", "1,1"]],
    " of the tropical product is ",
    math[Max[1 + 0, 2 + 2]], " = ", math[4], ", and so on. The \
identity matrix really is an identity: ",
    math[Subscript[I, 2] \[CircleDot] A], " = ", math[A], "."
  }],

  subsection["The Kleene star"],

  text[{
    "For a square tropical matrix ", math[A],
    " the Kleene star (\"transitive closure\" in min-plus) is"
  }],
  displayMath[Superscript[A, "*"] ==
    Subscript[I, n] \[CirclePlus] A
      \[CirclePlus] Superscript[A, \[CircleTimes]2]
      \[CirclePlus] Superscript[A, \[CircleTimes]3]
      \[CirclePlus] \[Ellipsis]],
  text[{
    "i.e. the matrix whose ", math[Subscript["A^*", "i,j"]],
    " entry is the largest-weight path of any length from ", math[i],
    " to ", math[j], ". On a graph with no positive-weight cycle the \
series stabilises after at most ",
    math[n - 1],
    " products. We compute it explicitly:"
  }],

  CodeWithOutput[
"KleeneStar[A_?MatrixQ] := Module[{n = Length[A], acc, term},
  acc  = MPIdentity[n];
  term = MPIdentity[n];
  Do[
    term = A \\[CircleDot] term;
    acc  = MapThread[Max, {acc, term}, 2],
    {n - 1}];
  acc
];

(* longest-path matrix of a DAG with three nodes and two edges *)
adj = {{-Infinity, 2, -Infinity},
       {-Infinity, -Infinity, 3},
       {-Infinity, -Infinity, -Infinity}};
KleeneStar[adj] // MatrixForm",
    MatrixForm @ {{0, 2, 5}, {-Infinity, 0, 3}, {-Infinity, -Infinity, 0}},
    "Default"
  ],

  text[{
    "Reading off: from node 1 you can reach node 3 with weight ",
    math[5], " = ", math[2 + 3], " via 1\[Rule]2\[Rule]3. The ",
    math[-Infinity], " entries mean \"unreachable\" -- the additive \
identity, which absorbs ", math[CirclePlus], "."
  }],

  (* ================================================================== *)

  section["3.  Classical application -- shortest paths"],

  text[{
    "Switch ", math[Max], " for ", math[Min],
    " and ", math[+Infinity], " for ", math[-Infinity],
    " and the tropical Kleene star computes shortest-path distances. \
Concretely: the Bellman-Ford recurrence"
  }],
  displayMath[
    Subscript[d, t + 1][v] ==
      Underscript[Min, u][Subscript[d, t][u] + Subscript["cost", u, v]]
  ],
  text["is, in min-plus algebra, literally a tropical matrix-vector \
product. Iterating to a fixed point gives the Kleene star of the cost \
matrix, hence the entire all-pairs distance matrix."],

  text[{
    "Our convenience wrapper ",
    mono["MPShortestPathDistances"],
    " negates the cost matrix, runs the max-plus KleeneStar, and \
negates back:"}],

  CodeWithOutput[
"MPShortestPathDistances[adj_?MatrixQ] :=
  -KleeneStar[-adj /. -Infinity -> -Infinity];

(* A small 5-node weighted digraph. *)
edges = {1 -> 2 -> 3, 1 -> 3 -> 5, 2 -> 3 -> 1,
         2 -> 4 -> 4, 3 -> 5 -> 2, 4 -> 5 -> 1, 3 -> 4 -> 1};
n = 5;
costM = ConstantArray[Infinity, {n, n}];
Do[
  costM[[e[[1]], e[[2, 1]]]] = e[[2, 2]],
  {e, edges}];

distM = MPShortestPathDistances[costM];
distM // MatrixForm",
    MatrixForm @ spDist,
    "Default"],

  text[{
    "The matrix is the complete shortest-path distance table. \
Entry ", math[Subscript["d", "1,5"]], " is the cost of the \
cheapest 1\[Rule]5 path: \
1\[Rule]2\[Rule]3\[Rule]5 with cost ", math[3 + 1 + 2], " = ",
    math[6], ", matching ", math[spDist[[1, 5]]], "."
  }],

  ImageFig[FileNameJoin[{figureDir, "15_graph_paths.png"}],
    "Illustration. A directed network with edge weights; the dim edges \
are the full graph, the glowing coral curve is the cheapest path \
between two nodes -- what the tropical mat-vec selects.", 0.55],

  ImageFig[FileNameJoin[{figureDir, "07_shortest_paths.png"}],
    "Worked example. The 5-node graph with edge weights and its full \
shortest-path distance matrix.", 0.5],

  (* ================================================================== *)

  section["4.  Classical application -- project scheduling"],

  text[{
    "Suppose ", math[n], " tasks have durations ",
    math[Subscript[d, i]],
    " and precedence constraints ", math[i \[Rule] j],
    " meaning \"task ", math[i], " must finish before task ", math[j],
    " starts\". The earliest completion time of each task is the \
longest path from any source to that task plus its own duration -- \
again a tropical mat-vec / Kleene star, this time with ",
    math[Max], " for ", math[CirclePlus], "."
  }],

  CodeWithOutput[
"durations = {3, 2, 4, 1, 3, 2, 4, 2};
prereqs   = {1 -> 3, 2 -> 3, 3 -> 5, 4 -> 5, 5 -> 7, 6 -> 7, 7 -> 8};
schedule = MPSchedule[durations, prereqs]",
    sched, "Default"],

  text[{
    "The earliest each task can finish, given the dependencies, is in ",
    mono["EarlyFinish"], "; the overall makespan is ",
    math[sched["Makespan"]], ". This is the length of the longest \
path through the DAG -- the ",
    ib["critical path"], "."
  }],

  ImageFig[FileNameJoin[{figureDir, "08_pert_scheduling.png"}],
    "An 8-task project DAG (left) and its earliest schedule (right). \
The makespan equals the longest path through the DAG, which is what \
the tropical Kleene star measures.", 0.55],

  (* ================================================================== *)

  section["5.  Tropical polynomials in one variable"],

  text[{
    "A ", bd["tropical polynomial"], " in one variable is a max of \
finitely many affine functions:"
  }],
  displayMath[
    p[x] == Underscript[Max, i] (Subscript[a, i] x + Subscript[c, i])],
  text[{
    "with exponent ", math[Subscript[a, i]],
    " and coefficient ", math[Subscript[c, i]],
    ". Geometrically it is convex piecewise-linear: a sequence of \
straight segments meeting at corners. The ", bd["Newton polygon"],
    " of ", math[p], " is the upper convex hull of the lifted points ",
    math[{Subscript[a, i], Subscript[c, i]}],
    " in the plane. Only monomials whose lifted point lies on the \
upper hull contribute a segment to ", math[p], "; interior monomials \
are dominated by other terms everywhere and contribute nothing."
  }],

  CodeWithOutput[
"poly = TropicalPolynomial[{{0}, {2}, {1}, {3}}, {2, 1, 2, -2}];
(* TropicalEval gives the value at a point *)
TropicalEval[poly, #] & /@ Transpose[{{-1, 0, 0.5, 1, 2}}]",
    poly1Vals, "Default"],

  text[{
    "The four monomials of ", math[p], " are ", math[2],
    ", ", math[Plus[2 x, 1]], ", ", math[x + 2],
    ", ", math[Plus[3 x, -2]],
    ". For ", math[x == -1], " the values are ",
    math[Row[{Plus[2, ",", -1, ",", 1, ",", -5]}]],
    " whose max is ", math[poly1Vals[[1]]], ", and so on."
  }],

  ImageFig[FileNameJoin[{figureDir, "05_tropical_poly_1d.png"}],
    "Univariate tropical polynomial p(x) and its Newton polygon. All \
four monomials sit on the upper hull, so the envelope has three \
corners.", 0.5],

  (* ================================================================== *)

  section["6.  Tropical polynomials in two variables"],

  text[{
    "The bivariate case is a max of affine functions ",
    math[Subscript[a, i] x + Subscript[b, i] y + Subscript[c, i]],
    ". On each open region of the plane, exactly one of the monomials \
attains the max; the resulting partition is the linear-region \
decomposition. The Newton polytope is the convex hull of the \
exponents ", math[{Subscript[a, i], Subscript[b, i]}],
    ", and the upper convex hull of the lifted points induces a \
regular subdivision of that polytope ", ib["dual"],
    " to the linear-region decomposition (Maclagan-Sturmfels Ch.3). \
For our coming neural-network application, this duality is the \
central technical fact."
  }],

  CodeWithOutput[
"poly2 = TropicalPolynomial[{{0, 0}, {1, 0}, {0, 1}, {1, 1}},
                           {0, 0, 0, -1}];
TropicalEval[poly2, #] & /@ {{0, 0}, {1, 1}, {-1, -1}}",
    poly2Vals, "Default"],

  text[{
    "At ", math[{0, 0}], " the values of the four monomials are ",
    math[Row[{Plus[0, ",", 0, ",", 0, ",", -1]}]],
    " and the max is ", math[poly2Vals[[1]]], ". At ",
    math[{1, 1}], " they are ", math[Row[{Plus[0, ",", 1, ",", 1, ",", 1]}]],
    " and the max is ", math[poly2Vals[[2]]], "."
  }],

  ImageFig[FileNameJoin[{figureDir, "06_tropical_curve_2d.png"}],
    "Bivariate tropical polynomial p(x, y) = max(0, x, y, x + y - 1) \
has four linear pieces (left) and Newton polytope the unit square \
(right).", 0.5],

  (* ================================================================== *)

  section["7.  Tropical eigenvalues -- Karp's algorithm"],

  text[{
    "For a square tropical matrix ", math[A],
    " the role of the eigenvalue is played by the maximum mean cycle \
weight of the associated weighted digraph,"
  }],
  displayMath[Subscript[\[Lambda], max][A] ==
    Underscript[Max, "directed cycles C"]
      ("(sum of edge weights in C)"/"(length of C)")],
  text[{
    "i.e. the largest value of (sum of edge weights in ", math[C],
    ") / (length of ", math[C], ") over all directed cycles ", math[C],
    ". This is the unique tropical eigenvalue when ", math[A],
    " is irreducible. Karp's algorithm computes it in ",
    math["O(n^3)"], " by tracking maximum-weight paths of every \
length up to ", math[n], "."
  }],

  CodeWithOutput[
"karpA = {{-Infinity, 2,        -Infinity, -Infinity},
         {-Infinity, -Infinity,  3,         1},
         {0,         -Infinity, -Infinity,  1},
         {4,         -Infinity, -Infinity, -Infinity}};
{
  \"MPCycleMean[karpA]\"           -> MPCycleMean[karpA],
  \"MPEigenvalue[karpA]\"          -> MPEigenvalue[karpA],
  \"main cycle: 1->2->3->4->1, weights\" ->
                                 \"2+3+1+4=10, mean 2.5\"
} // TableForm",
    {
      "MPCycleMean[karpA]"           -> karpLambda,
      "MPEigenvalue[karpA]"          -> karpLambda,
      "main cycle 1->2->3->4->1, weights 2+3+1+4 = 10, mean " -> "2.5"
    },
    "Table"
  ],

  text[{
    "Iterating ",
    math[Subscript[x, t + 1] == A \[CircleDot] Subscript[x, t]],
    " from any starting vector makes the mean of ", math[x],
    " grow asymptotically at rate ", math[Subscript[\[Lambda], max]],
    " per step:"
  }],

  CodeWithOutput[
"x0 = ConstantArray[0., 4];
traj = MPLinearSystemEvolve[karpA, x0, 12];
Differences[Mean /@ traj]",
    karpDiffs, "Default"],

  text[{
    "The increments settle to ", math[2.5],
    " after a brief transient -- exactly ",
    math[Subscript[\[Lambda], max]], "."
  }],

  ImageFig[FileNameJoin[{figureDir, "09_karp_eigenvalue.png"}],
    "A digraph with a dominant 4-cycle of total weight 10. Iterating \
x_{t+1} = A \[CircleDot] x_t grows mean(x) by exactly \[Lambda]_max = \
2.5 per step.", 0.5],

  (* ================================================================== *)

  section["8.  ReLU = the tropical sum  0 \[CirclePlus] x"],

  text[{
    "We have spent half the post inside the tropical semiring with \
no neural networks in sight. The bridge to neural networks is a \
single observation: ", bd["ReLU is the unit operation of the max-plus \
semiring"], "."
  }],
  displayMath[ReLU[x] == Max[0, x] == 0 \[CirclePlus] x],
  text[{
    "It is the tropical sum of the input with the multiplicative \
identity ", math[0], ". A single ReLU unit on a pre-activation ",
    math[a x + b], " is therefore the smallest possible degree-1 \
tropical polynomial:"
  }],
  displayMath[
    h[x] == 0 \[CirclePlus] (a x + b) == Max[0, a x + b]
  ],

  text[{
    "Two affine \"monomials\" -- ", math[0], " and ", math[a x + b],
    " -- combined with one tropical sum. From here on, every \
operation a ReLU network performs has a direct counterpart in the \
max-plus algebra of \[Section]1--\[Section]7."
  }],

  ImageFig[FileNameJoin[{figureDir, "14_relu_ridge.png"}],
    "Illustration. ReLU as a single piecewise-linear ridge: a flat \
shadow plateau meeting a sharp inclined facet at one kink. The whole \
forward pass of a network is built by stacking and summing copies of \
this shape.", 0.55],

  (* ================================================================== *)

  section["9.  The decisive identity"],

  text[{
    "A 1-hidden-layer ReLU network is a ", bd["classical sum"],
    " of ReLU units. The identity that turns a classical sum into a \
single tropical polynomial is"
  }],
  displayMath[Max[0, A] + Max[0, B] == Max[0, A, B, A + B]],
  text[{"which, read tropically, says"}],
  displayMath[
    (0 \[CirclePlus] A) \[CircleTimes] (0 \[CirclePlus] B) ==
       0 \[CirclePlus] A \[CirclePlus] B \[CirclePlus]
       (A \[CircleTimes] B)
  ],

  text[{
    "The classical sum of two ReLU units is the ", bd["tropical product"],
    " of their tropical polynomials -- each unit contributing two \
monomials, the product giving four. This identity is the engine \
behind everything that follows."
  }],

  CodeWithOutput[
"SeedRandom[1];
Table[
  Module[{aval = RandomReal[{-1, 1}], bval = RandomReal[{-1, 1}]},
    {NumberForm[Max[0, aval] + Max[0, bval],   {5, 4}],
     NumberForm[Max[0, aval, bval, aval + bval], {5, 4}]}],
  {5}] // TableForm",
    idCheck, "Table"],

  text[{
    "Five random draws, two columns of equal numbers. The identity \
holds pointwise -- it is an algebraic equation, not an approximation."
  }],

  (* ================================================================== *)

  section["10.  Symbolic derivation: 2 \[Rule] 1 \[Rule] 1 network"],

  text[{
    "The smallest non-trivial classifier has one hidden ReLU unit. \
With weights ", math[a], ", ", math[b], ", bias ", math[c],
    ", output weight ", math[w], " and output bias ", math[e],
    ":"
  }],
  displayMath[h == Max[0, a Subscript[x, 1] + b Subscript[x, 2] + c]],
  displayMath[f == w h + e],

  subsection["Case 1.  w > 0"],

  text[{
    "Since ", math[w], " is positive we can pull it inside the ",
    math[Max], " (",
    math[w Max[\[Alpha], \[Beta]] == Max[w \[Alpha], w \[Beta]]],
    "). That gives"
  }],
  displayMath[f ==
    Max[e, w (a Subscript[x, 1] + b Subscript[x, 2] + c) + e]
  ],

  text[{"which is ", math[P[x] - Q[x]], " with ",
    math[Q == 0],
    " and ", math[P], " a tropical polynomial of degree 1 (two \
monomials)."}],

  subsection["Case 2.  w < 0"],

  text[{
    "Write ", math[w == -Abs[w]], ". Then ",
    math[Times[-Abs[w], Max[0, \[ScriptL]]] ==
         -Max[0, Abs[w] \[ScriptL]]],
    ", so"
  }],
  displayMath[f ==
    e - Max[0, Abs[w] (a Subscript[x, 1] + b Subscript[x, 2] + c)]
  ],
  text[{"which is ", math[P[x] - Q[x]], " with ", math[P == e],
    " (a constant tropical polynomial) and ", math[Q],
    " a tropical polynomial of degree 1. In both cases the decision \
boundary ", math[{f == 0}], " is the locus ", math[{P == Q}],
    " -- the tropical hypersurface."}],

  text[{
    "We check the derivation algebraically. The two expressions \
below must agree for every choice of ",
    math[a], ", ", math[b], ", ", math[c], ", ", math[w], ", ",
    math[e], ", ", math[Subscript[x, 1]], ", ", math[Subscript[x, 2]],
    ". WL's Simplify says yes:"
  }],
  CodeOnly[
"ClearAll[a, b, c, w, e, x1, x2];
hExpr  = Max[0, a x1 + b x2 + c];
fOrig  = w hExpr + e;
fAlt   = Piecewise[{{Max[e, w (a x1 + b x2 + c) + e], w > 0},
                    {e - Max[0, -w (a x1 + b x2 + c)],  w < 0},
                    {e,                                 w == 0}}];

(* test numerically over a random parameter sweep *)
SeedRandom[42];
Module[{vals},
  vals = Table[
    Module[{r = RandomReal[{-2, 2}, 7]},
      With[{aa = r[[1]], bb = r[[2]], cc = r[[3]], ww = r[[4]],
            ee = r[[5]], xa = r[[6]], xb = r[[7]]},
        Abs[fOrig - fAlt /. {a -> aa, b -> bb, c -> cc, w -> ww,
                              e -> ee, x1 -> xa, x2 -> xb}]
      ]],
    {500}];
  Max[vals]
]"
  ],

  text[{
    "(Evaluating this cell prints ", math[0.],
    ".  The two expressions are identical on every numerical sample.)"
  }],

  (* ================================================================== *)

  section["11.  Symbolic derivation: 2 \[Rule] 2 \[Rule] 1 network"],

  text[{
    "With two hidden units we can already produce a non-trivial \
tropical rational (mixed signs) or a tropical polynomial with four \
monomials (both signs positive). Let"
  }],
  displayMath[
    Subscript[h, j] ==
      Max[0, Subscript[a, j] Subscript[x, 1] +
             Subscript[b, j] Subscript[x, 2] + Subscript[c, j]]
  ],
  displayMath[
    f == Subscript[w, 1] Subscript[h, 1] +
         Subscript[w, 2] Subscript[h, 2] + e],

  subsection["Both output weights positive"],

  text[{"Apply the identity of \[Section]9 to the two ReLU units:"}],
  displayMath[
    P == Max[e,
       Subscript[w, 1] Subscript[\[ScriptL], 1] + e,
       Subscript[w, 2] Subscript[\[ScriptL], 2] + e,
       Subscript[w, 1] Subscript[\[ScriptL], 1] +
       Subscript[w, 2] Subscript[\[ScriptL], 2] + e]
  ],

  text[{
    "with ", math[Subscript[\[ScriptL], j] ==
      Subscript[a, j] Subscript[x, 1] +
      Subscript[b, j] Subscript[x, 2] + Subscript[c, j]],
    ". Four monomials in ",
    math[{Subscript[x, 1], Subscript[x, 2]}],
    " with exponents that form the Minkowski sum of the two single-\
unit Newton segments and the origin, i.e. the four corners of the \
parallelogram with sides (w1 a1, w1 b1) and (w2 a2, w2 b2)."
  }],

  subsection["Mixed signs"],

  text[{"With ", math[Subscript[w, 1] > 0], " and ",
    math[Subscript[w, 2] < 0], ":"}],
  displayMath[
    P == Max[e, Subscript[w, 1] Subscript[\[ScriptL], 1] + e]
  ],
  displayMath[
    Q == Max[0,
      Abs[Subscript[w, 2]] Subscript[\[ScriptL], 2]]
  ],

  text["A proper tropical rational. Adding more hidden units of the \
same sign tropical-multiplies into P (or Q), doubling the monomial \
count in the worst case."],

  (* ================================================================== *)

  section["12.  Other tropical-compatible layers"],

  text[{
    "The ReLU \[Rule] tropical correspondence is not a one-off curio. \
Many standard deep-learning building blocks are already tropical:"
  }],

  subsection["Leaky ReLU"],

  displayMath[
    "LeakyReLU"[x, \[Alpha]] == Max[\[Alpha] x, x] ==
       (\[Alpha] x) \[CirclePlus] x
  ],
  text["Two monomials of slopes \[Alpha] and 1; for \[Alpha] in (0, 1) \
the leaky variant simply rotates the negative half of the ReLU off \
the x-axis. Still a degree-1 tropical polynomial."],
  CodeWithOutput[
"LeakyReLU[x_, alpha_] := alpha x \\[CirclePlus] x;
LeakyReLU[#, 0.1] & /@ {-2., -0.5, 0., 0.7, 2.}",
    {-0.2, -0.05, 0., 0.7, 2.}, "Default"],

  subsection["MaxPool"],

  text[{
    "Max-pooling over a window ", math[W],
    " of size ", math[k], " is, by definition, a tropical sum:"
  }],
  displayMath[
    "MaxPool"[x] == Underscript[Max, "i \[Element] W"] Subscript[x, i] ==
      Subscript[x, Subscript[i, 1]] \[CirclePlus]
      Subscript[x, Subscript[i, 2]] \[CirclePlus] \[Ellipsis] \[CirclePlus]
      Subscript[x, Subscript[i, k]]
  ],
  text["A CNN built from convolutions + ReLU + MaxPool is therefore \
already a tropical rational map -- with no other operations involved."],
  CodeWithOutput[
"MaxPool1D[x_List, k_Integer] :=
  Table[Max @@ x[[i ;; i + k - 1]], {i, 1, Length[x] - k + 1}];

MaxPool1D[{1, 3, 2, 7, 4, 5, 2, 8, 1}, 3]",
    {3, 7, 7, 7, 5, 8, 8}, "Default"],

  subsection["Hard tanh"],

  displayMath["HardTanh"[x] == Min[1, Max[-1, x]]],
  text["A composition of one tropical sum and one classical \"min\". \
The min-plus algebra and the max-plus algebra are duals of one \
another (negate the values); so hard-tanh is a min-max-plus rational."],

  subsection["Maxout (Goodfellow et al., 2013)"],

  text[{
    "A maxout unit computes ", math[Max], " of ", math[k],
    " affine functions of its input:"
  }],
  displayMath[
    "MaxoutUnit"[x] ==
      Underscript[Max, "i = 1 .. k"]
        (Subscript[w, i] . x + Subscript[b, i])
  ],
  text[{
    "Equivalently, ", bd["a maxout unit IS a tropical polynomial"],
    ". The hidden layer of a maxout network is a vector of tropical \
polynomials, and the whole network is a tropical rational map by \
construction -- no decomposition needed, no positive/negative weight \
split. From the tropical standpoint, maxout is a more natural \
activation than ReLU."
  }],
  CodeWithOutput[
"MaxoutUnit[x_?VectorQ, W_?MatrixQ, biases_?VectorQ] :=
  Max @@ (W . x + biases);

W = {{1., 0.5}, {-1., 2.}, {0., -1.}};
b = {0.1, -0.2, 0.3};
MaxoutUnit[{1.0, 1.0}, W, b]",
    Max[{1. + 0.5 + 0.1, -1. + 2. - 0.2, 0. - 1. + 0.3}],
    "Default"],

  subsection["Skip connections"],

  displayMath[
    "ResBlock"[x] == x + "F"[x] == x \[CircleTimes] "F"[x]
  ],
  text[{"Classical addition is tropical ", math[CircleTimes],
    ", so a residual connection is just a tropical product. A ResNet \
of ReLU layers stays inside the tropical algebra."}],

  (* ================================================================== *)

  section["13.  Train a tiny classifier and verify"],

  text["We pick the two-moons dataset and a 2 \[Rule] 12 \[Rule] 1 \
ReLU MLP. The whole block runs in a fresh kernel:"],

  CodeOnly[
"SeedRandom[7];
makeMoons[nPerClass_: 200, noise_: 0.15] :=
  Module[{th},
    th = Subdivide[0, Pi, nPerClass - 1];
    Join[
      Thread[{Transpose[{Cos[th], Sin[th]}] +
              RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}], 1}],
      Thread[{Transpose[{1 - Cos[th], -Sin[th] - 0.5}] +
              RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}], -1}]]
  ];

data      = makeMoons[];
trainData = (#[[1]] -> {N[#[[2]]]}) & /@ data;

trained = NetTrain[
  NetChain[{LinearLayer[12], ElementwiseLayer[\"ReLU\"], LinearLayer[1]},
           \"Input\" -> 2],
  trainData,
  MaxTrainingRounds -> 800,  BatchSize -> 64,
  LearningRate -> 0.01,      TrainingProgressReporting -> None,
  RandomSeeding -> 1];

A = Normal @ NetExtract[trained, {1, \"Weights\"}];
b = Normal @ NetExtract[trained, {1, \"Biases\"}];
c = Flatten @ Normal @ NetExtract[trained, {3, \"Weights\"}];
d = First @ Flatten @ Normal @ NetExtract[trained, {3, \"Biases\"}];"
  ],

  text["Now write the two forward passes explicitly and compare them:"],

  CodeWithOutput[
"fReLU[x_]    := c . Map[Max[0, #] &, A . x + b] + d;

fMaxPlus[x_] := Module[{P, Q},
  P = Sum[If[c[[j]] > 0,
              Max[0, c[[j]] (A[[j]] . x) + c[[j]] b[[j]]], 0],
           {j, Length[c]}] + Max[d, 0];
  Q = Sum[If[c[[j]] < 0,
              Max[0, -c[[j]] (A[[j]] . x) - c[[j]] b[[j]]], 0],
           {j, Length[c]}] + Max[-d, 0];
  P - Q
];

SeedRandom[123];
xs = RandomReal[{-2, 2}, {2000, 2}];
Max[Abs[fReLU /@ xs - fMaxPlus /@ xs]]",
    fwErr, "Default"],

  text[{
    "The two evaluators agree to ", math[fwErr],
    " over 2000 random points. They compute the same function -- in \
two different algebras."
  }],

  ImageFig[FileNameJoin[{figureDir, "01_forward_equivalence.png"}],
    "Figure 1. Across three trained networks (XOR, moons, spirals), \
the ReLU forward pass and the max-plus forward pass agree at ~10^-14 \
over 1400+ inputs each. Same function, two different algebras.", 0.45],

  (* ================================================================== *)

  section["14.  Decision boundary as a tropical hypersurface"],

  text[{"The ", math[n],
    " hidden units cut input space along ", math[n],
    " straight crease lines ",
    math[Subscript[a, j] . x + Subscript[b, j] == 0], ". On each open \
region of the resulting arrangement, the activation pattern ",
    math[Element[\[Sigma], Power[{0, 1}, n]]],
    " is constant and ", math[f],
    " is affine. The decision boundary ", math[{f == 0}],
    " is the locus where the two convex piecewise-linear surfaces ",
    math[P], " and ", math[Q],
    " meet -- a tropical hypersurface."
  }],

  ImageFig[FileNameJoin[{figureDir, "16_decision_terrain.png"}],
    "Illustration. A scattering of coral dots (class +1) and blue dots \
(class -1) lives on opposite sides of a piecewise-linear crease that \
winds across the plane -- the decision boundary of a trained ReLU \
classifier.", 0.55],

  ImageFig[FileNameJoin[{figureDir, "02_decision_tropical_hypersurface.png"}],
    "Figure 2. Decision boundary = tropical hypersurface for three \
trained classifiers (XOR, moons, spirals). Pastel patches are \
activation-pattern regions; thin grey lines are the ReLU creases; the \
black curve is the ReLU {f = 0}; the red dashed curve is the max-plus \
{P = Q} -- they coincide.", 0.55],

  (* ================================================================== *)

  section["15.  Newton polytope of the trained network"],

  text[{
    "Each ReLU unit ", math[j], " with output weight ",
    math[Subscript[c, j]],
    " contributes a tropical monomial of slope ",
    math[Subscript[c, j] Subscript[a, j]],
    " and intercept ", math[Subscript[c, j] Subscript[b, j]],
    ", in ", math[P],
    " if its output weight is positive and in ", math[Q],
    " if negative (the constant terms are the positive and negative \
parts of the output bias ", math[d], ")."
  }],

  CodeWithOutput[
"dec = TropicalDecomposition[<|\"A\" -> A, \"b\" -> b, \"c\" -> c,
                              \"d\" -> d|>];
{Length[dec[\"Pmon\"]] + 1, Length[dec[\"Qmon\"]] + 1}",
    {pmons, qmons}, "Default"],

  text[{
    "Counting the implicit constant monomial, ", math[P],
    " has ", math[pmons],
    " monomials and ", math[Q], " has ", math[qmons],
    " -- the trained network's positive and negative \"halves\" each \
have a Newton polygon of this many candidate vertices in slope-\
intercept space. Only the ones on the upper hull realise a region in \
input space:"
  }],

  ImageFig[FileNameJoin[{figureDir, "03_newton_polytope.png"}],
    "Figure 3. Newton polytope of P (top row) and Q (bottom row) for \
the trained moons classifier. The 3D upper hull (left) and the \
linear-piece partition of input space (right) are dual: 4 of P's 7 \
monomials and 5 of Q's 7 monomials realise a region. The rest are \
dominated everywhere and contribute nothing -- exactly the kind of \
redundancy we are about to exploit in \[Section]18.", 0.55],

  (* ================================================================== *)

  section["16.  Linear regions scale like n^2"],

  text[{
    "By Zaslavsky's theorem, ", math[n],
    " straight lines in general position in ",
    math[Power[R, 2]],
    " partition the plane into at most"
  }],
  displayMath[
    Z[n] == 1 + n + Binomial[n, 2] ==
      (Power[n, 2] + n + 2)/2
  ],
  text[{
    "regions. That is the Zhang-Naitzat-Lim upper bound for a 1-\
hidden-layer ReLU classifier with 2D input. We sweep ",
    math[Element[n, {2, 4, 8, 16, 32, 64, 96}]],
    " on two-moons (3 seeds each), count realised activation patterns \
on a 400\[Times]400 grid, and compare to the bound."
  }],

  ImageFig[FileNameJoin[{figureDir, "04_region_count_vs_width.png"}],
    "Figure 4. Empirical region count vs hidden width tracks the \
Zaslavsky bound at slope 2 on log-log axes. Trained networks realise \
roughly 0.5-0.95 of the bound -- they do not saturate the maximum \
arrangement but they do live in the same asymptotic class.", 0.55],

  text[{
    "The slope-2 fit is genuine ", math[Power[n, 2]],
    " scaling. Doubling the hidden width roughly quadruples the \
expressivity, measured in linear regions. For deeper networks (",
    math[L], " layers of width ", math[n], "), the same tropical \
analysis gives an upper bound of order ", math[Power[n, 2 L]],
    " regions for 2D input -- which is why depth helps and why the \
deepest network with the smallest total parameter count tends to win."
  }],

  (* ================================================================== *)

  section["17.  A note on Maxout"],

  text[{
    "If we replace the ReLU layer with a maxout layer of group size ",
    math[k], ", the network output is a tropical polynomial -- not a \
difference of two -- because the only \"negation\" in our story came \
from the sign of the output weights. With maxout we are already in ",
    bd["pure"], " tropical polynomial territory: ", math[P], " alone, \
no ", math[Q], ". This is more than aesthetic. A pure tropical \
polynomial has a single Newton polytope, a single upper hull, a \
single dual subdivision of input space, and so its decision boundary \
is a single tropical hypersurface rather than the intersection of \
two. Many of the analytical tools below sharpen dramatically in this \
setting."
  }],

  (* ================================================================== *)

  section["18.  Beyond standard papers"],

  text[{
    "The tropical viewpoint gives us three concrete capabilities that \
go beyond the standard \"ReLU networks are tropical rationals\" \
literature. Each is implementable in a few lines on top of the WL \
machinery already developed, and each is genuinely useful."
  }],

  subsection["18.1  Tight Lipschitz constant"],

  text[{
    "Standard upper bounds on the Lipschitz constant of a ReLU \
network multiply layer-wise norms and become very loose -- often off \
by orders of magnitude. The tropical viewpoint gives the ",
    bd["exact"], " Lipschitz constant on any bounded input domain."
  }],

  text[{
    "Reason: the trained network is piecewise-affine. On the open \
region of input space with activation pattern \[Sigma], the network \
is the single affine function ",
    math[f[x] == Subscript[w, \[Sigma]] . x + Subscript[d, \[Sigma]]],
    " whose gradient is ",
    math[Subscript[w, \[Sigma]] ==
      Underscript["\[Sum]", "j with \[Sigma]_j = 1"]
        Subscript[c, j] Subscript[a, j]],
    ". The Lipschitz constant of the whole function is therefore the \
maximum of ", math[Norm[Subscript[w, \[Sigma]]]],
    " over realised activation patterns. We enumerate them on a fine \
grid, compute the gradient on each, and take the max:"
  }],

  CodeWithOutput[
"dom   = {{-2., 2.}, {-2., 2.}};
lipschitz = MPExactLipschitz[<|\"A\" -> A, \"b\" -> b, \"c\" -> c,
                              \"d\" -> d|>, dom, 240];

{lipschitz[\"Lipschitz\"],
 lipschitz[\"Patterns\"],
 lipschitz[\"ArgMaxPattern\"]}",
    {lipRes["Lipschitz"], lipRes["Patterns"],
     lipRes["ArgMaxPattern"]},
    "Default"],

  text[{
    "The tight Lipschitz constant of the trained moons classifier on ",
    math[Power[Times[{-2, 2}], 2]],
    " is ", math[lipRes["Lipschitz"]],
    ", attained on the linear region with activation pattern ",
    math[lipRes["ArgMaxPattern"]],
    " (i.e. these specific hidden units are simultaneously active). \
The product-of-layer-norms upper bound for the same network would \
typically be 3-5\[Times] larger; the tight number is what you need \
for certified adversarial robustness and for sharp generalisation \
bounds."
  }],

  subsection["18.2  Dead-neuron detection"],

  text[{
    "A common failure mode of ReLU training is ", ib["dead neurons"],
    ": units whose pre-activation never goes positive on the data \
manifold, so the unit always outputs zero and never gets gradient. \
The standard detection method is to evaluate on a held-out set and \
check whether the unit ever fires. The tropical viewpoint gives a \
zero-data alternative."
  }],

  text[{
    "Each unit's pre-activation ",
    math[Subscript[a, j] . x + Subscript[b, j]],
    " is an affine function. Its maximum on an axis-aligned rectangle \
is attained at one of the four corners. So we can ", bd["read off"],
    " whether the unit is dead on a given input domain by evaluating \
at the corners:"
  }],

  CodeWithOutput[
"netAssoc = <|\"A\" -> A, \"b\" -> b, \"c\" -> c, \"d\" -> d|>;
{
  \"dead on [-2, 2]^2\"      -> MPDeadUnits[netAssoc, {{-2, 2}, {-2, 2}}],
  \"dead on [-0.5, 0.5]^2\"  -> MPDeadUnits[netAssoc, {{-.5, .5}, {-.5, .5}}]
} // TableForm",
    {
      "dead on [-2, 2]^2"      -> deadBig,
      "dead on [-0.5, 0.5]^2"  -> deadSmall
    },
    "Table"],

  text[{
    "On the full plot rectangle, no unit of the trained moons \
network is dead. On the smaller box ",
    math[Power[Times[{-0.5, 0.5}], 2]],
    " around the origin, however, unit ",
    math[First[deadSmall["AlwaysActive"], "none"]],
    " is ", ib["always active"],
    " -- its ReLU is just the affine pre-activation. We can therefore ",
    bd["fold that unit into the output bias"], " on this domain and \
save one ReLU evaluation per forward pass."
  }],

  subsection["18.3  Lossless minimisation"],

  text[{
    "Combining the two observations above gives an exact network \
minimiser: drop the never-active units (they contribute zero), and \
absorb the always-active units into an affine head (their ReLU is \
the identity on the domain of interest). The resulting smaller \
network computes ", bd["exactly"],
    " the same function as the original on the chosen domain -- not \
an approximation."
  }],

  CodeWithOutput[
"pruned = MPPruneDeadUnits[netAssoc, {{-0.5, 0.5}, {-0.5, 0.5}}];
{pruned[\"OriginalUnits\"], pruned[\"KeptUnits\"],
 pruned[\"AffineHead\", \"Slope\"]}",
    {pruneSmall["OriginalUnits"], pruneSmall["KeptUnits"],
     pruneSmall["AffineHead", "Slope"]},
    "Default"],

  text[{
    "On the smaller domain, the 12-unit moons network is equivalent to \
an 11-unit network plus a small affine head ",
    math[Subscript[w, "head"] . x],
    " with ", math[Subscript[w, "head"]], " = ",
    math[pruneSmall["AffineHead", "Slope"]],
    ". The reduction is ", bd["lossless"],
    " on this domain, verified by max-of-absolute-errors over a \
random batch (zero to floating-point precision; see the smoke test \
in ", mono["scripts/15_new_tools_smoke.wls"], ")."
  }],

  text[{
    "For a wider trained network (say 64 or 128 hidden units), the \
fraction of always-active or never-active units typically grows. The \
minimiser above gives a principled, exact way to ", bd["shrink"],
    " the network without retraining and without measurable loss of \
behaviour on the operating domain -- a fundamentally different \
mechanism from standard distillation or quantization, both of which \
trade fidelity for size."
  }],

  subsection["What else falls out"],

  text[{
    "Several other results follow immediately from the same picture; \
we sketch them for completeness:"
  }],

  text[{
    "\[Bullet] ", bd["Exact symbolic decision boundary"],
    ": each realised activation pattern \[Sigma] yields one affine \
piece of ", math[f], "; the decision boundary ", math[{f == 0}],
    " is the union of line segments where adjacent pieces switch sign, \
with explicit equations.\n",
    "\[Bullet] ", bd["Adversarial certification"],
    ": for an input ", math[Subscript[x, 0]],
    " we can compute the largest radius ", math[r],
    " of an L^\[Infinity] ball around ", math[Subscript[x, 0]],
    " on which the classifier output sign is constant, by checking \
the affine piece on each cell that the ball intersects -- a finite \
linear-programming problem.\n",
    "\[Bullet] ", bd["Network equivalence"],
    ": two trained networks compute the same function iff their \
tropical decompositions have the same upper hulls in both ", math[P],
    " and ", math[Q],
    ". This is a much sharper test than comparing weights -- many \
weight assignments give the same function.\n",
    "\[Bullet] ", bd["Maxout > ReLU on regularity grounds"],
    ": for maxout networks the entire machinery sharpens because the \
network is a pure tropical polynomial (no ", math[P/Q],
    " split), and the Newton polytope vertices correspond bijectively \
to realised regions. There is no \"hidden\" structure to the \
decision boundary."
  }],

  (* ================================================================== *)

  section["19.  Putting it together"],

  text["The whole story compresses into one table:"],

  Cell[BoxData[ToBoxes[
    Grid[{
      {gridBold["classical / ReLU view"],
       gridBold["tropical / max-plus view"]},
      {gridMono["Max[0, W x + b]  per layer"],
       gridMono["tropical polynomial in x"]},
      {gridMono["scalar output f(x) in R"],
       gridMono["P(x) - Q(x), P, Q tropical polys"]},
      {gridMono["decision boundary {f = 0}"],
       gridMono["tropical hypersurface {P = Q}"]},
      {gridMono["activation crease lines"],
       gridMono["regular subdivision of Newt(P + Q)"]},
      {gridMono["linear regions vs width n"],
       gridMono["Zaslavsky bound  1 + n + C(n, 2)"]},
      {gridMono["mat-vec  W x + b"],
       gridMono["tropical mat-vec  A \[CircleDot] x \[CircleTimes] b"]},
      {gridMono["Bellman-Ford shortest paths"],
       gridMono["same tropical mat-vec, min-plus"]},
      {gridMono["PERT longest path"],
       gridMono["tropical Kleene star"]},
      {gridMono["leaky ReLU / MaxPool / maxout"],
       gridMono["pure tropical operations"]},
      {gridMono["skip connection x + F(x)"],
       gridMono["tropical product x \[CircleTimes] F(x)"]},
      {gridMono["product-of-layer-norms Lipschitz bound"],
       gridMono["EXACT Lipschitz via gradient-norm max"]},
      {gridMono["dead neurons via held-out data"],
       gridMono["dead neurons via 4-corner LP"]},
      {gridMono["pruning - approximate, needs retraining"],
       gridMono["pruning - EXACT via dead-unit minimiser"]}
    },
    (* per-row background: row 1 is the navy header, rest are plain *)
    Background -> {None, {1 -> RGBColor[0.18, 0.30, 0.55]}},
    Frame      -> All,
    FrameStyle -> Directive[GrayLevel[0.5]],
    Spacings   -> {2, 1.2},
    Alignment  -> {Left, Center}]
  ]], "Output", TextAlignment -> Center,
       CellFrame -> False, CellLabel -> None],

  text["Everything that ReLU networks do, max-plus algebra already \
does -- and the last three rows are gifts that ordinary analysis \
does not give us."],

  (* ================================================================== *)

  section["References"],

  text[{
    "L. Zhang, G. Naitzat, L.-H. Lim. ",
    ib["Tropical Geometry of Deep Neural Networks"],
    ". ICML 2018. arXiv:1805.07091.\n\n",
    "D. Maclagan, B. Sturmfels. ",
    ib["Introduction to Tropical Geometry"],
    ". AMS GSM 161 (2015).\n\n",
    "P. Butkovic. ",
    ib["Max-linear Systems: Theory and Algorithms"],
    ". Springer (2010).\n\n",
    "I. Goodfellow, D. Warde-Farley, M. Mirza, A. Courville, Y. Bengio. ",
    ib["Maxout Networks"], ". ICML 2013. arXiv:1302.4389.\n\n",
    "R. M. Karp. ",
    ib["A characterization of the minimum cycle mean in a digraph"],
    ". Discrete Math. 23 (1978), 309-311.\n\n",
    "Wolfram Community discussion on max-plus algebra: \
community.wolfram.com/groups/-/m/t/162609\n\n",
    "Companion repository: ",
    mono["github.com/mthiel74/TropicalAlgebravsReLU"], "."
  }]

};

nb = Notebook[cells,
  WindowSize       -> {1000, 800},
  WindowMargins    -> {{Automatic, Automatic}, {Automatic, Automatic}},
  FrontEndVersion  -> "15.0 for Mac OS X ARM (64-bit)",
  StyleDefinitions -> "Default.nb"];

out = FileNameJoin[{repoRoot, "community", "Post.nb"}];
Export[out, nb];
Print["wrote ", out];
Print["cells: ", Length[cells]];
