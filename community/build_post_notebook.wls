#!/usr/bin/env wolframscript
(* Build the Wolfram Community post notebook
   "Tropical Algebra of ReLU Networks".
   Generates community/Post.nb from scratch each time. *)

repoRoot   = ParentDirectory[DirectoryName[$InputFileName]];
figureDir  = FileNameJoin[{repoRoot, "figures"}];

(* ---- cell helpers ----------------------------------------------- *)

title[s_]      := Cell[s, "Title"];
section[s_]    := Cell[s, "Section"];
subsection[s_] := Cell[s, "Subsection"];

(* Use TextData with strings + StyleBox to mix italic / bold inline *)
text[content_List] := Cell[TextData[content], "Text"];
text[content_String] := Cell[TextData[{content}], "Text"];

ib[s_] := StyleBox[s, FontSlant -> "Italic"];
bd[s_] := StyleBox[s, FontWeight -> "Bold"];
code[s_] := StyleBox[s, FontFamily -> "Source Code Pro",
                     Background -> GrayLevel[0.95]];

inputCell[txt_] := Cell[txt, "Input"];

imageFromFile[path_, caption_String, scale_:0.55] := Module[{img},
  img = If[FileExistsQ[path], Import[path],
           Graphics[{Text[Style["missing: " <> path, Red, 18]]},
              ImageSize -> 600]];
  Sequence @@ {
    Cell[BoxData[ToBoxes[Magnify[img, scale]]], "Output",
         CellFrame -> False, CellLabel -> None],
    Cell[caption, "Text",
         FontFamily -> "Helvetica",
         FontSlant -> Italic, FontSize -> 11,
         TextAlignment -> Center]}
];

(* ---- the notebook content -------------------------------------- *)

cells = {

  title["Tropical Algebra of ReLU Networks"],

  text[{
    bd["A ReLU neural network is literally a tropical rational map."],
    " We take that statement seriously. First we set up the max-plus \
(tropical) semiring from scratch and run it through three classical \
applications -- shortest paths, project scheduling, and Karp's \
eigenvalue. Then we show that a 1-hidden-layer ReLU classifier is a \
difference of two tropical polynomials, derive that fact symbolically \
on small examples, and finally train an actual classifier in WL and \
display its decision boundary as a tropical hypersurface."}],

  text[{
    "Companion code lives at ", code["github.com/mthiel74/TropicalAlgebravsReLU"], "."}],

  text[{
    "Reference: L. Zhang, G. Naitzat, L.-H. Lim, ", ib["Tropical Geometry of Deep Neural Networks"],
    ", ICML 2018."}],

  (* ============================================================== *)

  section["1.  Max-plus algebra in 60 seconds"],

  text["The tropical (max-plus) semiring is the set of real numbers \
extended with negative infinity, equipped with two operations that look \
like the addition and multiplication of classical algebra but are \
actually:"],

  text[{"  ", code["a \[CirclePlus] b := Max[a, b]"],
        "         (the tropical \"sum\")\n",
        "  ", code["a \[CircleTimes] b := a + b"],
        "          (the tropical \"product\")"}],

  text["The identities flip from {0, 1} to {-Infinity, 0}. Apart from \
the lack of additive inverses (you cannot tropically subtract \
universally) all the familiar axioms hold."],

  imageFromFile[FileNameJoin[{figureDir, "13_semiring_card.png"}],
    "Figure 13. Classical and tropical algebra side by side.  Every \
axiom of a commutative ring carries over EXCEPT additive inverses.",
    0.7],

  text[{
    bd["Operators in WL."],
    " ", code["\\[CirclePlus]"],
    " enters via Esc+c+p+Esc and binds at low precedence; ",
    code["\\[CircleTimes]"],
    " (Esc+c+t+Esc) and ",
    code["\\[CircleDot]"],
    " (Esc+c+.+Esc) are similar. We define them once and never \
again think about it."}],

  inputCell["\
(* The four max-plus operators, attached to the System` symbols so the
   infix syntax  a \\[CirclePlus] b \\[CirclePlus] c, A \\[CircleDot] B  works directly. *)

Unprotect[System`CirclePlus, System`CircleTimes,
           System`CircleMinus, System`CircleDot];

System`CirclePlus[a_, b_] := Max[a, b];
System`CirclePlus[a_, b__] := Max[a, b];
SetAttributes[System`CirclePlus, {Listable, Flat, OneIdentity, Orderless}];

System`CircleTimes[a_, b_] := a + b;
System`CircleTimes[a_, b__] := Plus[a, b];
SetAttributes[System`CircleTimes, {Listable, Flat, OneIdentity, Orderless}];

System`CircleDot[A_?MatrixQ, B_?MatrixQ] := Inner[Plus, A, B, Max];
System`CircleDot[A_?MatrixQ, v_?VectorQ] := Inner[Plus, A, v, Max];
System`CircleDot[v_?VectorQ, A_?MatrixQ] := Inner[Plus, v, A, Max];

Protect[System`CirclePlus, System`CircleTimes,
         System`CircleMinus, System`CircleDot];"],

  inputCell["(* try them *)
{3 \\[CirclePlus] 7, 3 \\[CircleTimes] 7, {1, 2, 3} \\[CirclePlus] {4, 0, 5}}"],

  text["Output:  ", code["{7, 10, {4, 2, 5}}"]],

  (* ============================================================== *)

  section["2.  Tropical matrices, identity, Kleene star"],

  text["A tropical matrix is just a real matrix; what changes is the \
multiplication. The identity has 0 on the diagonal and -Infinity \
everywhere else (because -Infinity is the new \"zero\")."],

  inputCell["MPIdentity[n_Integer] :=
  Normal @ SparseArray[{i_, i_} -> 0, {n, n}, -Infinity];

MPMatrixPower[A_, 0] := MPIdentity[Length[A]];
MPMatrixPower[A_, 1] := A;
MPMatrixPower[A_, k_] := Nest[A \\[CircleDot] # &, A, k - 1];

KleeneStar[A_?MatrixQ] := Module[{n = Length[A], acc, term},
  acc  = MPIdentity[n];
  term = MPIdentity[n];
  Do[
    term = A \\[CircleDot] term;
    acc  = MapThread[Max, {acc, term}, 2],
    {n - 1}];
  acc
];"],

  text["The Kleene star of A is the matrix whose (i, j) entry is the \
longest-path weight from i to j over paths of any length up to n - 1. \
On a finite directed graph with no positive-weight cycles this stabilises."],

  (* ============================================================== *)

  section["3.  Classical application 1 - shortest paths"],

  text["The Bellman-Ford recurrence ", code["d_{t+1}(v) = min_u (d_t(u) + cost(u,v))"],
   " is, in min-plus algebra, literally a matrix-vector product. So \
the Kleene star on the cost matrix (negated, to flip Max to Min) gives \
the full all-pairs distance matrix in one shot."],

  imageFromFile[FileNameJoin[{figureDir, "15_shortest_paths_infographic.png"}],
    "Figure 15. Shortest paths = tropical (min-plus) algebra. The \
Bellman-Ford recurrence is a single tropical mat-vec; iterating it \
to a fixed point gives the Kleene star.",
    0.55],

  imageFromFile[FileNameJoin[{figureDir, "07_shortest_paths.png"}],
    "Figure 7. Five-node weighted digraph and its complete shortest-path \
distance matrix computed as -KleeneStar[-costM].",
    0.45],

  (* ============================================================== *)

  section["4.  Classical application 2 - project scheduling"],

  text["Suppose n tasks have durations d_i and a precedence DAG \
(\"task i must finish before task j starts\"). The earliest \
completion time of each task is the longest path from any source to \
that task plus its own duration. That longest path is, again, a \
tropical mat-vec / Kleene star computation."],

  imageFromFile[FileNameJoin[{figureDir, "08_pert_scheduling.png"}],
    "Figure 8. An 8-task project DAG and its earliest schedule. The \
makespan = 7 is the longest path from any root to any leaf in the \
tropical sense.",
    0.55],

  (* ============================================================== *)

  section["5.  Tropical polynomials and the Newton polytope"],

  text[{"A ", bd["tropical polynomial"],
    " in d variables is a function of the form ",
    code["p(x) = max_i (a_i . x + c_i)"],
    " where the a_i are integer (or real) exponent vectors and c_i are \
real coefficients. It is convex piecewise-linear by construction."}],

  imageFromFile[FileNameJoin[{figureDir, "05_tropical_poly_1d.png"}],
    "Figure 5. A univariate tropical polynomial with 4 monomials and \
its Newton polygon. All four monomials sit on the upper hull, so the \
polynomial has 3 corners.",
    0.5],

  text["The crucial fact: only monomials whose lifted point (a_i, c_i) \
sits on the UPPER convex hull contribute a non-empty region to the \
piecewise-linear envelope. Interior monomials are dominated everywhere."],

  imageFromFile[FileNameJoin[{figureDir, "16_newton_duality_infographic.png"}],
    "Figure 16. Newton-polytope duality. Monomials lifted to one \
dimension higher, the upper convex hull, and the resulting tropical \
polynomial.  In higher dimension the upper hull projects to a regular \
subdivision of input space.",
    0.55],

  text["In two variables the same picture holds with the upper hull \
a 2D-polytope's set of facets:"],

  imageFromFile[FileNameJoin[{figureDir, "06_tropical_curve_2d.png"}],
    "Figure 6. The bivariate tropical polynomial p(x, y) = max(0, x, y, x + y - 1) \
has four pieces (left) and Newton polytope the unit square (right).",
    0.5],

  (* ============================================================== *)

  section["6.  Tropical eigenvalues - Karp's algorithm"],

  text["For a square tropical matrix A the role of the eigenvalue is \
played by the MAX MEAN CYCLE WEIGHT of the associated weighted digraph:"],

  text[{"  ", code["\\[Lambda]_max(A) = max over directed cycles C of  (sum of edge weights in C) / (length of C)."]}],

  text["This is the unique tropical eigenvalue when A is irreducible. \
Karp's algorithm computes it in O(n^3) by tracking maximum-weight \
paths of every length up to n."],

  imageFromFile[FileNameJoin[{figureDir, "09_karp_eigenvalue.png"}],
    "Figure 9. A small digraph with a dominant cycle 1 -> 2 -> 3 -> 4 -> 1 of \
total weight 10 over length 4. Iterating x_{t+1} = A \[CircleDot] x_t makes the mean \
of x grow at the eigenvalue rate \[Lambda]_max = 2.5 per step.",
    0.5],

  (* ============================================================== *)

  section["7.  ReLU = the tropical sum  0 \[CirclePlus] x"],

  text[{"Everything above stands on its own. Now we cross over to \
neural networks. The bridge is the observation that ", bd["ReLU is the unit operation of the max-plus semiring"],
    "."}],

  imageFromFile[FileNameJoin[{figureDir, "14_relu_is_tropical.png"}],
    "Figure 14. ReLU(x) = max(0, x) is, in tropical notation, 0 \[CirclePlus] x: \
the tropical sum of the input with the multiplicative identity 0. \
A single ReLU unit is a degree-1 tropical polynomial.",
    0.55],

  (* ============================================================== *)

  section["8.  The decisive identity"],

  text["A 1-hidden-layer ReLU classifier is a CLASSICAL SUM of ReLU \
units. The identity that lets us turn that classical sum into a \
single tropical polynomial is"],

  text[{"  ", code["Max[0, A] + Max[0, B]  ==  Max[0, A, B, A + B]"]}],

  text["which, read tropically, says that the classical sum of two \
ReLU units is the tropical PRODUCT of their tropical polynomials -- \
each unit contributing two monomials, the product giving four."],

  imageFromFile[FileNameJoin[{figureDir, "11_analytical_tropical_product.png"}],
    "Figure 11. The identity that does all the work, and the four \
regions of the right-hand side over the (A, B) plane.  Each region \
corresponds to one of the four monomials in the tropical product.",
    0.55],

  (* ============================================================== *)

  section["9.  Symbolic derivation: 2 -> 1 -> 1 network"],

  text["The smallest non-trivial classifier has 1 hidden unit. Splitting \
the output weight on its sign gives an explicit (P, Q) pair:"],

  imageFromFile[FileNameJoin[{figureDir, "10_analytical_2-1-1.png"}],
    "Figure 10. A 1-hidden-unit ReLU network rewritten as P(x1, x2) - \
Q(x1, x2), case-analysed on the sign of the output weight. The decision \
boundary {f = 0} is the tropical hypersurface {P = Q}.",
    0.6],

  section["10.  Two units: a proper tropical rational"],

  text["With two hidden units we can already produce a non-trivial \
tropical rational map (when one output weight is negative) or a \
tropical polynomial with FOUR monomials (when both output weights are \
positive). The 4 monomials come from the identity in Section 8."],

  imageFromFile[FileNameJoin[{figureDir, "12_analytical_2-2-1.png"}],
    "Figure 12. The 2 -> 2 -> 1 network has, when both output weights are \
positive, a single tropical polynomial of degree 1 with 4 monomials \
(the Minkowski sum of two segments).  With mixed signs it is a \
genuine difference P - Q.",
    0.6],

  (* ============================================================== *)

  section["11.  Train a tiny classifier"],

  text["We pick the two-moons dataset and a 2 -> 12 -> 1 ReLU MLP. The \
code below is what you'd type in a fresh kernel."],

  inputCell["SeedRandom[7];
makeMoons[nPerClass_: 200, noise_: 0.15] :=
  Module[{th},
    th = Subdivide[0, Pi, nPerClass - 1];
    Join[
      Thread[{Transpose[{Cos[th], Sin[th]}] +
              RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}], 1}],
      Thread[{Transpose[{1 - Cos[th], -Sin[th] - 0.5}] +
              RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}], -1}]]
  ];
data = makeMoons[];
trainData = (#[[1]] -> {N[#[[2]]]}) & /@ data;

netSpec = NetChain[
  {LinearLayer[12], ElementwiseLayer[\"ReLU\"], LinearLayer[1]},
  \"Input\" -> 2];

trained = NetTrain[netSpec, trainData,
  MaxTrainingRounds -> 800, BatchSize -> 64, LearningRate -> 0.01,
  TrainingProgressReporting -> None, RandomSeeding -> 1];"],

  inputCell["(* extract the trained weights *)
A = Normal @ NetExtract[trained, {1, \"Weights\"}];
b = Normal @ NetExtract[trained, {1, \"Biases\"}];
c = Flatten @ Normal @ NetExtract[trained, {3, \"Weights\"}];
d = First @ Flatten @ Normal @ NetExtract[trained, {3, \"Biases\"}];

(* two forward passes *)

fReLU[x_]    := c . Map[Max[0, #] &, A . x + b] + d;

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
xs   = RandomReal[{-2, 2}, {2000, 2}];
yR   = fReLU /@ xs;
yMP  = fMaxPlus /@ xs;
Max[Abs[yR - yMP]]"],

  text["The two evaluators agree to machine precision -- they are the \
same function in two different algebras."],

  (* ============================================================== *)

  section["12.  Forward-pass equivalence on the three datasets"],

  imageFromFile[FileNameJoin[{figureDir, "01_forward_equivalence.png"}],
    "Figure 1. On three trained networks (XOR, moons, spirals), the \
ReLU forward pass and the max-plus forward pass agree at ~10^-14 over \
1400+ inputs each.",
    0.45],

  (* ============================================================== *)

  section["13.  Decision boundary = tropical hypersurface"],

  text["The hidden units cut the input plane along n straight crease \
lines a_j . x + b_j = 0. On each open region of that arrangement the \
activation pattern sigma in {0, 1}^n is constant and f is affine. \
The decision boundary {f = 0} is the locus where the convex \
piecewise-linear surfaces P and Q meet -- a tropical hypersurface."],

  imageFromFile[FileNameJoin[{figureDir, "02_decision_tropical_hypersurface.png"}],
    "Figure 2. Decision boundary = tropical hypersurface for the three \
trained classifiers. The pastel patches are the activation-pattern \
regions, the thin grey lines are the ReLU creases, the black is the \
classical {f = 0} boundary, the red dashed is the max-plus {P = Q} \
hypersurface (overlaid on top -- they coincide).",
    0.55],

  (* ============================================================== *)

  section["14.  Newton polytope of the trained network"],

  text["The tropical polynomial P (and similarly Q) for the trained \
moons network has its own Newton polytope and upper hull. The \
duality of Section 5 means that interior lifted points contribute \
nothing -- they are dominated everywhere."],

  imageFromFile[FileNameJoin[{figureDir, "03_newton_polytope.png"}],
    "Figure 3. Newton polytope of P (top row) and Q (bottom row) for \
the trained moons classifier. The 3D upper hull (left) and the \
linear-piece partition of input space (right) are dual; 4 of P's 7 \
monomials and 5 of Q's 7 monomials realise a region.",
    0.55],

  (* ============================================================== *)

  section["15.  Linear regions scale like n^2"],

  text["By Zaslavsky's theorem, n straight lines in general position \
in R^2 partition the plane into at most Z(n) = 1 + n + C(n, 2) regions. \
That is the Zhang-Naitzat-Lim upper bound for a 2-hidden-1 ReLU \
classifier. We sweep hidden width n in {2, 4, 8, 16, 32, 64, 96} on \
moons (3 seeds each), count realised activation patterns on a fine \
grid, and compare."],

  imageFromFile[FileNameJoin[{figureDir, "04_region_count_vs_width.png"}],
    "Figure 4. Empirical linear regions vs hidden width n: the empirical \
curve tracks the Zaslavsky bound at slope 2 on log-log axes. \
Trained networks realise 0.5-0.95 of the maximum.",
    0.55],

  (* ============================================================== *)

  section["16.  Putting it together"],

  text["The takeaway in one table:"],

  text[{
    code["+----------------------------------+---------------------------------------+\n"],
    code["| classical / ReLU                 | tropical / max-plus                   |\n"],
    code["+----------------------------------+---------------------------------------+\n"],
    code["| max(0, W x + b) per layer        | tropical polynomial in x              |\n"],
    code["| scalar output f(x) in R          | P(x) - Q(x) with P, Q tropical polys  |\n"],
    code["| decision boundary {f = 0}        | tropical hypersurface {P = Q}         |\n"],
    code["| activation crease lines          | regular subdivision of Newt(P \\[CirclePlus] Q)        |\n"],
    code["| linear regions vs width          | Zaslavsky bound  1 + n + C(n, 2)      |\n"],
    code["| matrix-vector  W x + b           | tropical mat-vec  A \\[CircleDot] x \\[CircleTimes] b           |\n"],
    code["| Bellman-Ford for shortest paths  | the SAME tropical mat-vec (min-plus)  |\n"],
    code["| PERT longest path                | tropical Kleene star                  |\n"],
    code["+----------------------------------+---------------------------------------+\n"]
  }],

  text["Everything that ReLU networks do, max-plus algebra ALREADY \
does. The two are the same object viewed through two different operator \
keys."],

  section["References"],

  text[{
    "Liwen Zhang, Gregory Naitzat, Lek-Heng Lim. ", ib["Tropical Geometry of Deep Neural Networks"],
    ". ICML 2018. arXiv:1805.07091.\n\n",
    "D. Maclagan, B. Sturmfels. ", ib["Introduction to Tropical Geometry"], ". AMS GSM 161 (2015).\n\n",
    "P. Butkovic. ", ib["Max-linear Systems: Theory and Algorithms"], ". Springer (2010).\n\n",
    "All code is at  ", code["github.com/mthiel74/TropicalAlgebravsReLU"], "."}]

};

nb = Notebook[cells,
  WindowSize    -> {1000, 800},
  WindowMargins -> {{Automatic, Automatic}, {Automatic, Automatic}},
  FrontEndVersion -> "15.0 for Mac OS X ARM (64-bit)",
  StyleDefinitions -> "Default.nb"];

out = FileNameJoin[{repoRoot, "community", "Post.nb"}];
Export[out, nb];
Print["wrote ", out];
Print["cells: ", Length[cells]];
