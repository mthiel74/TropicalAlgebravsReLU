#!/usr/bin/env wolframscript
(* Build the Wolfram Community post notebook
   "Tropical Algebra of ReLU Networks".

   All mathematics lives in the notebook itself -- either as inline LaTeX
   ($...$ inside text cells) or as runnable Wolfram Language code cells
   that typeset their output through TraditionalForm.  The four GPT-
   generated illustrations (figures 13-16) are PURELY illustrative
   section openers: no equations are written into the images. *)

repoRoot   = ParentDirectory[DirectoryName[$InputFileName]];
figureDir  = FileNameJoin[{repoRoot, "figures"}];

(* ---- cell helpers ----------------------------------------------- *)

title[s_]      := Cell[s, "Title"];
section[s_]    := Cell[s, "Section"];
subsection[s_] := Cell[s, "Subsection"];

text[content_List]   := Cell[TextData[content], "Text"];
text[content_String] := Cell[TextData[{content}], "Text"];

ib[s_] := StyleBox[s, FontSlant -> "Italic"];
bd[s_] := StyleBox[s, FontWeight -> "Bold"];
code[s_] := StyleBox[s, FontFamily -> "Source Code Pro",
                     Background -> GrayLevel[0.95]];

(* an inline typeset formula inside a Text cell *)
math[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[expr]]],
                     "InlineFormula"];

(* a displayed (centred) typeset formula -- its own Cell *)
displayMath[expr_] := Cell[BoxData[ToBoxes[TraditionalForm[expr]]],
                            "DisplayFormula", TextAlignment -> Center];

inputCell[txt_]  := Cell[txt, "Input"];
outputCell[txt_] := Cell[txt, "Output", CellLabel -> "(* result *)"];

imageFromFile[path_, caption_String, scale_ : 0.55] := Module[{img},
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

(* ---- notebook content ------------------------------------------ *)

cells = {

  title["Tropical Algebra of ReLU Networks"],

  text[{
    bd["A ReLU neural network is literally a tropical rational map."],
    " We take that statement seriously. First we set up the max-plus \
(tropical) semiring from scratch and walk through three classical \
applications -- shortest paths, project scheduling, and Karp's \
eigenvalue. Then we show that a 1-hidden-layer ReLU classifier is a \
difference of two tropical polynomials, derive that fact symbolically \
on small examples, and finally train an actual classifier in WL and \
display its decision boundary as a tropical hypersurface."
  }],

  text[{"Companion code at ", code["github.com/mthiel74/TropicalAlgebravsReLU"], "."}],

  text[{
    "Reference: L. Zhang, G. Naitzat, L.-H. Lim, ",
    ib["Tropical Geometry of Deep Neural Networks"],
    ", ICML 2018. arXiv:1805.07091."
  }],

  imageFromFile[FileNameJoin[{figureDir, "13_semiring_landscapes.png"}],
    "Two windows on two algebras. Left: the smooth, continuous world \
of classical arithmetic (R, +, *).  Right: the same landscape under \
the operations (Max, +) -- everything piecewise-linear, every \
operation producing a crisp polyhedral surface. The two are connected \
by a small bridge.",
    0.55],

  (* ============================================================== *)

  section["1.  Max-plus algebra in 60 seconds"],

  text[{
    "The tropical (max-plus) semiring is ", math[Subscript[R, "max"]],
    " = $\\mathbb{R} \\cup \\{-\\infty\\}$, equipped with two operations \
that look like the addition and multiplication of classical algebra \
but are: tropical addition ", math[a \[CirclePlus] b], " = ",
    math[Max[a, b]], ", and tropical multiplication ",
    math[a \[CircleTimes] b], " = ", math[a + b], "."
  }],

  text[{
    "The identities flip: the additive identity is ", math[-Infinity],
    " (since ", math[Max[-Infinity, a]], " = ", math[a], "), and the \
multiplicative identity is ", math[0], " (since ", math[0 + a], " = ",
    math[a], "). Apart from the lack of additive inverses, every familiar \
axiom of a commutative ring carries over. Distributivity, in particular, \
holds: $a \\otimes (b \\oplus c) = (a \\otimes b) \\oplus (a \\otimes c)$."
  }],

  text[{
    bd["Operators in WL."],
    " ", code["\\[CirclePlus]"],
    " (Esc-c-p-Esc), ", code["\\[CircleTimes]"], " (Esc-c-t-Esc), and ",
    code["\\[CircleDot]"], " (Esc-c-.-Esc). We attach definitions to the \
System` symbols once so the infix syntax just works:"
  }],

  inputCell[
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

Protect[System`CirclePlus, System`CircleTimes, System`CircleDot];"
  ],

  inputCell["{3 \\[CirclePlus] 7,  3 \\[CircleTimes] 7,
 {1, 2, 3} \\[CirclePlus] {4, 0, 5}}"],
  text[{code["(* {7,  10,  {4, 2, 5}} *)"]}],

  (* ============================================================== *)

  section["2.  Tropical matrices, identity, Kleene star"],

  text[{
    "A tropical matrix is just a real matrix; what changes is the \
multiplication. The identity has $0$ on the diagonal and $-\\infty$ \
everywhere else (since $-\\infty$ is the new \"zero\"). The Kleene star \
of $A$ is "
  }],

  displayMath[Subscript[A, "*"] ==
    Subscript[I, n] \[CirclePlus] A \[CirclePlus]
      Superscript[A, \[CircleTimes]2] \[CirclePlus]
      Superscript[A, \[CircleTimes]3] \[CirclePlus] CenterDot
  ],

  text["i.e. the matrix whose $(i, j)$ entry is the largest-weight \
path from $i$ to $j$ over any number of steps. On a graph with no \
positive-weight cycles the series stabilises after at most $n - 1$ \
products."],

  inputCell["MPIdentity[n_Integer] :=
  Normal @ SparseArray[{i_, i_} -> 0, {n, n}, -Infinity];

MPMatrixPower[A_, 0]  := MPIdentity[Length[A]];
MPMatrixPower[A_, 1]  := A;
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

  (* ============================================================== *)

  section["3.  Classical application 1 -- shortest paths"],

  text[{
    "The Bellman-Ford recurrence $d_{t+1}(v) = \\min_u\\,(d_t(u) + \
\\text{cost}(u,v))$ is, in min-plus algebra, literally a matrix-vector \
product:"
  }],

  displayMath[Subscript[d, t + 1] == Subscript[d, t] \[CircleDot] cost],

  text["where the tropical \\[CircleDot] uses Min for \\[CirclePlus] and \
+ for \\[CircleTimes]. Iterating to a fixed point (the Kleene star of \
the cost matrix) gives the full all-pairs distance matrix in one shot."],

  imageFromFile[FileNameJoin[{figureDir, "15_graph_paths.png"}],
    "Illustration. A directed network with edge weights; the dim edges \
are the full graph, the glowing coral curve is the cheapest path \
between two nodes -- the entry the tropical mat-vec is selecting.",
    0.55],

  imageFromFile[FileNameJoin[{figureDir, "07_shortest_paths.png"}],
    "Figure: a 5-node weighted digraph and its complete shortest-path \
distance matrix computed as -KleeneStar[-costM] (negate to flip Max \
to Min).",
    0.5],

  (* ============================================================== *)

  section["4.  Classical application 2 -- project scheduling"],

  text["Suppose $n$ tasks have durations $d_i$ and a precedence DAG \
(task $i$ must finish before task $j$ starts). The earliest \
completion time of each task is the longest path from any source to \
that task plus its own duration -- again a tropical mat-vec / Kleene \
star, this time with Max for \\[CirclePlus]."],

  imageFromFile[FileNameJoin[{figureDir, "08_pert_scheduling.png"}],
    "An 8-task project DAG and its earliest schedule. The makespan = 7 \
is the longest path through the DAG.",
    0.55],

  (* ============================================================== *)

  section["5.  Tropical polynomials and the Newton polytope"],

  text["A tropical polynomial in $d$ variables is"],

  displayMath[
    p[Subscript[x, 1], Ellipsis, Subscript[x, d]] ==
      Underscript[Max, i] (Subscript[a, i] . x + Subscript[c, i])
  ],

  text["with exponent vectors $a_i$ (often integer, sometimes real) \
and real coefficients $c_i$. It is convex piecewise-linear by \
construction. The convex hull of the exponents is the Newton polytope; \
lifting each exponent $a_i$ to $(a_i, c_i)$ and taking the upper \
convex hull induces a regular subdivision of the Newton polytope. \
That subdivision is DUAL to the linear-region decomposition of input \
space induced by $p$."],

  text["Crucial: only monomials whose lifted point sits on the upper \
hull contribute a region. Interior monomials are dominated everywhere \
and never realise the max."],

  imageFromFile[FileNameJoin[{figureDir, "05_tropical_poly_1d.png"}],
    "A univariate tropical polynomial p(x) = max(2, 2x+1, x+2, 3x-2) \
and its Newton polygon. All four monomials sit on the upper hull, so \
the polynomial has 3 corners.",
    0.5],

  imageFromFile[FileNameJoin[{figureDir, "06_tropical_curve_2d.png"}],
    "A bivariate tropical polynomial p(x, y) = max(0, x, y, x+y-1) has \
four linear pieces (left) and Newton polytope the unit square (right).",
    0.5],

  (* ============================================================== *)

  section["6.  Tropical eigenvalues -- Karp's algorithm"],

  text["For a square tropical matrix $A$ the role of the eigenvalue \
is played by the maximum mean cycle weight of the associated weighted \
digraph,"],

  displayMath[Subscript[\[Lambda], max][A] ==
    Underscript[Max, "cycles C"]
      Subscript["mean-weight", "of C"]],

  text["i.e. the largest value of (sum of edge weights in $C$) / \
(length of $C$) over all directed cycles $C$. This is the unique \
tropical eigenvalue when $A$ is irreducible. Karp's algorithm computes \
it in $O(n^3)$."],

  imageFromFile[FileNameJoin[{figureDir, "09_karp_eigenvalue.png"}],
    "A digraph with a dominant cycle 1 -> 2 -> 3 -> 4 -> 1 of total \
weight 10 over length 4. Iterating x_{t+1} = A \[CircleDot] x_t makes \
the mean of x grow at the eigenvalue rate \[Lambda]_max = 2.5 per step.",
    0.5],

  (* ============================================================== *)

  section["7.  ReLU = the tropical sum  0 \[CirclePlus] x"],

  text[{
    "Everything above stands on its own. Now we cross over to neural \
networks. The bridge is the observation that ",
    bd["ReLU is the unit operation of the max-plus semiring"], ": "
  }],

  displayMath[
    "ReLU"[x] == Max[0, x] == 0 \[CirclePlus] x],

  text["A single ReLU unit on a pre-activation $a \\cdot x + b$ is \
therefore the smallest possible degree-1 tropical polynomial:"],

  displayMath[h[x] == 0 \[CirclePlus] (a x + b) == Max[0, a x + b]],

  imageFromFile[FileNameJoin[{figureDir, "14_relu_ridge.png"}],
    "Illustration. ReLU as a single piecewise-linear ridge: a flat \
shadow plateau meeting a sharp inclined facet at a kink. The whole \
forward pass of a network is built by stacking and summing these \
ridges.",
    0.55],

  (* ============================================================== *)

  section["8.  The decisive identity"],

  text["A 1-hidden-layer ReLU classifier is a CLASSICAL SUM of ReLU \
units. The identity that lets us turn that classical sum into a single \
tropical polynomial is"],

  displayMath[Max[0, A] + Max[0, B] == Max[0, A, B, A + B]],

  text["Read tropically: $(0 \\oplus A) \\otimes (0 \\oplus B) = 0 \
\\oplus A \\oplus B \\oplus (A \\otimes B)$. The classical sum of two \
ReLU units is the tropical PRODUCT of their tropical polynomials -- \
each unit contributing two monomials, the product giving four. This \
identity is the engine behind everything that follows."],

  inputCell["(* numerical check of the identity *)
SeedRandom[1];
Table[
  Module[{aval = RandomReal[{-1, 1}], bval = RandomReal[{-1, 1}]},
    {Max[0, aval] + Max[0, bval],
     Max[0, aval, bval, aval + bval]}],
  {5}]"],
  text[{code["(* each row gives two equal numbers *)"]}],

  (* ============================================================== *)

  section["9.  Symbolic derivation: 2 -> 1 -> 1 network"],

  text["The smallest non-trivial classifier has 1 hidden unit. Hidden \
unit with weights $a, b$, bias $c$, output weight $w$, output bias $e$:"],

  displayMath[h == Max[0, a Subscript[x, 1] + b Subscript[x, 2] + c]],
  displayMath[f == w h + e],

  subsection["Case 1.  $w > 0$"],

  text["We can pull $w$ inside the Max because it is non-negative: $w \
\\cdot \\max(\\alpha, \\beta) = \\max(w\\alpha, w\\beta)$. So"],

  displayMath[
    f == Max[e, w (a Subscript[x, 1] + b Subscript[x, 2] + c) + e]],

  text["which is $P(x) - Q(x)$ with $Q \\equiv 0$ and $P$ a tropical \
polynomial of degree 1 (two monomials)."],

  subsection["Case 2.  $w < 0$"],

  text["Write $w = -|w|$. Then $-|w| \\cdot \\max(0, \\ell) = -\\max(0, \
|w|\\,\\ell)$, so"],

  displayMath[f == e - Max[0, Abs[w] (a Subscript[x, 1] +
                                     b Subscript[x, 2] + c)]],

  text["which is $P(x) - Q(x)$ with $P \\equiv e$ and $Q$ a tropical \
polynomial of degree 1.  In both cases the decision boundary $\\{f = \
0\\}$ is the locus $\\{P = Q\\}$ -- the tropical hypersurface."],

  inputCell[
"(* symbolic derivation, executable *)
ClearAll[a, b, c, w, e, x1, x2];
hExpr = Max[0, a x1 + b x2 + c];
fExpr = w hExpr + e;
{
  \"case w > 0\"  -> Max[e, w (a x1 + b x2 + c) + e],
  \"case w < 0\"  -> e - Max[0, -w (a x1 + b x2 + c)]
} // TableForm"
  ],

  (* ============================================================== *)

  section["10.  Two hidden units"],

  text["With two hidden units we can already produce a non-trivial \
tropical rational (mixed signs) or a tropical polynomial with FOUR \
monomials (both signs positive). Take $h_j = \\max(0, a_j x_1 + b_j \
x_2 + c_j)$ and $f = w_1 h_1 + w_2 h_2 + e$."],

  subsection["Both output weights positive"],

  text["Apply the identity of Section 8 to the two ReLU units:"],

  displayMath[
    P == Max[e,
             Subscript[w, 1] (Subscript[a, 1] Subscript[x, 1] +
               Subscript[b, 1] Subscript[x, 2] + Subscript[c, 1]) + e,
             Subscript[w, 2] (Subscript[a, 2] Subscript[x, 1] +
               Subscript[b, 2] Subscript[x, 2] + Subscript[c, 2]) + e,
             Subscript[w, 1] (Subscript[a, 1] Subscript[x, 1] +
               Subscript[b, 1] Subscript[x, 2] + Subscript[c, 1]) +
             Subscript[w, 2] (Subscript[a, 2] Subscript[x, 1] +
               Subscript[b, 2] Subscript[x, 2] + Subscript[c, 2]) + e
            ]],

  text["The four monomials have exponents (in input space) $\\{(0, 0), \
(w_1 a_1, w_1 b_1), (w_2 a_2, w_2 b_2), (w_1 a_1 + w_2 a_2, w_1 b_1 + \
w_2 b_2)\\}$ -- the Minkowski sum of the two single-unit Newton segments \
and the origin."],

  subsection["Mixed signs ($w_1 > 0$, $w_2 < 0$)"],

  displayMath[P == Max[e,
    Subscript[w, 1] (Subscript[a, 1] Subscript[x, 1] +
      Subscript[b, 1] Subscript[x, 2] + Subscript[c, 1]) + e]],

  displayMath[Q == Max[0,
    Abs[Subscript[w, 2]] (Subscript[a, 2] Subscript[x, 1] +
      Subscript[b, 2] Subscript[x, 2] + Subscript[c, 2])]],

  text["A proper tropical rational. Adding more hidden units of the \
same sign tropical-multiplies into $P$ (or $Q$), doubling the monomial \
count in the worst case."],

  (* ============================================================== *)

  section["11.  Train a tiny classifier"],

  text["Two-moons dataset, a $2 \\to 12 \\to 1$ ReLU MLP, NetTrain. \
The whole block runs in a fresh kernel:"],

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

trained = NetTrain[
  NetChain[{LinearLayer[12], ElementwiseLayer[\"ReLU\"], LinearLayer[1]},
           \"Input\" -> 2],
  trainData,
  MaxTrainingRounds -> 800,  BatchSize -> 64,
  LearningRate -> 0.01,      TrainingProgressReporting -> None,
  RandomSeeding -> 1];"],

  inputCell["(* pull out the weights *)
A = Normal @ NetExtract[trained, {1, \"Weights\"}];
b = Normal @ NetExtract[trained, {1, \"Biases\"}];
c = Flatten @ Normal @ NetExtract[trained, {3, \"Weights\"}];
d = First @ Flatten @ Normal @ NetExtract[trained, {3, \"Biases\"}];

(* the two forward passes *)

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
Max[Abs[fReLU /@ xs - fMaxPlus /@ xs]]"],

  text[{code["(* ~1.0 x 10^-14 *)"]}],

  imageFromFile[FileNameJoin[{figureDir, "01_forward_equivalence.png"}],
    "Figure: on three trained networks (XOR, moons, spirals), ReLU and \
max-plus forward passes agree at ~$10^{-14}$ over 1400+ inputs each. \
Same function, two different algebras.",
    0.45],

  (* ============================================================== *)

  section["12.  Decision boundary = tropical hypersurface"],

  text["The $n$ hidden units cut input space along $n$ straight crease \
lines $a_j \\cdot x + b_j = 0$. On each open region of that arrangement \
the activation pattern $\\sigma \\in \\{0, 1\\}^n$ is constant and $f$ \
is affine. The decision boundary $\\{f = 0\\}$ is the locus where the \
two convex piecewise-linear surfaces $P$ and $Q$ meet -- a tropical \
hypersurface."],

  imageFromFile[FileNameJoin[{figureDir, "16_decision_terrain.png"}],
    "Illustration. A scattering of coral dots (class +1) and blue dots \
(class -1) lives on opposite sides of a piecewise-linear crease that \
winds across the plane -- the decision boundary of a trained ReLU \
classifier.",
    0.55],

  imageFromFile[FileNameJoin[{figureDir, "02_decision_tropical_hypersurface.png"}],
    "Figure. Decision boundary = tropical hypersurface for three trained \
classifiers. Pastel patches are activation-pattern regions, thin grey \
lines are the ReLU creases, the black curve is the ReLU $\\{f = 0\\}$, \
the red dashed curve is the max-plus $\\{P = Q\\}$ -- they coincide.",
    0.55],

  (* ============================================================== *)

  section["13.  Newton polytope of the trained network"],

  text["The tropical polynomial $P$ (and similarly $Q$) of the trained \
moons network has its own Newton polytope. Only monomials whose \
lifted point sits on the upper hull realise a region in input space; \
the rest are dominated everywhere."],

  imageFromFile[FileNameJoin[{figureDir, "03_newton_polytope.png"}],
    "Figure. Newton polytope of P (top row) and Q (bottom row) for the \
trained moons classifier. The 3D upper hull (left) and the linear-\
piece partition of input space (right) are dual: 4 of P's 7 monomials \
and 5 of Q's 7 monomials realise a region.",
    0.55],

  (* ============================================================== *)

  section["14.  Linear regions scale like $n^2$"],

  text["By Zaslavsky's theorem, $n$ straight lines in general position \
in $\\mathbb{R}^2$ partition the plane into at most"],

  displayMath[
    Z[n] == 1 + n + Binomial[n, 2] == (n^2 + n + 2)/2
  ],

  text["regions, which is the Zhang-Naitzat-Lim upper bound for a 1-\
hidden-layer ReLU classifier with 2-D input. We sweep hidden width \
$n \\in \\{2, 4, 8, 16, 32, 64, 96\\}$ on two-moons (3 seeds each), \
count realised activation patterns on a $400 \\times 400$ grid, and \
compare:"],

  imageFromFile[FileNameJoin[{figureDir, "04_region_count_vs_width.png"}],
    "Figure. Empirical region count vs hidden width tracks the Zaslavsky \
bound at slope 2 on log-log axes. Trained networks realise 0.5-0.95 \
of the bound.",
    0.55],

  (* ============================================================== *)

  section["15.  Putting it together"],

  text["The whole story compresses into one table:"],

  (* table rendered as a Grid so it actually looks like a table in the
     notebook front end                                                *)
  Cell[BoxData[ToBoxes[
    Grid[{
      {"classical / ReLU view",      "tropical / max-plus view"},
      {Style["Max[0, W x + b]  per layer", 12, FontFamily -> "Courier"],
       Style["tropical polynomial in x", 12, FontFamily -> "Courier"]},
      {Style["scalar output f(x)\[Element]R", 12, FontFamily -> "Courier"],
       Style["P(x) \[Minus] Q(x), P, Q tropical polys", 12,
              FontFamily -> "Courier"]},
      {Style["decision boundary {f=0}", 12, FontFamily -> "Courier"],
       Style["tropical hypersurface {P=Q}", 12, FontFamily -> "Courier"]},
      {Style["activation crease lines", 12, FontFamily -> "Courier"],
       Style["regular subdivision of Newt(P\[CirclePlus]Q)", 12,
              FontFamily -> "Courier"]},
      {Style["linear regions vs width", 12, FontFamily -> "Courier"],
       Style["Zaslavsky 1 + n + C(n,2)", 12, FontFamily -> "Courier"]},
      {Style["mat-vec  W x + b", 12, FontFamily -> "Courier"],
       Style["tropical mat-vec  A \[CircleDot] x \[CircleTimes] b",
             12, FontFamily -> "Courier"]},
      {Style["Bellman-Ford shortest paths", 12, FontFamily -> "Courier"],
       Style["same tropical mat-vec (min-plus)", 12, FontFamily -> "Courier"]},
      {Style["PERT longest path", 12, FontFamily -> "Courier"],
       Style["tropical Kleene star", 12, FontFamily -> "Courier"]}
    },
    Background -> {None,
       Join[{Directive[White, Bold,
              Background -> RGBColor[0.18, 0.30, 0.55]]},
            ConstantArray[None, 8]]},
    Frame -> All,
    Spacings -> {2, 1.2},
    Alignment -> Left]
  ]], "Output", CellFrame -> False, CellLabel -> None],

  text["Everything that ReLU networks do, max-plus algebra ALREADY \
does. The two are the same object viewed through two different \
operator keys."],

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
    "Wolfram Community discussion on max-plus algebra: \
community.wolfram.com/groups/-/m/t/162609\n\n",
    "Companion repository: ",
    code["github.com/mthiel74/TropicalAlgebravsReLU"],
    "."
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
