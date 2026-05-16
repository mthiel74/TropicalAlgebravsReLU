#!/usr/bin/env wolframscript
(* Build the Wolfram Community post notebook for
   "Tropical Algebra of ReLU Networks". The notebook is generated
   programmatically so it can be regenerated from sources. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
figureDir = FileNameJoin[{repoRoot, "figures"}];

(* Helper cell constructors *)

titleCell[txt_]    := Cell[txt, "Title"];
sectionCell[txt_]  := Cell[txt, "Section"];
subsectionCell[txt_]:= Cell[txt, "Subsection"];
textCell[txt_]     := Cell[TextData[txt], "Text"];
codeCell[expr_]    := Cell[BoxData[ToBoxes[Defer[expr]]], "Input"];
codeCellRaw[txt_]  := Cell[txt, "Input"];

img[path_, caption_] := Module[{img},
  img = If[FileExistsQ[path], Import[path], Graphics[{Text["missing: " <> path]}]];
  Cell[BoxData[ToBoxes[Magnify[img, 0.6]]], "Output", CellLabel -> "Figure"]
];

(* The cells *)

nb = Notebook[{
  titleCell["Tropical Algebra of ReLU Networks"],
  textCell[{
    "A ", StyleBox["ReLU neural network is literally a tropical rational map",
                   Bold],
    ". This post takes that statement seriously: we train a tiny 2D classifier ",
    "in Wolfram Language, rewrite its forward pass using nothing but ",
    StyleBox["max", Bold], " and ",
    StyleBox["+", Bold], ", and show that its decision boundary is a ",
    StyleBox["tropical hypersurface", Bold],
    " - a finite, piecewise-linear arrangement whose pieces we make explicit. ",
    "We also reproduce the n^2 linear-region scaling predicted by ",
    "Zhang, Naitzat, and Lim (ICML 2018) and by Zaslavsky's classical bound ",
    "on hyperplane arrangements."
  }],

  sectionCell["1.  Why ReLU is tropical"],
  textCell[{
    "The tropical semiring is the set of real numbers (plus -infinity) ",
    "with two operations:\n\n",
    "    a (+) b := max(a, b)        (tropical addition)\n",
    "    a (x) b := a + b            (tropical multiplication)\n\n",
    "The neutral element for (+) is -infinity, and the neutral element ",
    "for (x) is 0. The ReLU activation is then simply\n\n",
    "    ReLU(x) = max(0, x) = 0 (+) x,\n\n",
    "i.e. the tropical addition with the (+) neutral element 0 swapped ",
    "in for the (x) neutral element. An affine map followed by ReLU,\n\n",
    "    h(x) = max(0, a . x + b),\n\n",
    "is a degree-1 tropical polynomial in (x_1, x_2). A 1-hidden-layer ",
    "ReLU classifier\n\n",
    "    f(x) = sum_j c_j max(0, a_j . x + b_j) + d\n\n",
    "splits along c_j > 0 vs c_j < 0 into P(x) - Q(x), with both ",
    "P and Q convex piecewise-linear (i.e. tropical polynomials). ",
    "f is therefore a tropical rational map, and the decision boundary ",
    "{f = 0} is the tropical hypersurface {P = Q}."
  }],

  sectionCell["2.  Train a tiny ReLU classifier"],
  textCell[{"We use two moons as the dataset and a 2 -> 12 -> 1 ReLU MLP."}],
  codeCellRaw[
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
data = makeMoons[200];
trainData = (#[[1]] -> {N[#[[2]]]}) & /@ data;

netSpec = NetChain[
  {LinearLayer[12], ElementwiseLayer[\"ReLU\"], LinearLayer[1]},
  \"Input\" -> 2];

trained = NetTrain[netSpec, trainData,
  MaxTrainingRounds -> 800, BatchSize -> 64, LearningRate -> 0.01,
  TrainingProgressReporting -> None, RandomSeeding -> 1];

A = Normal @ NetExtract[trained, {1, \"Weights\"}];
b = Normal @ NetExtract[trained, {1, \"Biases\"}];
c = Flatten @ Normal @ NetExtract[trained, {3, \"Weights\"}];
d = First @ Flatten @ Normal @ NetExtract[trained, {3, \"Biases\"}];
{Dimensions[A], Dimensions[b], Dimensions[c]}"
  ],

  sectionCell["3.  The two forward passes"],
  textCell[{
    "The classical (ReLU, real arithmetic) forward pass is direct. ",
    "The max-plus forward pass pulls every positive output weight inside ",
    "a max, splits negative weights into Q, and never uses subtraction ",
    "until the final P - Q step:"
  }],
  codeCellRaw[
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
xs   = RandomReal[{-2, 2}, {1000, 2}];
yR   = fReLU /@ xs;
yMP  = fMaxPlus /@ xs;
Max[Abs[yR - yMP]]"
  ],
  textCell[{
    "The two evaluators agree at machine precision (~10^-15). They are ",
    "the same function, written in two different algebras."
  }],

  sectionCell["4.  Forward-pass equivalence on the three datasets"],
  img[FileNameJoin[{figureDir, "01_forward_equivalence.png"}],
      "Forward-pass equivalence: ReLU vs max-plus."],

  sectionCell["5.  Decision boundary as a tropical hypersurface"],
  textCell[{
    "The hidden units cut the input plane along n straight lines ",
    "a_j . x + b_j = 0 (the ReLU 'creases'). On each open region of the ",
    "resulting arrangement, the activation pattern sigma in {0, 1}^n ",
    "is constant, and f is affine. The decision boundary {f = 0} is a ",
    "piecewise-linear curve that snaps to the crease arrangement - it is ",
    "the tropical hypersurface {P = Q}."
  }],
  img[FileNameJoin[{figureDir, "02_decision_tropical_hypersurface.png"}],
      "Decision boundary = tropical hypersurface."],

  sectionCell["6.  Newton polytope of a tropical polynomial"],
  textCell[{
    "Every tropical polynomial P(x) = max_i (a_i . x + c_i) has a ",
    "Newton polytope Newt(P) = conv{a_i}, and the upper convex hull of ",
    "{(a_i, c_i)} in R^3 induces a regular subdivision of Newt(P). That ",
    "subdivision is dual to the linear-region decomposition of input ",
    "space induced by P. Monomials whose lifted point lies in the interior ",
    "of the upper envelope are dominated by other terms and never realise ",
    "the max - they contribute no region."
  }],
  img[FileNameJoin[{figureDir, "03_newton_polytope.png"}],
      "Newton polytopes of P and Q for the moons network."],

  sectionCell["7.  Linear regions scale like n^2"],
  textCell[{
    "n lines in general position in R^2 partition the plane into at most ",
    "Z(n) = 1 + n + C(n, 2) open regions (Zaslavsky). Training a 2 -> n -> 1 ",
    "ReLU classifier and counting distinct activation patterns on a fine ",
    "grid gives the empirical scaling below. The trained network sits ",
    "at ~0.5-0.95 of the bound and matches its asymptotic slope exactly."
  }],
  img[FileNameJoin[{figureDir, "04_region_count_vs_width.png"}],
      "Linear regions vs hidden width n."],

  sectionCell["Reference"],
  textCell[{
    "Liwen Zhang, Gregory Naitzat, Lek-Heng Lim. ",
    StyleBox["Tropical Geometry of Deep Neural Networks", Italic],
    ". ICML 2018. arXiv:1805.07091."
  }]

}, WindowSize -> {1000, 800}, WindowMargins -> {{Automatic, Automatic}, {Automatic, Automatic}},
   FrontEndVersion -> "15.0 for Mac OS X ARM (64-bit)",
   StyleDefinitions -> "Default.nb"];

out = FileNameJoin[{repoRoot, "community", "Post.nb"}];
Export[out, nb];
Print["wrote ", out];
