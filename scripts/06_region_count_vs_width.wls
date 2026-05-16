#!/usr/bin/env wolframscript
(* Linear regions of trained 1-hidden-layer ReLU classifiers,
   as a function of hidden width n.

   For a 1-hidden-layer ReLU MLP on 2D input, the hidden units cut the
   input plane along n straight "crease" lines a_j . x + b_j = 0.
   By Zaslavsky's theorem, n lines in general position in R^2 partition
   it into at most
        Z(n) = 1 + n + C(n,2) = (n^2 + n + 2) / 2
   open regions. Each region carries a unique activation pattern, so

        #linear regions of f  <=  Z(n).

   We train networks at n in {2, 4, 8, 16, 32, 64, 96}, count realised
   activation patterns on a fine grid over [-3, 3]^2, and compare to
   the Zaslavsky bound.

   Each width is repeated R = 3 times with different random seeds so
   we can see the spread. Dataset: two-moons. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

figureDir = FileNameJoin[{repoRoot, "figures"}];
If[!DirectoryQ[figureDir], CreateDirectory[figureDir]];

(* --- data: two moons --------------------------------------------- *)
makeMoons[nPerClass_: 200, noise_: 0.15, seed_: 1] := (
  SeedRandom[seed];
  Module[{theta, top, bot},
    theta = Subdivide[0, Pi, nPerClass - 1];
    top  = Transpose[{Cos[theta], Sin[theta]}]
           + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
    bot  = Transpose[{1 - Cos[theta], -Sin[theta] - 0.5}]
           + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
    Join[Thread[{top, 1}], Thread[{bot, -1}]]
  ]
);

trainOne[n_, seed_] := Module[{ds, asc, netSpec, trained, A, b, c, d},
  ds = makeMoons[200, 0.15, seed];
  asc = (#[[1]] -> {N[#[[2]]]}) & /@ ds;
  netSpec = NetChain[
    {LinearLayer[n], ElementwiseLayer["ReLU"], LinearLayer[1]},
    "Input" -> 2
  ];
  trained = NetTrain[netSpec, asc,
    MaxTrainingRounds -> 800, BatchSize -> 64, LearningRate -> 0.01,
    TrainingProgressReporting -> None, RandomSeeding -> seed];
  A = Normal @ NetExtract[trained, {1, "Weights"}];
  b = Normal @ NetExtract[trained, {1, "Biases"}];
  c = Flatten @ Normal @ NetExtract[trained, {3, "Weights"}];
  d = First @ Flatten @ Normal @ NetExtract[trained, {3, "Biases"}];
  <|"A" -> A, "b" -> b, "c" -> c, "d" -> d|>
];

countRegions[net_, dom_: {{-3., 3.}, {-3., 3.}}, gridN_: 400] := Module[
  {xs, ys, pts, sigmas},
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], gridN - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], gridN - 1];
  pts = Flatten[Table[{x, y}, {x, xs}, {y, ys}], 1];
  sigmas = TropicalReLU`ActivationPattern[net, #] & /@ pts;
  Length[DeleteDuplicates[sigmas]]
];

zaslavsky[n_] := 1 + n + Binomial[n, 2];

widths = {2, 4, 8, 16, 32, 64, 96};
seeds  = {1, 2, 3};

results = {};
Do[
  Do[
    Module[{net, regs, bound = zaslavsky[n]},
      net = trainOne[n, seed];
      regs = countRegions[net];
      AppendTo[results, <|"n" -> n, "seed" -> seed,
                         "regions" -> regs, "bound" -> bound|>];
      Print["n = ", n, ", seed = ", seed, ", regions = ", regs,
            ",  Zaslavsky bound = ", bound,
            ",  ratio = ", N[regs / bound]];
    ],
    {seed, seeds}
  ],
  {n, widths}
];

(* group by width *)
byWidth = GroupBy[results, #["n"] &];
means   = Mean[#["regions"] & /@ byWidth[#]] & /@ widths;
mins    = Min[#["regions"] & /@ byWidth[#]] & /@ widths;
maxs    = Max[#["regions"] & /@ byWidth[#]] & /@ widths;
bounds  = zaslavsky /@ widths;

(* save raw numbers for the post *)
dataFile = FileNameJoin[{repoRoot, "data", "region_count_vs_width.m"}];
Put[<|"widths" -> widths, "means" -> means,
      "mins" -> mins, "maxs" -> maxs, "bounds" -> bounds,
      "seeds" -> seeds, "raw" -> results|>, dataFile];
Print["wrote ", dataFile];

rawXY = {#["n"], #["regions"]} & /@ results;

fig = ListLogLogPlot[
  {Transpose[{widths, means}], rawXY, Transpose[{widths, bounds}]},
  PlotStyle -> {
    Directive[ColorData[97][1], PointSize[0.018], Thick],
    Directive[ColorData[97][1], Opacity[0.45], PointSize[0.012]],
    Directive[ColorData[97][2], PointSize[0.012], Dashed, Thick]
  },
  Joined -> {True, False, True},
  Frame -> True,
  FrameLabel -> {Style["hidden width n", 14],
                  Style["number of linear regions", 14]},
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin],
  PlotLabel -> Style[
    "Linear regions of a 2 -> n -> 1 ReLU classifier vs width (moons)", 16],
  PlotLegends -> Placed[
    LineLegend[
      {Directive[ColorData[97][1], Thick],
       Directive[ColorData[97][1], Opacity[0.45], PointSize[0.012]],
       Directive[ColorData[97][2], Dashed, Thick]},
      {"empirical (mean over 3 seeds)",
       "individual runs",
       "Zaslavsky upper bound  1 + n + C(n,2)"},
      LegendFunction -> "Frame"],
    {0.30, 0.83}],
  ImageSize -> 820
];

fileOut = FileNameJoin[{figureDir, "04_region_count_vs_width.png"}];
Export[fileOut, fig, ImageResolution -> 200];
Print["wrote ", fileOut];
