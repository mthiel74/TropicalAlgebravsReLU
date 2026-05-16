#!/usr/bin/env wolframscript
(* Train tiny 2 -> n -> 1 ReLU classifiers on three 2D synthetic datasets:
   XOR (Gaussian blobs), two-moons, two-spirals.
   Save the resulting weights as .mx files in data/. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

SeedRandom[7];

makeXOR[nPerClass_: 150, sigma_: 0.30] := Module[{centers, pts, labels},
  centers = {{1, 1}, {-1, -1}, {1, -1}, {-1, 1}};
  labels = {1, 1, -1, -1};
  pts = Flatten[Table[
    {centers[[k]] + RandomVariate[NormalDistribution[0, sigma], 2], labels[[k]]},
    {k, 4}, {nPerClass}], 1];
  pts
];

makeMoons[nPerClass_: 200, noise_: 0.15] := Module[{theta, top, bot},
  theta = Subdivide[0, Pi, nPerClass - 1];
  top  = Transpose[{Cos[theta], Sin[theta]}]
         + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
  bot  = Transpose[{1 - Cos[theta], -Sin[theta] - 0.5}]
         + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
  Join[Thread[{top, 1}], Thread[{bot, -1}]]
];

makeSpirals[nPerClass_: 200, turns_: 1.25, noise_: 0.08] := Module[{t, r, ang, p1, p2},
  t = Subdivide[0.05, 1, nPerClass - 1];
  r = 2 t;
  ang = 2 Pi turns t;
  p1 = Transpose[{r Cos[ang], r Sin[ang]}]
        + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
  p2 = -p1 + RandomVariate[NormalDistribution[0, noise], {nPerClass, 2}];
  Join[Thread[{p1, 1}], Thread[{p2, -1}]]
];

(* Feed targets as 1-vectors so they match the shape of the LinearLayer[1] output. *)
toAssoc[pts_] := (#[[1]] -> {N[#[[2]]]}) & /@ pts;

(* Train a 2 -> n -> 1 ReLU regressor on +/-1 targets.
   We use MeanSquared loss so the output is real-valued; the decision
   rule is sign(f). *)

trainNet[data_, n_, rounds_: 1500, lr_: 0.01] := Module[
  {netSpec, trained, A, b, c, d, X, Y, predTrain, acc},
  netSpec = NetChain[
    {LinearLayer[n], ElementwiseLayer["ReLU"], LinearLayer[1]},
    "Input" -> 2
  ];
  trained = NetTrain[
    netSpec, data,
    MaxTrainingRounds -> rounds,
    BatchSize -> 64,
    LearningRate -> lr,
    TrainingProgressReporting -> None,
    RandomSeeding -> 1
  ];
  A = Normal @ NetExtract[trained, {1, "Weights"}];
  b = Normal @ NetExtract[trained, {1, "Biases"}];
  c = Flatten @ Normal @ NetExtract[trained, {3, "Weights"}];
  d = First @ Flatten @ Normal @ NetExtract[trained, {3, "Biases"}];
  X = data[[All, 1]];
  Y = Flatten[data[[All, 2]]];           (* unwrap 1-vector targets *)
  predTrain = Sign[TropicalReLU`ReLUForward[<|"A" -> A, "b" -> b, "c" -> c, "d" -> d|>, X]];
  acc = N[Mean[MapThread[Boole[#1 == #2] &, {predTrain, Y}]]];
  <|"A" -> A, "b" -> b, "c" -> c, "d" -> d, "Net" -> trained, "TrainAcc" -> acc|>
];

dataDir = FileNameJoin[{repoRoot, "data"}];
If[!DirectoryQ[dataDir], CreateDirectory[dataDir]];

datasets = <|
  "xor"     -> makeXOR[],
  "moons"   -> makeMoons[],
  "spirals" -> makeSpirals[]
|>;

widths = <|"xor" -> 8, "moons" -> 12, "spirals" -> 32|>;

Do[
  Module[{ds = datasets[name], asc, net, file, sample},
    Print["[", name, "] ", Length[ds], " points, training width ", widths[name]];
    asc = toAssoc[ds];
    net = trainNet[asc, widths[name]];
    Print["[", name, "] train accuracy = ", net["TrainAcc"]];
    file = FileNameJoin[{dataDir, name <> "_net.mx"}];
    DumpSave[file, {net, ds}];
    Print["[", name, "] saved -> ", file];
    sample = ds[[;; ;; Quotient[Length[ds], 4]]];
    Print["[", name, "] sample points -> ", sample[[All, 1]]];
  ],
  {name, Keys[datasets]}
];

Print["done."];
