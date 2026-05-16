#!/usr/bin/env wolframscript
(* Verify, on each trained classifier, that the ReLU forward pass and
   the max-plus forward pass agree to machine precision. Produce a
   figure showing the diagonal scatter and the error magnitudes. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

dataDir   = FileNameJoin[{repoRoot, "data"}];
figureDir = FileNameJoin[{repoRoot, "figures"}];
If[!DirectoryQ[figureDir], CreateDirectory[figureDir]];

SeedRandom[2026];

panels = {};
errs   = <||>;

runOne[name_String] := Module[{theNet, theDs, X, yRelu, yMax, err, panel},
  ClearAll[Global`net, Global`ds];
  Get[FileNameJoin[{dataDir, name <> "_net.mx"}]];
  theNet = Global`net;
  theDs  = Global`ds;
  X = Join[
    RandomReal[{-2.5, 2.5}, {1024, 2}],
    theDs[[All, 1]]
  ];
  yRelu = TropicalReLU`ReLUForward[theNet, X];
  yMax  = TropicalReLU`MaxPlusForward[theNet, X];
  err   = Max[Abs[yRelu - yMax]];
  errs[name] = err;
  Print[name, ": max |ReLU - max-plus| = ", err, ",  on ", Length[X], " points"];
  panel = ListPlot[
    Transpose[{yRelu, yMax}],
    AspectRatio -> 1,
    Frame -> True,
    FrameLabel -> {"ReLU forward f(x)", "max-plus  P(x) - Q(x)"},
    PlotLabel -> Row[{name, ", max abs err = ", ScientificForm[err, 2]}],
    PlotStyle -> Directive[Opacity[0.4], PointSize[0.005], ColorData[97][1]],
    Epilog -> {Red, Dashed,
      Line[{{Min[yRelu], Min[yRelu]}, {Max[yRelu], Max[yRelu]}}]},
    ImageSize -> 320
  ];
  AppendTo[panels, panel];
];

Do[runOne[name], {name, {"xor", "moons", "spirals"}}];

fig = GraphicsRow[panels, ImageSize -> 1000, Spacings -> 10];
fileOut = FileNameJoin[{figureDir, "01_forward_equivalence.png"}];
Export[fileOut, fig, ImageResolution -> 220];
Print["wrote ", fileOut];
Print["summary errors: ", Normal[errs]];
