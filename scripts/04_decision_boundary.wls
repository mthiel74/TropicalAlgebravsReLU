#!/usr/bin/env wolframscript
(* Decision boundary as tropical hypersurface.
   For each trained classifier produce a panel with:
     - light-grey hidden-unit "crease" lines a_j . x + b_j = 0
     - filled patches colour-coded by activation pattern sigma(x)
     - black contour {f_ReLU = 0} (classical decision boundary)
     - red dashed contour {P_maxplus = Q_maxplus} (tropical hypersurface) *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

dataDir   = FileNameJoin[{repoRoot, "data"}];
figureDir = FileNameJoin[{repoRoot, "figures"}];
If[!DirectoryQ[figureDir], CreateDirectory[figureDir]];

(* numeric helpers using compiled funcs would be nicer but for clarity we
   call the package directly on points. *)

domainFor[name_] := Switch[name,
  "xor",     {{-2.2, 2.2}, {-2.2, 2.2}},
  "moons",   {{-1.7, 2.7}, {-2.0, 2.0}},
  "spirals", {{-2.4, 2.4}, {-2.4, 2.4}}
];

makePanel[name_] := Module[
  {theNet, theDs, dom, gridN, xs, ys, pts, sigma, sigmaIdx, indexImg,
   patternBgFn, creaseLines, contourReLU, contourMaxPlus, dataPts, panel,
   nUnits, nRegions, pmonStr, qmonStr},
  ClearAll[Global`net, Global`ds];
  Get[FileNameJoin[{dataDir, name <> "_net.mx"}]];
  theNet = Global`net;
  theDs  = Global`ds;
  dom = domainFor[name];

  nUnits = Length[theNet["b"]];

  (* --- activation pattern raster ---------------------------------- *)
  gridN = 320;
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], gridN - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], gridN - 1];
  pts = Flatten[Table[{x, y}, {x, xs}, {y, ys}], 1];
  sigma = TropicalReLU`ActivationPattern[theNet, #] & /@ pts;
  (* hash each binary pattern to an integer 0..k-1, k = #distinct *)
  Module[{patterns = DeleteDuplicates[sigma], idxAssoc},
    idxAssoc = AssociationThread[patterns, Range[Length[patterns]]];
    sigmaIdx = idxAssoc /@ sigma;
    nRegions = Length[patterns];
  ];
  (* shape into image, columns = x, rows = y; flip vertically for ArrayPlot *)
  indexImg = ArrayReshape[sigmaIdx, {gridN, gridN}];
  (* Build a pastel colour list for the regions. *)
  patternBgFn = With[{cols = Table[
       Hue[(k - 1)/nRegions, 0.22, 1.0], {k, nRegions}]},
    ArrayPlot[Transpose[indexImg],
      DataReversed -> True,
      ColorRules ->
        Thread[Range[nRegions] -> cols],
      PlotRange -> dom, Frame -> False, AspectRatio -> 1,
      DataRange -> dom]
  ];

  (* --- crease lines ----------------------------------------------- *)
  creaseLines = Show @@ Table[
    With[{aj = theNet["A"][[j]], bj = theNet["b"][[j]]},
      ContourPlot[aj.{x1, x2} + bj == 0,
        {x1, dom[[1, 1]], dom[[1, 2]]},
        {x2, dom[[2, 1]], dom[[2, 2]]},
        ContourStyle -> Directive[GrayLevel[0.55], Thickness[0.0015]],
        PlotPoints -> 25]
    ],
    {j, nUnits}
  ];

  (* --- ReLU decision boundary ------------------------------------- *)
  contourReLU = ContourPlot[
    Evaluate[TropicalReLU`ReLUForward[theNet, {x1, x2}]] == 0,
    {x1, dom[[1, 1]], dom[[1, 2]]},
    {x2, dom[[2, 1]], dom[[2, 2]]},
    ContourStyle -> Directive[Black, Thickness[0.005]],
    PlotPoints -> 60];

  (* --- tropical hypersurface P = Q  (red dashed) ------------------ *)
  contourMaxPlus = ContourPlot[
    Evaluate[TropicalReLU`MaxPlusForward[theNet, {x1, x2}]] == 0,
    {x1, dom[[1, 1]], dom[[1, 2]]},
    {x2, dom[[2, 1]], dom[[2, 2]]},
    ContourStyle -> Directive[Red, AbsoluteDashing[{8, 8}], Thickness[0.004]],
    PlotPoints -> 60];

  (* --- training points -------------------------------------------- *)
  dataPts = Graphics[{
    {ColorData[97][1], PointSize[0.005],
     Point[Select[theDs, #[[2]] == 1 &][[All, 1]]]},
    {ColorData[97][2], PointSize[0.005],
     Point[Select[theDs, #[[2]] == -1 &][[All, 1]]]}
  }];

  panel = Show[
    patternBgFn,
    creaseLines,
    contourReLU,    (* black ReLU boundary *)
    dataPts,
    contourMaxPlus, (* red dashed max-plus boundary, on top to show overlay *)
    PlotRange -> dom,
    AspectRatio -> 1,
    Frame -> True,
    PlotLabel ->
      Style[Row[{name, ": ", nUnits, " hidden units, ", nRegions,
                 " linear regions"}], 18, FontFamily -> "Helvetica"],
    LabelStyle -> Directive[Black, 14, FontFamily -> "Helvetica"],
    ImageSize -> 560
  ];

  Print["[", name, "] hidden = ", nUnits, ", regions on grid = ", nRegions];
  panel
];

panels = makePanel /@ {"xor", "moons", "spirals"};

legend = Graphics[{
  GrayLevel[0.55], Thickness[0.005], Line[{{0, 0}, {0.6, 0}}],
  Black,           Text[Style["crease lines a_j.x + b_j = 0", 14], {0.7, 0}, {-1, 0}],
  Black,           Thickness[0.005], Line[{{0, -0.5}, {0.6, -0.5}}],
  Black,           Text[Style["ReLU decision boundary  {f = 0}", 14], {0.7, -0.5}, {-1, 0}],
  Red, AbsoluteDashing[{8, 8}], Thickness[0.005], Line[{{0, -1.0}, {0.6, -1.0}}],
  Black,           Text[Style["tropical hypersurface  {P = Q}", 14], {0.7, -1.0}, {-1, 0}]
}, ImageSize -> 360, PlotRange -> {{-0.1, 4.2}, {-1.4, 0.4}}];

fig = Column[{
  GraphicsRow[panels, ImageSize -> 1700, Spacings -> 20],
  legend
}, Alignment -> Center, Spacings -> 0.5];
fileOut = FileNameJoin[{figureDir, "02_decision_tropical_hypersurface.png"}];
Export[fileOut, fig, ImageResolution -> 220];
Print["wrote ", fileOut];
