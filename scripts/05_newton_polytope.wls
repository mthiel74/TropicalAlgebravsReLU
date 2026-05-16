#!/usr/bin/env wolframscript
(* Newton polytope figure for the trained XOR network.

   For a tropical polynomial P(x) = max_i (a_i . x + c_i) in two
   variables, Newt(P) = conv{a_i} is the Newton polytope and lifting
   each a_i to (a_i, c_i) in R^3 gives a regular subdivision of
   Newt(P) by the upper convex hull. That subdivision is dual to the
   linear-region decomposition of input space induced by P.
   (Maclagan-Sturmfels, Introduction to Tropical Geometry, Ch.3) *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

dataDir   = FileNameJoin[{repoRoot, "data"}];
figureDir = FileNameJoin[{repoRoot, "figures"}];

(* moons has 6 P + 6 Q monomials, a balanced nondegenerate case *)
which = "moons";

ClearAll[Global`net, Global`ds];
Get[FileNameJoin[{dataDir, which <> "_net.mx"}]];
theNet = Global`net;

dec = TropicalReLU`TropicalDecomposition[theNet];
Pmon = dec["Pmon"]; Qmon = dec["Qmon"];
Pconst = dec["Pconst"]; Qconst = dec["Qconst"];

(* Include the constant monomial (slope 0). *)
PmonAll = Join[Pmon, {{{0., 0.}, Pconst}}];
QmonAll = Join[Qmon, {{{0., 0.}, Qconst}}];

Print[which, " network:"];
Print["  P has ", Length[PmonAll], " monomials (incl. constant)"];
Print["  Q has ", Length[QmonAll], " monomials (incl. constant)"];

(* --- Panel A: 3D lift with upper-hull polyhedron --------------- *)

upperHullGraphics3D[mon_, color_, viewLabel_] := Module[
  {lifted = Append @@@ mon, hull3, faces, coords, ups, upFaces},
  If[Length[mon] < 4,
    (* degenerate 3D hull -> render as a plain polygon *)
    Return[Graphics3D[{
      {Opacity[0.3], color, Polygon[lifted]},
      {color, PointSize[0.04], Point[lifted]},
      {GrayLevel[0.7], Thin,
       Line[{#, ReplacePart[#, 3 -> 0]}] & /@ lifted}
    }, Axes -> True,
       AxesLabel -> {"a_x", "a_y", "c"},
       PlotLabel -> Style[viewLabel, 15],
       BoxRatios -> {1, 1, 0.7},
       ViewPoint -> {1.6, -2.4, 1.4},
       ImageSize -> 460]
    ];
  ];
  hull3 = ConvexHullMesh[lifted];
  coords = MeshCoordinates[hull3];
  faces = MeshCells[hull3, 2];
  ups = Function[face,
     Module[{ids = face[[1]], v1, v2, n},
       v1 = coords[[ids[[2]]]] - coords[[ids[[1]]]];
       v2 = coords[[ids[[3]]]] - coords[[ids[[1]]]];
       n = Cross[v1, v2];
       n[[3]] > 0]] /@ faces;
  upFaces = Pick[faces, ups];
  Graphics3D[{
    {Opacity[0.30], color,
     GraphicsComplex[coords, upFaces]},
    {Black, AbsoluteThickness[1],
     GraphicsComplex[coords, Line /@
       (Partition[Append[#[[1]], #[[1, 1]]], 2, 1] & /@ upFaces)]},
    {color, PointSize[0.035], Point[lifted]},
    {GrayLevel[0.7], Thin,
     Line[{#, ReplacePart[#, 3 -> 0]}] & /@ lifted},
    {GrayLevel[0.5], Dotted, Line[ReplacePart[#, 3 -> 0] & /@ lifted]}
  },
    Axes -> True,
    AxesLabel -> {Style["a_x", 12], Style["a_y", 12], Style["c", 12]},
    PlotLabel -> Style[viewLabel, 15],
    BoxRatios -> {1, 1, 0.7},
    ViewPoint -> {1.6, -2.4, 1.4},
    ImageSize -> 460,
    PlotRange -> All
  ]
];

panelA1 = upperHullGraphics3D[PmonAll, ColorData[97][1],
  "lifted monomials of P  (upper hull)"];
panelA2 = upperHullGraphics3D[QmonAll, ColorData[97][2],
  "lifted monomials of Q  (upper hull)"];

(* --- Panel B: linear regions of P and Q on the input plane ------ *)

argmaxOnGrid[mon_, dom_, nGrid_] := Module[
  {xs, ys, pts, vals, idx, img},
  xs = Subdivide[dom[[1, 1]], dom[[1, 2]], nGrid - 1];
  ys = Subdivide[dom[[2, 1]], dom[[2, 2]], nGrid - 1];
  pts = Flatten[Table[{x, y}, {x, xs}, {y, ys}], 1];
  vals = Table[m[[1]] . p + m[[2]], {m, mon}, {p, pts}];
  idx  = First @ Ordering[#, -1] & /@ Transpose[vals];
  ArrayReshape[idx, {nGrid, nGrid}]
];

dom = {{-2.2, 2.2}, {-2.2, 2.2}};
gridN = 240;

Pimg = argmaxOnGrid[PmonAll, dom, gridN];
Qimg = argmaxOnGrid[QmonAll, dom, gridN];

nP = Length[DeleteDuplicates[Flatten[Pimg]]];
nQ = Length[DeleteDuplicates[Flatten[Qimg]]];
Print["P has ", nP, " visible linear pieces on the plot rectangle"];
Print["Q has ", nQ, " visible linear pieces on the plot rectangle"];

paletteFor[mon_, base_] := Module[{n = Length[mon]},
  Table[Lighter[base, 0.25 + 0.55 (k - 1)/Max[1, n - 1]], {k, n}]
];

panelB1 = ArrayPlot[Transpose[Pimg], DataReversed -> True,
  ColorRules -> Thread[Range[Length[PmonAll]] ->
                       paletteFor[PmonAll, ColorData[97][1]]],
  PlotRange -> dom, DataRange -> dom, Frame -> True,
  FrameLabel -> {"x_1", "x_2"},
  PlotLabel -> Style[Row[{"linear pieces of P:  ", nP, " of ",
                          Length[PmonAll]}], 15],
  AspectRatio -> 1, ImageSize -> 460];

panelB2 = ArrayPlot[Transpose[Qimg], DataReversed -> True,
  ColorRules -> Thread[Range[Length[QmonAll]] ->
                       paletteFor[QmonAll, ColorData[97][2]]],
  PlotRange -> dom, DataRange -> dom, Frame -> True,
  FrameLabel -> {"x_1", "x_2"},
  PlotLabel -> Style[Row[{"linear pieces of Q:  ", nQ, " of ",
                          Length[QmonAll]}], 15],
  AspectRatio -> 1, ImageSize -> 460];

(* --- assemble ---------------------------------------------------- *)

fig = Grid[{
  {panelA1, panelB1},
  {panelA2, panelB2}
}, Spacings -> {2, 2}];

fileOut = FileNameJoin[{figureDir, "03_newton_polytope.png"}];
Export[fileOut, fig, ImageResolution -> 220];
Print["wrote ", fileOut];
