#!/usr/bin/env wolframscript
(* Worked examples of the MaxPlus package, exported as figures 05-09.

   Each example is small and complete; the figures are reused in the
   Wolfram Community notebook to illustrate the algebra before any
   neural-network material appears. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "MaxPlus.wl"}]];
Needs["MaxPlus`"];

figureDir = FileNameJoin[{repoRoot, "figures"}];
If[!DirectoryQ[figureDir], CreateDirectory[figureDir]];

(* =====================================================================
   Figure 05 — Univariate tropical polynomial and its Newton polygon
   ===================================================================== *)

(* p(x) = max(2, 2x + 1, x + 2, 3x - 2) — a degree-3 tropical polynomial
   with four monomials of slopes 0, 2, 1, 3 and intercepts 2, 1, 2, -2. *)

p1Exps   = {{0}, {2}, {1}, {3}};
p1Coeffs = {2, 1, 2, -2};
p1       = TropicalPolynomial[p1Exps, p1Coeffs];

xs    = Subdivide[-1.5, 2.5, 400];
pvals = TropicalEval[p1, #] & /@ Transpose[{xs}];

(* find breakpoints (corners): pixels where the argmax changes *)
argmaxAt = First @ Ordering[p1Exps . #1 + p1Coeffs, -1] & /@ Transpose[{xs}];
corners = Position[Differences[argmaxAt], Except[0], {1}, Heads -> False][[All, 1]];
cornerXs = xs[[corners]];
cornerYs = pvals[[corners]];

colour = Table[Hue[(k - 1)/Length[p1Coeffs], 0.7, 0.85], {k, Length[p1Coeffs]}];

(* individual monomial lines *)
monLines = MapIndexed[
  Plot[#1[[1]] x + p1Coeffs[[#2[[1]]]],
    {x, -1.5, 2.5},
    PlotStyle -> Directive[colour[[#2[[1]]]], Thickness[0.003], Dashed]] &,
  p1Exps];

envelopePlot = Plot[
  Max[2, 2 x + 1, x + 2, 3 x - 2], {x, -1.5, 2.5},
  PlotStyle -> Directive[Black, Thickness[0.006]],
  Frame -> True, FrameLabel -> {Style["x", 14], Style["p(x)", 14]},
  PlotLabel -> Style["p(x) = max(2, 2x + 1, x + 2, 3x - 2)   (4 monomials, 3 corners)", 15],
  ImageSize -> 520, GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin]];

envelopeWithCorners = Show[
  monLines,
  envelopePlot,
  Graphics[{Black, AbsolutePointSize[8], Point[Transpose[{cornerXs, cornerYs}]]}]];

(* --- right panel: Newton polygon (1D = upper hull of (slope, intercept)) --- *)

ptsLift = MapThread[{#1[[1]], #2} &, {p1Exps, p1Coeffs}];
(* upper hull in 2D: ConvexHullMesh then keep the top boundary *)
hull   = ConvexHullMesh[ptsLift];
hullCoords = MeshCoordinates[hull];
(* upper hull = path of vertices with largest y for each x range *)
sortByX = SortBy[hullCoords, First];
upperOnly = Module[{seq = {First[sortByX]}, last},
  Do[
    last = Last[seq];
    If[sortByX[[k, 1]] > last[[1]] || sortByX[[k, 2]] > last[[2]],
       AppendTo[seq, sortByX[[k]]]],
    {k, 2, Length[sortByX]}];
  (* the proper upper hull *)
  Module[{stack = {sortByX[[1]]}, p},
    Do[
      p = sortByX[[i]];
      While[Length[stack] >= 2 &&
        With[{a = stack[[-2]], b = stack[[-1]]},
          (b[[1]] - a[[1]]) (p[[2]] - a[[2]]) -
          (b[[2]] - a[[2]]) (p[[1]] - a[[1]]) >= 0],
        stack = Most[stack]];
      AppendTo[stack, p],
      {i, 2, Length[sortByX]}];
    stack
  ]
];

newtonFig = Graphics[{
  (* drop-lines *)
  GrayLevel[0.6], Thin,
  Line[{#, {#[[1]], -3.5}}] & /@ ptsLift,
  (* base line at y = -3.5 to show the slope axis *)
  Black, Thick, Line[{{-0.3, -3.5}, {3.3, -3.5}}],
  (* hull edges *)
  ColorData[97][1], Thick,
  Line[upperOnly],
  (* monomial points *)
  MapIndexed[
    {colour[[#2[[1]]]], AbsolutePointSize[12], Point[#1],
     Black, Text[
       Style[Row[{"a=", #1[[1]], ", c=", #1[[2]]}], 10],
       #1, {-1.2, -0.8}]} &,
    ptsLift]
  },
  Frame -> True,
  FrameLabel -> {Style["slope a", 14], Style["intercept c", 14]},
  PlotLabel -> Style["Newton polygon (upper hull): vertices realise pieces of p(x)", 15],
  PlotRange -> {{-0.3, 3.3}, {-3.5, 3.5}},
  AspectRatio -> 0.7, ImageSize -> 520,
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin]];

fig05 = GraphicsRow[{envelopeWithCorners, newtonFig},
  ImageSize -> 1200, Spacings -> 40];
Export[FileNameJoin[{figureDir, "05_tropical_poly_1d.png"}], fig05,
  ImageResolution -> 200];
Print["wrote figures/05_tropical_poly_1d.png"];

(* =====================================================================
   Figure 06 — Bivariate tropical polynomial and its tropical curve
   ===================================================================== *)

(* Classic tropical conic: p(x, y) = max(0, x, y, x + y).
   Its tropical curve {at least two terms tie} is a tropical "line"
   with three rays meeting at the origin. *)

p2Exps   = {{0, 0}, {1, 0}, {0, 1}, {1, 1}};
p2Coeffs = {0, 0, 0, -1};   (* slight bias on x+y so the curve is nontrivial *)
p2       = TropicalPolynomial[p2Exps, p2Coeffs];

dom    = {{-2, 2}, {-2, 2}};
panelL = TropicalRegionPlot[p2, {x, y}, dom];
panelL = Show[panelL,
  PlotLabel -> Style["regions of  max(0, x, y, x + y - 1)", 15],
  FrameLabel -> {Style["x", 14], Style["y", 14]}];

(* Newton polytope = convex hull of the four exponents *)
newt2 = Graphics[{
  LightYellow,
  Polygon[{{0, 0}, {1, 0}, {1, 1}, {0, 1}}],
  Black, Thick,
  Line[{{0, 0}, {1, 0}, {1, 1}, {0, 1}, {0, 0}}],
  AbsolutePointSize[12], ColorData[97][1],
  Point /@ {{0, 0}, {1, 0}, {0, 1}, {1, 1}},
  Black,
  MapIndexed[Text[
    Style[Row[{"c = ", p2Coeffs[[#2[[1]]]]}], 11],
    #1, {-1.1, -1.1}] &, p2Exps]
  },
  Frame -> True,
  FrameLabel -> {Style["exponent x", 14], Style["exponent y", 14]},
  PlotLabel -> Style["Newton polytope Newt(p) = conv{exponents}", 15],
  AspectRatio -> 1,
  PlotRange -> {{-0.3, 1.3}, {-0.3, 1.3}},
  ImageSize -> 460,
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin]];

fig06 = GraphicsRow[{panelL, newt2},
  ImageSize -> 1100, Spacings -> 40];
Export[FileNameJoin[{figureDir, "06_tropical_curve_2d.png"}], fig06,
  ImageResolution -> 200];
Print["wrote figures/06_tropical_curve_2d.png"];

(* =====================================================================
   Figure 07 — Shortest paths via tropical Kleene star
   ===================================================================== *)

(* A 5-node directed graph with edge weights (in "cost" units). *)

edges = {
  1 -> 2 -> 3,    (* edges with weights given separately *)
  1 -> 3 -> 5,
  2 -> 3 -> 1,
  2 -> 4 -> 4,
  3 -> 5 -> 2,
  4 -> 5 -> 1,
  3 -> 4 -> 1
};

(* parse: each rule i -> j -> w *)
parsedEdges = {#[[1]], #[[2, 1]], #[[2, 2]]} & /@ edges;

n = 5;
costMatrix = ConstantArray[Infinity, {n, n}];
Do[costMatrix[[e[[1]], e[[2]]]] = e[[3]], {e, parsedEdges}];

(* shortest-path distances via tropical Kleene on -costs *)
shortest = MPShortestPathDistances[costMatrix];

graphEdges = DirectedEdge @@@ parsedEdges[[All, 1 ;; 2]];
edgeLabels = Thread[
  graphEdges ->
  (Style[#, 12, Background -> White] & /@ parsedEdges[[All, 3]])
];
graphG = Graph[
  graphEdges,
  EdgeLabels    -> edgeLabels,
  VertexLabels  -> "Name",
  VertexSize    -> 0.25,
  VertexStyle   -> Lighter[Orange, 0.5],
  EdgeStyle     -> Directive[GrayLevel[0.4], Arrowheads[0.04]],
  ImageSize     -> 460,
  GraphLayout   -> "SpringElectricalEmbedding"];

(* Render the distance matrix as an explicit Grid of cells so that the
   Export to PNG actually rasterises it (TableForm does not). *)

infSym = "\[Infinity]";
formatDist[v_] := If[v === Infinity || v === -Infinity, infSym,
                     ToString[NumberForm[N[v], {3, 1}]]];

headerStyle  = Directive[Bold, 13];
cellStyle    = Directive[13];

distRows = Table[
  Prepend[
    Table[Style[formatDist[shortest[[i, j]]], cellStyle], {j, n}],
    Style[Row[{"from ", i}], headerStyle]],
  {i, n}];

distGrid = Grid[
  Join[
    {Prepend[
       Table[Style[Row[{"to ", j}], headerStyle], {j, n}], ""]},
    distRows],
  Frame      -> All,
  Background -> {None,
                 Join[{LightGray}, ConstantArray[None, n]]},
  Spacings   -> {1.5, 1.0},
  Alignment  -> {Center, Center}];

distFig = Column[{
  Style["Shortest-path distance matrix d(i -> j)", 15, Bold],
  Spacer[10],
  distGrid,
  Spacer[10],
  Style["computed as -KleeneStar(-A) for cost matrix A", 11, Italic]
}, Alignment -> Center];

fig07 = Grid[{{
  Column[{Style["Directed weighted graph (5 nodes, 7 edges)", 15, Bold],
          graphG}, Alignment -> Center],
  distFig
}}, Spacings -> {4, 0}, Alignment -> {Center, Center}];

(* Rasterise to PNG. Rasterize is more reliable than Export for mixed
   Graph + Grid content. *)
Export[FileNameJoin[{figureDir, "07_shortest_paths.png"}],
  Rasterize[fig07, ImageResolution -> 200, Background -> White]];
Print["wrote figures/07_shortest_paths.png"];

(* =====================================================================
   Figure 08 — PERT-style task scheduling
   ===================================================================== *)

(* Eight tasks with durations, and a precedence DAG.
   We compute the earliest-start, earliest-finish, and total makespan. *)

durations = {3, 2, 4, 1, 3, 2, 4, 2};
prereqs   = {1 -> 3, 2 -> 3, 3 -> 5, 4 -> 5, 5 -> 7, 6 -> 7, 7 -> 8};

sched = MPSchedule[durations, prereqs];

es = sched["EarlyStart"];
ef = sched["EarlyFinish"];
mk = sched["Makespan"];

bars = MapIndexed[
  With[{i = #2[[1]]},
    {ColorData[97][i],
     Rectangle[{es[[i]], i - 0.4}, {ef[[i]], i + 0.4}],
     Black, Text[Style[Row[{"T", i, "  (d=", durations[[i]], ")"}], 12],
       {(es[[i]] + ef[[i]])/2, i}]}] &,
  durations];

ganttFig = Graphics[bars,
  Frame -> True,
  FrameLabel -> {Style["time", 14], Style["task index", 14]},
  PlotRange -> {{-0.5, mk + 1}, {0, Length[durations] + 1}},
  PlotLabel -> Style[Row[{
    "Earliest schedule, makespan = ", mk}], 15],
  AspectRatio -> 0.5, ImageSize -> 760,
  GridLines -> {Range[0, mk + 1], None},
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin]];

(* DAG visual *)
depFig = Graph[
  DirectedEdge @@@ List @@@ prereqs,
  VertexLabels -> Thread[Range[Length[durations]] ->
    (Placed[Style[Row[{"T", #, "\n", durations[[#]], "u"}], 11],
      Center] & /@ Range[Length[durations]])],
  VertexSize -> 0.3,
  VertexStyle -> Lighter[Orange, 0.4],
  EdgeStyle -> Directive[GrayLevel[0.4], Arrowheads[0.05]],
  ImageSize -> 460,
  GraphLayout -> "LayeredDigraphEmbedding"];

fig08 = Grid[{{depFig, ganttFig}}, Spacings -> {2, 0}];

Export[FileNameJoin[{figureDir, "08_pert_scheduling.png"}], fig08,
  ImageResolution -> 200];
Print["wrote figures/08_pert_scheduling.png"];

(* =====================================================================
   Figure 09 — Karp's algorithm: max cycle mean = tropical eigenvalue
   ===================================================================== *)

(* A directed graph with a single dominant cycle, plus some other edges *)

n2 = 4;
A2 = ConstantArray[-Infinity, {n2, n2}];
A2[[1, 2]] = 2;
A2[[2, 3]] = 3;
A2[[3, 4]] = 1;
A2[[4, 1]] = 4;  (* the big cycle 1→2→3→4→1 with total weight 10, mean 2.5 *)
A2[[1, 3]] = 1;
A2[[3, 1]] = 0;
A2[[2, 4]] = 1;

lambdaStar = MPEigenvalue[A2];

graphG2 = Graph[
  Flatten[Table[
    If[A2[[i, j]] =!= -Infinity, DirectedEdge[i, j], Nothing],
    {i, n2}, {j, n2}]],
  EdgeLabels -> Flatten[Table[
    If[A2[[i, j]] =!= -Infinity,
       DirectedEdge[i, j] -> Style[A2[[i, j]], 12, Background -> White],
       Nothing],
    {i, n2}, {j, n2}]],
  VertexLabels -> "Name",
  VertexSize -> 0.25,
  VertexStyle -> Lighter[ColorData[97][3], 0.4],
  EdgeStyle -> Directive[GrayLevel[0.4], Arrowheads[0.05]],
  ImageSize -> 460,
  GraphLayout -> "CircularEmbedding"];

(* Plot of the iterates: starting from x0 = {0, 0, 0, 0},
   x_t = A ⊙ x_{t-1} grows asymptotically like λ_max ⊗ x_{t-1}. *)

x0 = ConstantArray[0., n2];
traj = MPLinearSystemEvolve[A2, x0, 15];
trajVals = Mean /@ traj;

(* Plot per-step growth of mean(x_t).  The increments stabilise at
   exactly the tropical eigenvalue \[Lambda]_max. *)
diffs = Differences[trajVals];

iterPlot = ListLinePlot[
  diffs,
  PlotMarkers -> {Automatic, 8},
  PlotStyle -> Directive[ColorData[97][3], Thick],
  Frame -> True,
  FrameLabel -> {Style["iteration t", 14],
                  Style["mean(x_{t+1}) - mean(x_t)", 14]},
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.85], Thin],
  PlotRange -> {All, {0, Max[diffs] + 0.5}},
  Epilog -> {Red, AbsoluteDashing[{6, 6}], Thickness[0.003],
    Line[{{0, lambdaStar}, {Length[traj], lambdaStar}}],
    Text[Style[
      Row[{"\[Lambda]_max = ", N[lambdaStar]}],
      14, Red, Bold],
      {Length[traj] - 1, lambdaStar}, {1.2, -1.2}]},
  PlotLabel -> Style[
    Row[{"x_{t+1} = A \[CircleDot] x_t : per-step mean growth -> ",
         "\[Lambda]_max = ", N[lambdaStar]}], 15],
  ImageSize -> 620];

fig09 = Grid[{{
  Column[{Style["Weighted digraph; Karp's tropical eigenvalue",
                15, FontWeight -> "Bold"], graphG2}, Alignment -> Center],
  iterPlot
}}, Spacings -> {2, 0}];

Export[FileNameJoin[{figureDir, "09_karp_eigenvalue.png"}], fig09,
  ImageResolution -> 200];
Print["wrote figures/09_karp_eigenvalue.png  (λ_max = ", lambdaStar, ")"];

Print["all worked-example figures generated."];
