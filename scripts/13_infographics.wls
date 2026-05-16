#!/usr/bin/env wolframscript
(* Four pedagogical infographics, rendered entirely in WL and exported
   as figures 13-16. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
figureDir = FileNameJoin[{repoRoot, "figures"}];
If[!DirectoryQ[figureDir], CreateDirectory[figureDir]];

(* shared styling helpers *)

bigTitle[s_] := Style[s, 22, Bold, FontFamily -> "Helvetica"];
hdr[s_]      := Style[s, 15, Bold, FontFamily -> "Helvetica",
                       White, Background -> RGBColor[0.18, 0.30, 0.55]];
cell[s_]     := Style[s, 14, FontFamily -> "Helvetica"];
mono[s_]     := Style[s, 14, FontFamily -> "Courier"];

(* =====================================================================
   Figure 13 -- The tropical semiring card
   ===================================================================== *)

semiringTable = Grid[{
  {hdr["property"],                hdr["classical algebra (R, +, *)"],         hdr["tropical algebra (R \[Union] {-\[Infinity]}, \[CirclePlus], \[CircleTimes])"]},
  {cell["addition"],               cell["a + b"],                                cell[Row[{"a \[CirclePlus] b  =  ", Style["Max[a, b]", FontFamily -> "Courier"]}]]},
  {cell["multiplication"],         cell["a * b"],                                cell[Row[{"a \[CircleTimes] b  =  ", Style["a + b", FontFamily -> "Courier"]}]]},
  {cell["additive identity"],      cell["0   (since 0 + a = a)"],                cell["-\[Infinity]   (since Max[-\[Infinity], a] = a)"]},
  {cell["multiplicative identity"],cell["1   (since 1 * a = a)"],                cell["0   (since 0 + a = a)"]},
  {cell["addition associative?"],  cell["yes"],                                  cell["yes (Max is associative)"]},
  {cell["addition commutative?"],  cell["yes"],                                  cell["yes"]},
  {cell["distributivity"],         cell["a*(b+c) = a*b + a*c"],                  cell["a \[CircleTimes] (b \[CirclePlus] c) = (a \[CircleTimes] b) \[CirclePlus] (a \[CircleTimes] c)"]},
  {cell["additive inverse?"],      cell["yes (a + (-a) = 0)"],                   cell[Style["no -- max-plus is a SEMIRING, not a ring", Italic, RGBColor[0.6, 0.2, 0.2]]]},
  {cell["powers"],                 cell["a^n = a*a*...*a"],                       cell[Row[{Style["a", FontFamily -> "Courier"],
       Superscript["", Style["\[CircleTimes]n", 11]],
       " = ",
       Style["n*a", FontFamily -> "Courier"]}]]},
  {cell["example: 3 + 7"],         cell["10"],                                   cell[Row[{Style["3 \[CirclePlus] 7  =  Max[3, 7]  =  7", FontFamily -> "Courier"]}]]},
  {cell["example: 3 * 7"],         cell["21"],                                   cell[Row[{Style["3 \[CircleTimes] 7  =  3 + 7  =  10", FontFamily -> "Courier"]}]]}
  },
  Frame      -> All,
  Spacings   -> {1.4, 1.0},
  Background -> {None,
                 Join[{RGBColor[0.18, 0.30, 0.55]},
                      ConstantArray[None, 11]]},
  Alignment  -> {Left, Center},
  Dividers   -> {{True, True, True, True}, All}];

fig13 = Column[{
  bigTitle["The two algebras side by side"],
  Spacer[10],
  Style["The max-plus semiring keeps every axiom of classical algebra except additive inverses.",
        14, Italic, FontFamily -> "Helvetica"],
  Spacer[14],
  semiringTable
}, Alignment -> Center, Spacings -> 0.4];

Export[FileNameJoin[{figureDir, "13_semiring_card.png"}],
  Rasterize[Pane[fig13, ImageMargins -> 24, Background -> White],
    ImageResolution -> 200, Background -> White]];
Print["wrote figures/13_semiring_card.png"];

(* =====================================================================
   Figure 14 -- ReLU(x) = 0 \[CirclePlus] x  (the bridge picture)
   ===================================================================== *)

reluPlot = Plot[Max[0, x], {x, -3, 3},
  PlotStyle -> Directive[ColorData[97][1], Thickness[0.012]],
  Frame     -> True, FrameLabel -> {Style["x", 16], Style["ReLU(x)", 16]},
  Axes      -> True, AxesStyle -> GrayLevel[0.5],
  PlotRange -> {{-3, 3}, {-0.5, 3}},
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin],
  Epilog -> {
    (* the constant 0 monomial line *)
    ColorData[97][2], AbsoluteDashing[{6, 6}], Thickness[0.004],
    Line[{{-3, 0}, {3, 0}}],
    Text[Style["y = 0  (the additive identity of \[CircleTimes])", 12,
               ColorData[97][2]],
         {-1.5, 0.18}, {-1, 0}],
    (* the y = x monomial line *)
    ColorData[97][3], AbsoluteDashing[{6, 6}], Thickness[0.004],
    Line[{{-0.5, -0.5}, {3, 3}}],
    Text[Style["y = x", 12, ColorData[97][3]],
         {2.4, 2.7}, {-1, 0}],
    (* the "kink" at the origin *)
    Black, AbsolutePointSize[10], Point[{0, 0}],
    Text[Style["kink at 0", 11, Bold], {0.3, 0.5}, {-1, 0}]
  },
  ImageSize -> 460,
  PlotLabel -> Style["ReLU(x) = max(0, x)", 16]];

reluFormulaPanel = Pane[
  Column[{
    bigTitle["ReLU is the tropical sum  0 \[CirclePlus] x"],
    Spacer[14],
    Style[TraditionalForm[Defer[ReLU[x] == Max[0, x] == 0 \[CirclePlus] x]],
          22],
    Spacer[14],
    Style["The output of a single ReLU unit is the max of its \
pre-activation with 0.  In max-plus algebra, that operation is \
literally  0 \[CirclePlus] (\!\(\*StyleBox[\"input\",FontSlant->\"Italic\"]\)). \
0 is the tropical multiplicative identity, so a ReLU unit is the \
smallest possible degree-1 tropical polynomial:",
          14, FontFamily -> "Helvetica"],
    Spacer[8],
    Style[TraditionalForm[
       Defer[h[x] == 0 \[CirclePlus] (a x + b) == Max[0, a x + b]]],
      20],
    Spacer[14],
    Style["A whole 1-hidden-layer ReLU network is then a \
classical sum of such tropical monomials -- which, by distributivity, \
is itself a tropical polynomial (or a difference of two, if some \
output weights are negative). \nThe whole forward pass is a \
tropical rational map.",
      14, FontFamily -> "Helvetica"]
  }, Alignment -> Left, Spacings -> 0.7],
  ImageMargins -> 20, Background -> White,
  ImageSize -> 820];

fig14 = Rasterize[
  Pane[Row[{reluPlot, Spacer[40], reluFormulaPanel},
        BaselinePosition -> Top],
    Background -> White, ImageMargins -> 18],
  ImageResolution -> 200, Background -> White];
Export[FileNameJoin[{figureDir, "14_relu_is_tropical.png"}], fig14];
Print["wrote figures/14_relu_is_tropical.png"];

(* =====================================================================
   Figure 15 -- Shortest paths as tropical matrix-vector
   ===================================================================== *)

(* Small 4-node graph with edge costs.  The Bellman-Ford recurrence
        d_{t+1}(v) = min_u  (d_t(u) + cost(u, v))
   reads in tropical algebra as a SHORTEST-PATH min-plus mat-vec, the
   "twin" of our max-plus mat-vec.  We show both forms.            *)

nodes = {1, 2, 3, 4};
edges = {1 -> 2 -> 2, 1 -> 3 -> 5, 2 -> 3 -> 1, 2 -> 4 -> 6, 3 -> 4 -> 2};
parsed = {#[[1]], #[[2, 1]], #[[2, 2]]} & /@ edges;
n      = 4;

costM = ConstantArray[Infinity, {n, n}];
Do[costM[[e[[1]], e[[2]]]] = e[[3]], {e, parsed}];

infStr[v_] := If[v === Infinity, "\[Infinity]", ToString[v]];

graphFig = Graph[
  DirectedEdge @@@ parsed[[All, 1 ;; 2]],
  EdgeLabels   -> Thread[
    (DirectedEdge @@@ parsed[[All, 1 ;; 2]]) ->
    (Style[#, 13, Background -> White] & /@ parsed[[All, 3]])],
  VertexLabels -> "Name",
  VertexSize   -> 0.25,
  VertexStyle  -> Lighter[Orange, 0.45],
  EdgeStyle    -> Directive[GrayLevel[0.4], Arrowheads[0.05]],
  GraphLayout  -> "CircularEmbedding",
  ImageSize    -> 360];

costMatrix = Grid[
  Join[
    {Prepend[Table[Style["to " <> ToString[j], Bold, 12], {j, n}], ""]},
    Table[
      Prepend[
        Table[Style[infStr[costM[[i, j]]], 13], {j, n}],
        Style["from " <> ToString[i], Bold, 12]],
      {i, n}]],
  Frame     -> All,
  Spacings  -> {1.5, 0.8},
  Background -> {None,
                 Join[{LightGray}, ConstantArray[None, n]]},
  Alignment -> {Center, Center}];

formulaPanel = Pane[
  Column[{
    bigTitle["Shortest paths = tropical (min-plus) algebra"],
    Spacer[12],
    Style["Bellman-Ford in classical notation",
       14, Italic, GrayLevel[0.4]],
    Style[TraditionalForm[
       Subscript[d, t + 1][v] == Underscript[Min, u][Subscript[d, t][u] + cost[u, v]]],
      20],
    Spacer[10],
    Style["Bellman-Ford in tropical (min-plus) notation",
       14, Italic, GrayLevel[0.4]],
    Style[TraditionalForm[
       Subscript[d, t + 1] == Subscript[d, t] \[CircleDot] C],
      22],
    Spacer[12],
    Style["where  \[CircleDot]  uses  Min  for \[CirclePlus]  and  +  for \[CircleTimes].",
      13, Italic, FontFamily -> "Helvetica"],
    Spacer[10],
    Style["Iterating the tropical mat-vec is therefore the classical \
Bellman-Ford algorithm.  Taking the Kleene star",
      14, FontFamily -> "Helvetica"],
    Spacer[4],
    Style[TraditionalForm[
       Superscript[C, \[FilledSmallSquare]] ==
         Subscript[I, n] \[CirclePlus] C \[CirclePlus]
         Superscript[C, \[CircleTimes]2] \[CirclePlus] \[Ellipsis]],
      18],
    Style["recovers the full all-pairs distance matrix in one shot.",
      14, FontFamily -> "Helvetica"]
  }, Alignment -> Left, Spacings -> 0.5],
  ImageMargins -> 18, Background -> White,
  ImageSize -> 720];

fig15 = Rasterize[
  Pane[
    Row[{
      Column[{Style["weighted directed graph", 15, Bold], graphFig,
              Spacer[8],
              Style["cost matrix C  (\[Infinity] = no edge)", 14, Italic],
              costMatrix},
        Alignment -> Center, Spacings -> 0.6],
      Spacer[40],
      formulaPanel
    }, BaselinePosition -> Top],
    Background -> White, ImageMargins -> 20],
  ImageResolution -> 200, Background -> White];
Export[FileNameJoin[{figureDir, "15_shortest_paths_infographic.png"}], fig15];
Print["wrote figures/15_shortest_paths_infographic.png"];

(* =====================================================================
   Figure 16 -- Newton-polytope duality schematic
   ===================================================================== *)

(* Toy polynomial:  p(x) = max(0, 2x - 1, x + 1, 3x - 3)  in 1D.
   Lift each (slope a, intercept c) to R^2 and show:
     - lifted points
     - upper hull
     - regions in the input plane induced by p
     - dual subdivision of the slope-axis  *)

p1mon = {{0, 0}, {2, -1}, {1, 1}, {3, -3}};

(* upper hull in 2D *)
ptsSorted = SortBy[p1mon, First];
upper = Module[{stack = {ptsSorted[[1]]}, p},
  Do[
    p = ptsSorted[[i]];
    While[Length[stack] >= 2 &&
      With[{a = stack[[-2]], b = stack[[-1]]},
        (b[[1]] - a[[1]]) (p[[2]] - a[[2]]) -
        (b[[2]] - a[[2]]) (p[[1]] - a[[1]]) >= 0],
      stack = Most[stack]];
    AppendTo[stack, p],
    {i, 2, Length[ptsSorted]}];
  stack];

(* find x-domain where each upper-hull monomial wins *)
breakpoints = Table[
  With[{u = upper[[i]], v = upper[[i + 1]]},
    (v[[2]] - u[[2]]) / (u[[1]] - v[[1]])],
  {i, Length[upper] - 1}];

(* Panel A: the lifted picture *)
panelA = Graphics[{
  (* drop lines *)
  GrayLevel[0.7], Thin,
  Line[{#, {#[[1]], -4.5}}] & /@ p1mon,
  (* upper hull *)
  ColorData[97][1], Thick, Line[upper],
  (* baseline = slope axis *)
  Black, AbsoluteThickness[1.2], Line[{{-0.4, -4.5}, {3.4, -4.5}}],
  (* points *)
  AbsolutePointSize[12], ColorData[97][1],
  Point /@ p1mon,
  (* labels *)
  Black,
  Text[Style["(0, 0)", 11], p1mon[[1]], {-1.2, -0.8}],
  Text[Style["(2, -1)", 11], p1mon[[2]], {-1.2, -0.8}],
  Text[Style["(1, 1)", 11], p1mon[[3]], {-1.2, -0.8}],
  Text[Style["(3, -3)", 11], p1mon[[4]], {-1.2, -0.8}],
  (* axis labels *)
  Black,
  Text[Style["slope  a", 13, Italic], {3.2, -4.7}],
  Text[Style["intercept  c", 13, Italic], {-0.5, 1.5}, {0, 0}, {0, 1}]
  },
  Frame -> True,
  FrameLabel -> None,
  PlotLabel -> Style["1.  lift each monomial to (slope, intercept)", 14],
  AspectRatio -> 0.7,
  PlotRange -> {{-0.5, 3.5}, {-4.7, 2.5}},
  ImageSize -> 380,
  GridLines -> Automatic,
  GridLinesStyle -> Directive[GrayLevel[0.88], Thin]];

(* Panel B: the input-space picture *)

(* x-coords of breakpoints; segments are (-inf, b1], [b1, b2], ..., [bk, inf) *)
domX = {-2, 3};
xs = Subdivide[domX[[1]], domX[[2]], 600];
vals = Max @@ (#[[1]] x + #[[2]]) & /@ p1mon /. x -> # & /@ xs;

(* find argmax for each x *)
argmaxXs = First @ Ordering[p1mon[[All, 1]] * #1 + p1mon[[All, 2]], -1] & /@ xs;

panelB = Show[
  Plot[Max[0, 2 x - 1, x + 1, 3 x - 3], {x, domX[[1]], domX[[2]]},
    PlotStyle -> Directive[Black, Thickness[0.008]],
    Frame -> True,
    FrameLabel -> {Style["x", 14], Style["p(x)", 14]},
    ImageSize -> 380,
    PlotLabel -> Style["2.  the resulting tropical polynomial p(x)", 14],
    GridLines -> Automatic,
    GridLinesStyle -> Directive[GrayLevel[0.88], Thin]],
  Graphics[{
    Red, AbsoluteDashing[{4, 4}], Thickness[0.003],
    Line[{{#, -3}, {#, 8}}] & /@ breakpoints,
    Black, Text[Style["corners", 12, Bold, Red],
      {Mean[breakpoints], 7}]
  }]];

(* legend: explain duality *)
explanation = Pane[
  Column[{
    bigTitle["The Newton-polytope duality"],
    Spacer[10],
    Style["A tropical polynomial   p(x) = max\!\(\*SubscriptBox[\(\), \(i\)]\)( \!\(\*SubscriptBox[\(a\), \(i\)]\) x + \!\(\*SubscriptBox[\(c\), \(i\)]\))   defines a piecewise-linear function. \
Each piece is realised by the monomial whose lifted point (\!\(\*SubscriptBox[\(a\), \(i\)]\), \!\(\*SubscriptBox[\(c\), \(i\)]\)) \
sits on the upper convex hull of all lifted points. Interior monomials \
are dominated everywhere and contribute nothing.",
      14, FontFamily -> "Helvetica"],
    Spacer[8],
    Style["Corners of the envelope occur at edges of the upper hull, \
and the slopes left and right of a corner are the slopes of the two \
adjacent upper-hull vertices.",
      14, FontFamily -> "Helvetica"],
    Spacer[8],
    Style["In higher dimension the same picture holds with the upper \
hull a polytope and its facets projecting to a regular subdivision \
of input space.  For a neural network this is exactly the activation-\
pattern arrangement.",
      14, FontFamily -> "Helvetica"]
  }, Alignment -> Left, Spacings -> 0.6],
  ImageSize -> 760, Background -> White, ImageMargins -> 20];

fig16 = Rasterize[
  Pane[
    Column[{
      Row[{panelA, Spacer[20], panelB}, BaselinePosition -> Top],
      Spacer[10],
      explanation
    }, Alignment -> Center, Spacings -> 0.4],
    Background -> White, ImageMargins -> 20],
  ImageResolution -> 200, Background -> White];
Export[FileNameJoin[{figureDir, "16_newton_duality_infographic.png"}], fig16];
Print["wrote figures/16_newton_duality_infographic.png"];

Print["all infographics generated."];
