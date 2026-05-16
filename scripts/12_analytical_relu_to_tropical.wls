#!/usr/bin/env wolframscript
(* Symbolic ReLU -> tropical-rational derivations.

   Goal: produce nicely typeset figures (rendered via Rasterize +
   TraditionalForm) that show step-by-step why a 1-hidden-layer ReLU
   classifier is *literally* a max-plus rational map P(x) ⊖ Q(x). *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "MaxPlus.wl"}]];
Needs["MaxPlus`"];

figureDir = FileNameJoin[{repoRoot, "figures"}];

(* helper: render an Annotated equation cell as a centred block *)

eqLabel[lbl_, expr_] :=
  Style[
    Row[{Style[lbl, Italic, 11, GrayLevel[0.3]], "       ",
         TraditionalForm[expr]}],
    18];

textBlock[s_, size_: 14] :=
  Pane[Style[s, size, FontFamily -> "Helvetica", LineSpacing -> {1.3, 0}],
       ImageSize -> {880, Automatic}];

sectionTitle[s_] :=
  Style[s, 17, Bold, FontFamily -> "Helvetica",
        Background -> GrayLevel[0.92]];

(* ===================================================================
   Figure 10 --Symbolic derivation for a 2 -> 1 -> 1 ReLU network
   =================================================================== *)

(* Variables.  a, b are the input weights of the single hidden unit,
   c is its bias, w is the output weight, e is the output bias.
   max(0, .) is the ReLU. *)

ClearAll[a, b, c, w, e, x1, x2, ax, bx];

lAffine = a x1 + b x2 + c;            (* the pre-activation        *)
h       = Max[0, lAffine];            (* the hidden unit value     *)
f       = w h + e;                    (* the network output        *)

(* Case 1: w > 0 *)
fPos = w Max[0, lAffine] + e;
(* pull w inside the Max because it is positive *)
fPosExpanded = Max[0 + e, w lAffine + e];

(* As P - Q with Q = 0 (a tropical zero polynomial) *)
PCase1 = Max[e, w (a x1 + b x2 + c) + e];   (* a tropical degree-1 poly *)
QCase1 = 0;

(* Case 2: w < 0.  Write w = -wabs, wabs > 0. *)
ClearAll[wabs];
fNeg = -wabs Max[0, lAffine] + e;
(* -wabs * Max[0, l] = -Max[0, wabs * l].  Set P = e (constant) and
   Q = Max[0, wabs * l] = Max[0, wabs*a x1 + wabs*b x2 + wabs*c]. *)
PCase2 = e;
QCase2 = Max[0, wabs (a x1 + b x2 + c)];

fig10 = Pane[
  Column[{
    sectionTitle["A 2 \[RightArrow]1 \[RightArrow]1 ReLU network as a tropical rational map"],
    Spacer[8],
    textBlock["The smallest non-trivial classifier is a 1-hidden-unit ReLU \
network on two inputs. We derive its tropical rational form by case-\
analysing the sign of the output weight w."],
    Spacer[8],
    eqLabel["hidden unit",
      HoldForm[h] == HoldForm[Max[0, a x1 + b x2 + c]]],
    eqLabel["output",
      HoldForm[f] == HoldForm[w h + e]],
    Spacer[10],
    sectionTitle["Case 1.  w > 0  (we can pull w inside the Max)"],
    eqLabel["substitute h",
      HoldForm[f] ==
        HoldForm[w Max[0, a x1 + b x2 + c] + e]],
    eqLabel["w \[GreaterEqual] 0 \[Implies]  w\[CenterDot]Max[a, b] = Max[w\[CenterDot]a, w\[CenterDot]b]",
      HoldForm[f] ==
        HoldForm[Max[e, w (a x1 + b x2 + c) + e]]],
    eqLabel["this is P - Q with Q == 0",
      Row[{
        HoldForm[P[x1, x2]] == HoldForm[Max[e, w a x1 + w b x2 + w c + e]],
        "        ",
        HoldForm[Q[x1, x2]] == 0
      }]],
    Spacer[10],
    sectionTitle["Case 2.  w < 0  (write w = -|w|; the minus goes to Q)"],
    eqLabel["substitute h",
      HoldForm[f] ==
        HoldForm[-wabs Max[0, a x1 + b x2 + c] + e]],
    eqLabel["factor: -wabs\[CenterDot]Max[0, l] = -Max[0, wabs\[CenterDot]l]",
      HoldForm[f] ==
        HoldForm[e - Max[0, wabs (a x1 + b x2 + c)]]],
    eqLabel["this is P - Q",
      Row[{
        HoldForm[P[x1, x2]] == e,
        "        ",
        HoldForm[Q[x1, x2]] ==
          HoldForm[Max[0, wabs a x1 + wabs b x2 + wabs c]]
      }]],
    Spacer[10],
    sectionTitle["Take-away"],
    textBlock["In both cases the network output is the classical \
difference of two tropical polynomials P(x) and Q(x).  P and Q are \
convex piecewise-linear, hence their level sets {P = const} and \
{Q = const} are tropical hypersurfaces.  The decision boundary \
{f = 0} = {P = Q} is the tropical hypersurface where the two convex \
piecewise-linear surfaces meet."]
  }, Spacings -> 0.6],
  ImageMargins -> 18, Background -> White];

Export[FileNameJoin[{figureDir, "10_analytical_2-1-1.png"}],
  Rasterize[fig10, ImageResolution -> 200, Background -> White]];
Print["wrote figures/10_analytical_2-1-1.png"];

(* ===================================================================
   Figure 11 --The decisive identity:  classical sum of ReLU units is
   tropical product of their tropical polynomials.
   =================================================================== *)

(* The identity
       Max[0, A] + Max[0, B]  ==  Max[0, A, B, A + B]
   is the engine behind everything that follows.  Tropically it reads
       (0 ⊕ A) ⊗ (0 ⊕ B)  ==  0 ⊕ A ⊕ B ⊕ (A ⊗ B).
   On the right we see *four* tropical monomials, i.e. the Newton
   polytope of (0 ⊕ A) ⊗ (0 ⊕ B) is a 2-simplex / unit square in the
   classical exponent lattice. *)

ClearAll[A, B];

lhs = Max[0, A] + Max[0, B];
rhs = Max[0, A, B, A + B];

(* Visual: split the (A, B) plane into the 4 regions where each of the
   four arguments to Max is the max.                                 *)

dom = {{-2, 2}, {-2, 2}};
panel11A = TropicalRegionPlot[
  TropicalPolynomial[{{0, 0}, {1, 0}, {0, 1}, {1, 1}},
                      {0, 0, 0, 0}],
  {A, B}, dom];
panel11A = Show[panel11A,
  Frame -> True,
  FrameLabel -> {Style["A", 14], Style["B", 14]},
  PlotLabel -> Style["four regions of  Max[0, A, B, A+B]", 15],
  ImageSize -> 460];

(* Right panel: the algebraic identity, typeset *)

panel11B = Pane[
  Column[{
    sectionTitle["Identity (the heart of the matter)"],
    Spacer[6],
    eqLabel["classical sum of two ReLU units",
      HoldForm[Max[0, A] + Max[0, B]] ==
        HoldForm[Max[0, A, B, A + B]]],
    eqLabel["same identity in tropical notation",
      Row[{
        HoldForm[(0 \[CirclePlus] A) \[CircleTimes]
                 (0 \[CirclePlus] B)],
        " = ",
        HoldForm[0 \[CirclePlus] A \[CirclePlus] B \[CirclePlus]
                  (A \[CircleTimes] B)]
      }]],
    Spacer[10],
    textBlock["A 1-hidden-layer ReLU network is a CLASSICAL SUM of \
ReLU units.  By the identity above, that classical sum is --in \
max-plus algebra --exactly the TROPICAL PRODUCT of the units' \
tropical polynomials. So the whole network is a *single* tropical \
polynomial (positive-weight case) or a difference of two tropical \
polynomials (mixed-sign case)."],
    Spacer[6],
    textBlock["Geometrically: each tropical product doubles the number \
of monomials in the worst case; the convex hull of the exponents (the \
Newton polytope) gets larger, and so does the number of realised \
linear regions in input space.  For a 2-D input and n hidden units \
this caps at the Zaslavsky bound  Z(n) = 1 + n + C(n,2)."]
  }, Spacings -> 0.6],
  ImageMargins -> 18, Background -> White];

(* Rasterise the two halves independently to fixed-size PNGs and then
   ImageAssemble them — this is more predictable than mixing Pane/Grid/
   Rasterize for content of very different aspect ratios.            *)
imgL = Rasterize[panel11A, ImageResolution -> 200, Background -> White,
   ImageSize -> 460];
imgR = Rasterize[panel11B, ImageResolution -> 200, Background -> White,
   ImageSize -> 920];
(* pad imgL vertically to match imgR's height if needed *)
{hL, wL} = ImageDimensions[imgL][[{2, 1}]];
{hR, wR} = ImageDimensions[imgR][[{2, 1}]];
targetH = Max[hL, hR];
imgL = ImagePad[imgL, {{0, 0}, {0, targetH - hL}}, White];
imgR = ImagePad[imgR, {{0, 0}, {0, targetH - hR}}, White];
fig11Img = ImageAssemble[{{imgL,
    Image[ConstantArray[{1, 1, 1}, {targetH, 40}]], imgR}}];
Export[FileNameJoin[{figureDir, "11_analytical_tropical_product.png"}],
  fig11Img];
Print["wrote figures/11_analytical_tropical_product.png"];
(* skip the second Export below by short-circuiting *)
fig11 = fig11Img;

(* the figure was written above *)

(* ===================================================================
   Figure 12 --2-2-1 network: full symbolic P, Q
   =================================================================== *)

ClearAll[a1, b1, c1, a2, b2, c2, w1, w2, e];

l1 = a1 x1 + b1 x2 + c1;
l2 = a2 x1 + b2 x2 + c2;
h1 = Max[0, l1];
h2 = Max[0, l2];

(* Sub-case A: w1 > 0, w2 > 0  ->  P alone (Q = 0) *)
fPosPos = w1 h1 + w2 h2 + e;
PfromPosPos =
  Max[e,
      w1 (a1 x1 + b1 x2 + c1) + e,
      w2 (a2 x1 + b2 x2 + c2) + e,
      w1 (a1 x1 + b1 x2 + c1) + w2 (a2 x1 + b2 x2 + c2) + e];
(* 4 monomials; Newton polytope spans {0, e1, e2, e1+e2} *)

(* Sub-case B: w1 > 0, w2 < 0  ->  proper P and Q *)
ClearAll[w2abs];
fPosNeg = w1 h1 - w2abs h2 + e;
PfromPosNeg = Max[e, w1 (a1 x1 + b1 x2 + c1) + e];
QfromPosNeg = Max[0, w2abs (a2 x1 + b2 x2 + c2)];

fig12 = Pane[
  Column[{
    sectionTitle["A 2 \[RightArrow]2 \[RightArrow]1 ReLU network --full symbolic P and Q"],
    Spacer[8],
    textBlock["Two ReLU units feeding a linear output. The two output \
weights split into a positive-weight group (contributing to P) and a \
negative-weight group (contributing to Q)."],
    Spacer[6],
    eqLabel["hidden units",
      Row[{
        HoldForm[h1] == HoldForm[Max[0, a1 x1 + b1 x2 + c1]],
        "       ",
        HoldForm[h2] == HoldForm[Max[0, a2 x1 + b2 x2 + c2]]
      }]],
    eqLabel["output",
      HoldForm[f] == HoldForm[w1 h1 + w2 h2 + e]],
    Spacer[8],
    sectionTitle["Sub-case  w1 > 0, w2 > 0  \[RightArrow] tropical polynomial"],
    eqLabel["use the identity Max[0,A] + Max[0,B] = Max[0, A, B, A+B]",
      HoldForm[P[x1, x2]] ==
        Max[e,
            w1 (a1 x1 + b1 x2 + c1) + e,
            w2 (a2 x1 + b2 x2 + c2) + e,
            w1 (a1 x1 + b1 x2 + c1) + w2 (a2 x1 + b2 x2 + c2) + e]],
    eqLabel["Q == 0",
      Row[{HoldForm[Q[x1, x2]] == 0}]],
    textBlock["P is a tropical polynomial with 4 monomials.  Their \
exponents in input space are {(0,0), (w1\[CenterDot]a1, w1\[CenterDot]b1), (w2\[CenterDot]a2, w2\[CenterDot]b2), \
(w1\[CenterDot]a1+w2\[CenterDot]a2, w1\[CenterDot]b1+w2\[CenterDot]b2)}, which form a (possibly degenerate) \
parallelogram in R^2 --the Minkowski sum of the two single-unit \
Newton segments."],
    Spacer[10],
    sectionTitle["Sub-case  w1 > 0, w2 < 0  \[RightArrow] proper tropical rational"],
    eqLabel["positive part",
      HoldForm[P[x1, x2]] ==
        HoldForm[Max[e, w1 (a1 x1 + b1 x2 + c1) + e]]],
    eqLabel["negative part",
      HoldForm[Q[x1, x2]] ==
        HoldForm[Max[0, w2abs (a2 x1 + b2 x2 + c2)]]],
    eqLabel["network output",
      HoldForm[f] == HoldForm[P[x1, x2] - Q[x1, x2]]],
    textBlock["Now P and Q are each degree-1 tropical polynomials \
(2 monomials each).  The decision boundary {f = 0} = {P = Q} is the \
locus where the two convex piecewise-linear surfaces P and Q meet --a \
finite arrangement of straight segments.  Adding more hidden units of \
the same sign multiplies P (or Q) tropically, doubling the monomial \
count in the worst case but at most quadrupling the number of Newton-\
polytope vertices in R^2."]
  }, Spacings -> 0.6],
  ImageMargins -> 18, Background -> White];

Export[FileNameJoin[{figureDir, "12_analytical_2-2-1.png"}],
  Rasterize[fig12, ImageResolution -> 200, Background -> White]];
Print["wrote figures/12_analytical_2-2-1.png"];

(* ===================================================================
   Sanity check: numerical equality between symbolic and numerical
   forms for random weights.
   =================================================================== *)

SeedRandom[19];
testCases = 10;
Do[
  Module[{aa, bb, cc, ww, ee, x1n, x2n, lhsVal, rhsVal},
    aa = RandomReal[{-1, 1}]; bb = RandomReal[{-1, 1}];
    cc = RandomReal[{-1, 1}]; ww = RandomReal[{-2, 2}];
    ee = RandomReal[{-0.5, 0.5}];
    x1n = RandomReal[{-2, 2}]; x2n = RandomReal[{-2, 2}];
    lhsVal = ww Max[0, aa x1n + bb x2n + cc] + ee;
    rhsVal = If[ww > 0,
      Max[ee, ww (aa x1n + bb x2n + cc) + ee],
      ee - Max[0, -ww (aa x1n + bb x2n + cc)]];
    If[Abs[lhsVal - rhsVal] > 1*^-12,
      Print["[FAIL] case ", t, ": ", lhsVal, " vs ", rhsVal];
      Exit[1]];
  ],
  {t, testCases}];
Print["sanity OK: 2-1-1 symbolic == numerical on ", testCases, " random cases."];

Print["all analytical figures generated."];
