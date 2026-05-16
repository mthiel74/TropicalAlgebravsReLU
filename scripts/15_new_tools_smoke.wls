#!/usr/bin/env wolframscript
(* Smoke tests for the analytical tools added to MaxPlus.wl:
   MPRealisedPatterns, MPRegionGradient, MPExactLipschitz, MPDeadUnits,
   MPPruneDeadUnits. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "MaxPlus.wl"}]];
Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];
Needs["MaxPlus`"];

dataDir = FileNameJoin[{repoRoot, "data"}];

(* Load the trained moons net. *)
ClearAll[Global`net, Global`ds];
Get[FileNameJoin[{dataDir, "moons_net.mx"}]];
theNet = Global`net;

dom = {{-2., 2.}, {-2., 2.}};

Print["--- MPRealisedPatterns ---"];
pats = MaxPlus`MPRealisedPatterns[theNet, dom, 200];
Print["realised activation patterns: ", Length[pats]];

Print["--- MPRegionGradient (first 3) ---"];
Do[
  Print["pattern ", pats[[k]], " -> gradient ",
    MaxPlus`MPRegionGradient[theNet, pats[[k]]]],
  {k, Min[3, Length[pats]]}];

Print["--- MPExactLipschitz ---"];
lip = MaxPlus`MPExactLipschitz[theNet, dom, 200];
Print["tight Lipschitz constant on ", dom, " = ", lip["Lipschitz"]];
Print["argmax activation pattern    = ", lip["ArgMaxPattern"]];

Print["--- MPDeadUnits on dom = ", dom, " ---"];
dead = MaxPlus`MPDeadUnits[theNet, dom];
Print["NeverActive units:  ", dead["NeverActive"]];
Print["AlwaysActive units: ", dead["AlwaysActive"]];

Print["--- MPDeadUnits on dom = {{-0.5, 0.5}, {-0.5, 0.5}} ---"];
dead2 = MaxPlus`MPDeadUnits[theNet, {{-0.5, 0.5}, {-0.5, 0.5}}];
Print["NeverActive units:  ", dead2["NeverActive"]];
Print["AlwaysActive units: ", dead2["AlwaysActive"]];

Print["--- MPPruneDeadUnits on dom = ", dom, " ---"];
pruned = MaxPlus`MPPruneDeadUnits[theNet, dom];
Print["original units: ", pruned["OriginalUnits"],
       ", kept units: ", pruned["KeptUnits"]];

Print["--- equivalence check ---"];
xtest = Transpose[{RandomReal[dom[[1]], 200],
                   RandomReal[dom[[2]], 200]}];
yOrig = TropicalReLU`ReLUForward[theNet, xtest];
prunedNet = <|"A" -> pruned["A"], "b" -> pruned["b"],
              "c" -> pruned["c"], "d" -> pruned["d"]|>;
headSlope = pruned["AffineHead", "Slope"];
yPruned   = TropicalReLU`ReLUForward[prunedNet, xtest]
              + (headSlope . # & /@ xtest);
err = Max[Abs[yOrig - yPruned]];
Print["max |f_original - f_pruned| over 200 random points in dom = ", err];
If[err > 10^-10,
  Print["[FAIL] pruning changes the function on dom"];
  Exit[1],
  Print["[ok] pruning is exact (or no units are dead on this dom)"]];
