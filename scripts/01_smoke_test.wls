#!/usr/bin/env wolframscript
(* Smoke test for the TropicalReLU package:
   construct a random tiny network and verify that
   ReLUForward and MaxPlusForward agree numerically. *)

repoRoot = DirectoryName[$InputFileName] /. "" :> Directory[];
repoRoot = ParentDirectory[repoRoot];

Get[FileNameJoin[{repoRoot, "Kernel", "TropicalReLU.wl"}]];

SeedRandom[42];
net = <|
  "A" -> RandomReal[{-1, 1}, {6, 2}],
  "b" -> RandomReal[{-1, 1}, 6],
  "c" -> RandomReal[{-1, 1}, 6],
  "d" -> RandomReal[{-0.2, 0.2}]
|>;

xs = RandomReal[{-2, 2}, {200, 2}];
yRelu = TropicalReLU`ReLUForward[net, xs];
yMaxPlus = TropicalReLU`MaxPlusForward[net, xs];

err = Max[Abs[yRelu - yMaxPlus]];
Print["max |ReLU - max-plus| = ", err];
If[err < 10^-10,
  Print["[ok] forward passes agree to machine precision"],
  (Print["[FAIL] forward passes disagree"]; Exit[1])
];

(* Activation pattern + region count *)
nRegions = TropicalReLU`LinearRegionCount[net,
  "Domain" -> {{-2, 2}, {-2, 2}}, "Resolution" -> 200];
Print["empirical linear regions on [-2,2]^2, n=6 hidden: ", nRegions];

(* Tropical monomials *)
dec = TropicalReLU`TropicalDecomposition[net];
Print["|P monomials| = ", Length[dec["Pmon"]],
      ", |Q monomials| = ", Length[dec["Qmon"]],
      ", Pconst = ", dec["Pconst"], ", Qconst = ", dec["Qconst"]];
