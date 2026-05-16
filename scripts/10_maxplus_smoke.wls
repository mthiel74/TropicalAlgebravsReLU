#!/usr/bin/env wolframscript
(* Smoke tests for Kernel/MaxPlus.wl. *)

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{repoRoot, "Kernel", "MaxPlus.wl"}]];
Needs["MaxPlus`"];

assert[lbl_String, lhs_, rhs_] := Module[{ok = lhs === rhs},
  Print[If[ok, "[ok]", "[FAIL]"], "  ", lbl, "  (", lhs, " === ", rhs, ")"];
  If[!ok, Exit[1]]];

(* scalar ops *)
assert["3 ⊕ 7 = 7",       CirclePlus[3, 7], 7];
assert["3 ⊗ 7 = 10",      CircleTimes[3, 7], 10];
assert["a ⊕ -∞ = a",      CirclePlus[5, -Infinity], 5];
assert["a ⊗ 0 = a",       CircleTimes[5, 0], 5];

(* n-ary *)
assert["⊕ over list",     CirclePlus[1, 2, 3, 0], 3];
assert["⊗ over list",     CircleTimes[1, 2, 3], 6];

(* matrices *)
A = {{1, 2}, {3, 0}};
B = {{0, -1}, {2, 4}};
assert["A ⊙ B",
  CircleDot[A, B],
  {{4, 6}, {3, 4}}];

(* identity / power *)
I2 = MPIdentity[2];
assert["I ⊙ A = A",      CircleDot[I2, A], A];
assert["A^0 = I",        MPMatrixPower[A, 0], I2];
assert["A^2",            MPMatrixPower[A, 2],
                          CircleDot[A, A]];

(* Kleene star: longest paths in a 3-node graph (no positive cycles) *)
adj = {{-Infinity, 2, -Infinity}, {-Infinity, -Infinity, 3},
       {-Infinity, -Infinity, -Infinity}};
star = KleeneStar[adj];
Print["Kleene star = ", star];
assert["star[[1,3]] = 5 (1→2→3 = 2+3)", star[[1, 3]], 5];
assert["star[[1,1]] = 0 (id on diagonal)", star[[1, 1]], 0];

(* tropical determinant = permanent *)
M = {{1, 2}, {3, 4}};
assert["MPDet[{{1,2},{3,4}}] = max(1+4, 2+3) = 5",
  MPDeterminant[M], 5];

(* tropical eigenvalue / cycle mean of a simple cycle *)
cyc = {{-Infinity, 1, -Infinity},
       {-Infinity, -Infinity, 2},
       {3, -Infinity, -Infinity}};
Print["Cycle mean of 1→2→3→1 with weights {1,2,3} = ",
       MPCycleMean[cyc], "  (expected 2)"];
assert["MPEigenvalue of 1→2→3→1 = 2",
  N @ MPEigenvalue[cyc], 2.];

(* tropical polynomial *)
poly = TropicalPolynomial[{{0, 0}, {1, 0}, {0, 1}}, {0, -1, -2}];
Print["TropicalEval[max(0, x-1, y-2), (3,5)] = ",
       TropicalEval[poly, {3, 5}],
       "  (expected 3)"];
assert["TropicalEval (3,5) = 3", TropicalEval[poly, {3., 5.}], 3.];

Print["all max-plus smoke tests passed."];
