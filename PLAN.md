# Simulation plan

The end product is a Wolfram Community post showing that a ReLU
neural network is literally a tropical rational map, with all
intermediate objects (tropical polynomials, Newton polytope, linear
regions, decision boundary) made explicit. The figures below are the
ones the post will ultimately contain.

## The mathematical setup we keep coming back to

Let `x ∈ ℝ²` (2D inputs throughout for visualisation).
A single hidden ReLU layer with `n` units produces
```
h_j(x) = max(0, a_j · x + b_j),     j = 1, …, n.
```
A scalar-output classifier on top reads
```
f(x) = Σ_j c_j h_j(x) + d.
```
Splitting positive and negative output weights `c_j = c_j⁺ − c_j⁻`,
```
f(x) = P(x) − Q(x),
P(x) = d⁺ + Σ_{j : c_j > 0}      c_j   max(0, a_j · x + b_j)
Q(x) = d⁻ + Σ_{j : c_j < 0}    (−c_j)  max(0, a_j · x + b_j).
```
Both `P` and `Q` are convex piecewise-linear, i.e. **tropical
polynomials** in `(x₁, x₂)` with classical real exponents and classical
real coefficients. `f` is their classical difference — a tropical
rational map. The decision boundary `{f = 0}` is the **tropical
hypersurface** `{P = Q}`.

The input plane is partitioned by the `n` lines `a_j · x + b_j = 0`
into open regions on each of which `f` is affine. The pattern
`σ ∈ {0,1}ⁿ` of which `h_j` are active is constant on each region.
This is the *activation-pattern arrangement*.

## Experiments

### 1. Forward-pass equivalence (sanity check)

Train a `2 → n → 1` ReLU classifier with `NetTrain`. Implement an
independent max-plus evaluator that consumes the trained weights and
returns the same scalar via `max`/`+` only. Verify equality on a random
batch and on a meshgrid.

*Deliverable:* `figures/01_forward_equivalence.png` — scatter of
`f_ReLU(x)` vs `f_maxplus(x)` on the diagonal, plus a `Max[|Δ|]` print.

### 2. Tropical polynomial decomposition

Extract the monomial list of `P` and `Q`. Each ReLU unit `j` with
output weight `c_j` contributes the monomial pair
```
(a_j, b_j)  with classical coefficient |c_j|   in P (if c_j > 0) or Q (if c_j < 0).
```
The function on a region with active set `S` is
```
P(x) − Q(x) = Σ_{j ∈ S, c_j>0} c_j(a_j·x+b_j) − Σ_{j ∈ S, c_j<0} (−c_j)(a_j·x+b_j) + d.
```

*Deliverable:* `figures/02_tropical_monomials.png` — print of `P`, `Q`
as max-of-affine forms next to a `ContourPlot[f, …]` of the trained
network.

### 3. Decision boundary as tropical hypersurface

Plot the input plane with:
* the `n` lines `a_j · x + b_j = 0` (ReLU "creases") in light grey,
* coloured patches for distinct activation patterns,
* the curve `{f = 0}` in black,
* the curve `{P = Q}` in red dashed (should overlay exactly).

*Deliverable:* `figures/03_decision_tropical_hypersurface.png`.

### 4. Newton polytope

A tropical polynomial `P = max_α (α · x + cα)` has a *Newton polytope*
`Newt(P) = conv{α}` lifted by `cα`. The faces of its upper convex hull
project to the linear regions of `P` (Maclagan–Sturmfels Ch. 3). For
the classifier we use the regular subdivision induced by the lifts
`cα` to draw the dual decomposition of the input plane.

*Deliverable:* `figures/04_newton_polytope.png` — 3D lift of the
monomials, upper hull shaded, dual subdivision in the input plane.

### 5. Linear region scaling

Sweep hidden width `n ∈ {2, 4, 8, 16, 32, 64}` and count empirical
linear regions of the **trained** network by enumerating sign patterns
of `{a_j · x + b_j}` over a fine grid. Compare to:
* the Zaslavsky upper bound `Σ_{k=0}^{2} C(n, k)` for `n` lines in the
  plane (input dim = 2),
* the Pascanu–Montúfar–Bengio / Zhang–Naitzat–Lim bounds.

*Deliverable:* `figures/05_region_count_vs_width.png`.

### 6. Depth experiment (stretch)

Repeat with a `2 → n → n → 1` ReLU MLP. The tropical rational map then
has `O(n²)` linear regions, and `P, Q` become *iterated* tropical
polynomials. We do this if time allows.

*Deliverable:* `figures/06_depth_regions.png`.

## Datasets

All 2D, synthetic:

* **XOR** — four Gaussian blobs at `(±1, ±1)`, two classes by sign of `x₁ x₂`.
* **moons** — `sklearn`-style two-moons re-implemented in WL.
* **spirals** — two interleaved Archimedean spirals, the hardest of the
  three for a 1-hidden-layer ReLU network.

## What "ReLU vs max-plus" actually compares

There is no numerical difference between the two — that is the point.
The comparison is *structural*:

| Aspect                      | ReLU view                         | max-plus view                                |
|-----------------------------|-----------------------------------|----------------------------------------------|
| Forward pass                | `max(0, W x + b)` per layer       | tropical polynomial in `x`                   |
| Output                      | scalar `f(x) ∈ ℝ`                 | `P(x) − Q(x)` with `P, Q` tropical polys     |
| Decision boundary           | level set `{f = 0}`               | tropical hypersurface `{P = Q}`              |
| "Where does it bend?"       | activation crease lines           | regular subdivision of `Newt(P ⊕ Q)`         |
| Region count vs width       | empirical                         | Zaslavsky / Zhang–Naitzat–Lim upper bound    |
| Region count vs depth       | empirical                         | products of tropical polynomials → `~ n^L`   |
