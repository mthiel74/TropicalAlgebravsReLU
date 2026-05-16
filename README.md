# Tropical Algebra of ReLU Networks

A ReLU neural network is **literally** a tropical rational map. The
unit operation of the tropical (max-plus) semiring is `a ⊕ b = max(a, b)`,
and `ReLU(x) = max(0, x)` is just `0 ⊕ x`. Every affine layer
`y = W x + b` followed by ReLU is a *tropical polynomial* in `x` with
classical real coefficients, and an entire L-layer ReLU network with
real-valued output is a *tropical rational map*:

```
f(x) = P(x) ⊖ Q(x)        (the symbol ⊖ here means classical subtraction
                           of two tropical polynomials)
```

where `P` and `Q` are convex piecewise-linear functions of the input. The
zero set `{P = Q}` is a **tropical hypersurface** — a finite, explicit
piecewise-linear arrangement that is also exactly the *decision boundary*
of the trained classifier.

This repository:

1. Ships a **self-contained `MaxPlus.wl` package** (Kernel/MaxPlus.wl) —
   tropical operators `⊕`, `⊗`, `⊖`, `⊙` attached to the System`
   symbols, tropical matrices and Kleene star, tropical
   determinant (= permanent), Karp's algorithm for the maximum cycle
   mean (= tropical eigenvalue), tropical polynomials with Newton
   polytopes, tropical curve plotting, plus a small library of
   classical applications (shortest paths, PERT scheduling, max-plus
   discrete-time linear systems).
2. Trains a tiny ReLU classifier on a 2D dataset in the Wolfram Language.
3. Rewrites the trained network in max-plus arithmetic.
4. Verifies that the two forward passes are numerically identical.
5. Extracts the tropical polynomials `P` and `Q` explicitly.
6. Visualises the decision boundary as the tropical hypersurface
   `{P = Q}`, overlaid on the input plane partitioned into linear
   regions by the network's activation patterns.
7. Plots the **Newton polytope** of `P` and `Q` and relates its edges
   to the pieces of the decision boundary.
8. Sweeps hidden-layer width and compares the empirical number of linear
   regions to the Zhang–Naitzat–Lim / Pascanu–Montúfar–Bengio upper
   bounds.
9. Derives the `P, Q` decomposition symbolically for the `2 → 1 → 1`
   and `2 → 2 → 1` networks, and includes pedagogical infographics
   on max-plus algebra, ReLU as `0 ⊕ x`, shortest paths as a min-plus
   mat-vec, and the Newton-polytope duality.

## Reference

Liwen Zhang, Gregory Naitzat, Lek-Heng Lim,
*Tropical Geometry of Deep Neural Networks*, ICML 2018.
[arXiv:1805.07091](https://arxiv.org/abs/1805.07091)

## Gallery

### Max-plus algebra side by side with classical algebra

![semiring card](figures/13_semiring_card.png)

### ReLU is the tropical sum `0 ⊕ x`

![ReLU is tropical](figures/14_relu_is_tropical.png)

### Shortest paths as a tropical min-plus mat-vec

![shortest paths infographic](figures/15_shortest_paths_infographic.png)

### Newton-polytope duality

![Newton-polytope duality](figures/16_newton_duality_infographic.png)

### A univariate tropical polynomial and its Newton polygon

![univariate tropical polynomial](figures/05_tropical_poly_1d.png)

### A bivariate tropical polynomial and its 4 linear regions

![bivariate tropical curve](figures/06_tropical_curve_2d.png)

### Shortest paths in a 5-node weighted digraph

![shortest paths worked example](figures/07_shortest_paths.png)

### PERT-style project scheduling via tropical Kleene star

![PERT scheduling](figures/08_pert_scheduling.png)

### Tropical eigenvalue (Karp's algorithm)

![Karp's eigenvalue](figures/09_karp_eigenvalue.png)

### Symbolic derivation: 2 → 1 → 1 ReLU network as a tropical rational

![analytical 2-1-1](figures/10_analytical_2-1-1.png)

### The decisive identity

![tropical product identity](figures/11_analytical_tropical_product.png)

### Symbolic derivation: 2 → 2 → 1 ReLU network

![analytical 2-2-1](figures/12_analytical_2-2-1.png)

### Forward pass equivalence (ReLU vs max-plus)

On three trained networks (XOR, moons, spirals), the maximum
discrepancy between the classical real-arithmetic forward pass and a
max-plus-only forward pass is about `10⁻¹⁴` over 1400+ points each.
Identical functions, two different algebras.

![forward equivalence](figures/01_forward_equivalence.png)

### Decision boundary as tropical hypersurface

Pastel patches are the activation-pattern regions cut out by the `n`
hidden-unit creases `aⱼ · x + bⱼ = 0`; the black `{f = 0}` ReLU
boundary and the red dashed `{P = Q}` max-plus hypersurface overlay
each other exactly.

![decision boundary](figures/02_decision_tropical_hypersurface.png)

### Newton polytope

Top row: monomials of `P` for the trained moons network, lifted to
`R³` and shaded with their upper hull, alongside the linear pieces `P`
realises on the input plane. Bottom row: same for `Q`. The
`upper-hull ↔ regular subdivision ↔ input-plane pieces` duality is
visible.

![Newton polytope](figures/03_newton_polytope.png)

### Linear regions vs hidden width

Sweeping `n ∈ {2, 4, 8, 16, 32, 64, 96}` and counting realised
activation patterns on a 400 × 400 grid:

![region count vs width](figures/04_region_count_vs_width.png)

The empirical curve tracks the Zaslavsky upper bound
`Z(n) = 1 + n + C(n, 2)` at slope `≈ 2` on log-log axes.

## Structure

```
TropicalAlgebravsReLU/
├── Kernel/
│   └── TropicalReLU.wl       core Wolfram Language library
├── scripts/                  reproducible .wls drivers
├── figures/                  generated PNGs / GIFs
├── notebooks/                exploratory Wolfram notebooks
├── community/                Wolfram Community post draft (.nb + assets)
├── PLAN.md                   simulation plan
└── README.md
```

## Status

Work in progress. See [PLAN.md](PLAN.md) for the simulation roadmap.
