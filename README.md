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

1. Trains a tiny ReLU classifier on a 2D dataset in the Wolfram Language.
2. Rewrites the trained network in max-plus arithmetic.
3. Verifies that the two forward passes are numerically identical.
4. Extracts the tropical polynomials `P` and `Q` explicitly.
5. Visualises the decision boundary as the tropical hypersurface
   `{P = Q}`, overlaid on the input plane partitioned into linear
   regions by the network's activation patterns.
6. Plots the **Newton polytope** of `P ⊕ Q` and relates its edges to
   the pieces of the decision boundary.
7. Sweeps hidden-layer width and compares the empirical number of linear
   regions to the Zhang–Naitzat–Lim / Pascanu–Montúfar–Bengio upper
   bounds.

## Reference

Liwen Zhang, Gregory Naitzat, Lek-Heng Lim,
*Tropical Geometry of Deep Neural Networks*, ICML 2018.
[arXiv:1805.07091](https://arxiv.org/abs/1805.07091)

## Gallery

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
