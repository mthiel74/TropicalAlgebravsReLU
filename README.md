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
