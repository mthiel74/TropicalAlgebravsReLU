# Tropical Algebra of ReLU Networks

A ReLU neural network is **literally** a tropical rational map. This
post takes that statement seriously: we train a tiny 2D classifier in
the Wolfram Language, rewrite its forward pass using nothing but `max`
and `+`, and show that its decision boundary is a *tropical
hypersurface* — a finite, piecewise-linear arrangement whose pieces are
explicit. We also reproduce the `n²` linear-region scaling predicted by
Zhang, Naitzat, and Lim (ICML 2018) and by Zaslavsky's classical bound
on hyperplane arrangements.

## 1. Why ReLU is tropical

The tropical (max-plus) semiring is the real numbers, extended with
`−∞`, equipped with

```
a ⊕ b := max(a, b)        (tropical addition)
a ⊗ b := a + b            (tropical multiplication)
```

The identity for `⊕` is `−∞`, the identity for `⊗` is `0`. The ReLU
activation is then literally

```
ReLU(x) = max(0, x) = 0 ⊕ x,
```

i.e. tropical addition of `x` and the multiplicative identity `0`.
An affine map followed by ReLU,

```
h(x) = max(0, a · x + b),
```

is a *tropical polynomial of degree 1* in `(x₁, x₂)`. A 1-hidden-layer
ReLU classifier

```
f(x) = Σⱼ cⱼ max(0, aⱼ · x + bⱼ) + d
```

splits along the sign of `cⱼ` into

```
f(x) = P(x) − Q(x),
```

with `P` and `Q` both convex piecewise-linear — i.e. *tropical
polynomials*. The function `f` is therefore a *tropical rational map*,
and its decision boundary `{f = 0}` is the **tropical hypersurface**
`{P = Q}`.

## 2. The two forward passes

```mathematica
fReLU[x_]    := c . Map[Max[0, #] &, A . x + b] + d;
fMaxPlus[x_] := Module[{P, Q},
  P = Sum[If[c[[j]] > 0,
              Max[0, c[[j]] (A[[j]] . x) + c[[j]] b[[j]]], 0],
           {j, Length[c]}] + Max[d, 0];
  Q = Sum[If[c[[j]] < 0,
              Max[0, -c[[j]] (A[[j]] . x) - c[[j]] b[[j]]], 0],
           {j, Length[c]}] + Max[-d, 0];
  P - Q
];
```

On three trained networks (XOR, two-moons, two-spirals), the maximum
absolute discrepancy over 1400+ points is about `10⁻¹⁴`:

![forward equivalence](../figures/01_forward_equivalence.png)

The two evaluators are *the same function* expressed in two different
algebras.

## 3. Decision boundary = tropical hypersurface

The hidden units cut the input plane along `n` straight crease lines
`aⱼ · x + bⱼ = 0`. On each open region of the resulting arrangement,
the activation pattern `σ ∈ {0,1}ⁿ` is constant, and `f` is affine.
The decision boundary `{f = 0}` is the piecewise-linear arrangement
where two convex piecewise-linear functions `P` and `Q` meet — the
tropical hypersurface `{P = Q}`:

![decision boundary](../figures/02_decision_tropical_hypersurface.png)

The black ReLU boundary and the red dashed `{P = Q}` overlay each
other exactly. The pastel patches are the activation-pattern regions.

## 4. Newton polytope

Every tropical polynomial `P(x) = max_i (aᵢ · x + cᵢ)` has a Newton
polytope `Newt(P) = conv{aᵢ}` in `R²`, and the upper convex hull of
the lifted points `(aᵢ, cᵢ)` in `R³` induces a regular subdivision of
`Newt(P)`. That subdivision is **dual** to the linear-region
decomposition of input space (Maclagan & Sturmfels, *Introduction to
Tropical Geometry*, Ch. 3).

![Newton polytope](../figures/03_newton_polytope.png)

Top row: `P` for the trained moons network — 7 monomials but only 4 on
the upper hull, hence only 4 visible pieces. Bottom row: `Q`, 7
monomials, 5 on the upper hull. Interior lifted points are dominated
by the upper envelope and never attain the max.

## 5. Linear regions scale like `n²`

By Zaslavsky's theorem, `n` lines in general position in `R²` cut the
plane into at most

```
Z(n) = 1 + n + C(n, 2) = (n² + n + 2) / 2
```

open regions. Sweeping the hidden width `n ∈ {2, 4, 8, 16, 32, 64, 96}`
on two-moons (three seeds per width) and counting realised activation
patterns on a 400 × 400 grid:

![region count vs width](../figures/04_region_count_vs_width.png)

The empirical curve tracks the Zaslavsky bound at slope ≈ 2 on log-log
axes (so `R(n) ∼ n²`). The trained networks sit at roughly `0.5–0.95`
of the bound — they don't realise the maximal arrangement, but they
live in the same asymptotic class.

## 6. Putting it together

The structural takeaway: every object the post talks about lives in
both pictures simultaneously.

| classical / ReLU view              | tropical / max-plus view                   |
|------------------------------------|---------------------------------------------|
| `max(0, Wx + b)` per layer         | tropical polynomial in `x`                  |
| scalar output `f(x) ∈ R`           | `P(x) − Q(x)` with `P, Q` tropical polys    |
| decision boundary `{f = 0}`        | tropical hypersurface `{P = Q}`             |
| activation crease lines            | regular subdivision of `Newt(P ⊕ Q)`        |
| region count vs width              | Zaslavsky `Z(n) = 1 + n + C(n, 2)`          |
| region count vs depth (future)     | products of tropical polynomials ~ `nᴸ`     |

## Reference

Liwen Zhang, Gregory Naitzat, Lek-Heng Lim.
*Tropical Geometry of Deep Neural Networks*. ICML 2018.
[arXiv:1805.07091](https://arxiv.org/abs/1805.07091)

## Code

All the code and the buildable notebook are in the repo:
<https://github.com/mthiel74/TropicalAlgebravsReLU>
