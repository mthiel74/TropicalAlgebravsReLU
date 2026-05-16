#!/usr/bin/env python3
"""Generate four illustrative cover images for the Wolfram Community post
using OpenAI's gpt-image-2.

The images are PURELY visual concept illustrations -- no equations, no
labels, no math symbols are written into them. All mathematics lives in
the notebook itself.

Outputs to figures/13_*.png through figures/16_*.png, overwriting the
older WL-rendered "infographic" versions.
"""

from __future__ import annotations

import base64
import os
import sys
import time
from pathlib import Path

from openai import OpenAI

REPO_ROOT = Path(__file__).resolve().parent.parent
FIG_DIR = REPO_ROOT / "figures"
FIG_DIR.mkdir(exist_ok=True)

# Prompts.  Each prompt:
#  - describes a VISUAL scene (what the image should show)
#  - explicitly forbids equations, formulas, text, labels
#  - uses a consistent house style so the four images feel like a series

HOUSE_STYLE = (
    " Editorial science-magazine illustration style. "
    "Clean, contemporary, slightly stylised, with soft natural lighting "
    "and a restrained two-or-three colour palette of muted blues, "
    "warm sand, and a single accent of coral or amber. "
    "NO TEXT, NO LETTERS, NO NUMBERS, NO EQUATIONS, NO MATH SYMBOLS, "
    "NO LABELS of any kind anywhere in the image."
)

PROMPTS = {
    "13_semiring_landscapes.png": (
        "A diptych: two parallel landscapes shown side by side as if "
        "looking through twin windows. The LEFT landscape is smooth, "
        "rolling green hills under a soft pastel sky, with gentle "
        "curves -- a metaphor for classical, continuous arithmetic. "
        "The RIGHT landscape, of the same scale, is a sharp folded "
        "origami mountain range made of crisp planar facets meeting at "
        "knife-edge ridges -- a metaphor for the tropical (max-plus) "
        "world where every operation produces a piecewise-linear surface. "
        "The two windows share a subtle frame and a small bridge "
        "connecting them. Symmetric composition, no people."
        + HOUSE_STYLE
    ),
    "14_relu_ridge.png": (
        "A single, very large origami-style ramp emerging out of a flat "
        "plane. The ramp has a perfectly sharp horizontal section that "
        "smoothly continues into a perfectly sharp inclined section, "
        "forming a single 'kink' where the slope changes. The horizontal "
        "section is in shadow, the inclined section catches warm light. "
        "Below the structure, a faint gridded landscape suggests the "
        "input domain. The shape is the entire focus -- abstract, "
        "geometric, beautiful in its simplicity, NOT a graph or chart."
        + HOUSE_STYLE
    ),
    "15_graph_paths.png": (
        "An aerial view of a stylised abstract network: small spheres "
        "(nodes) connected by glowing curved lines (edges) across a "
        "dark navy background. Most edges are dim; a small number of "
        "edges form a clearly highlighted, glowing path through the "
        "network from one corner to another, in a warm coral hue, like "
        "current flowing along the cheapest route between two points. "
        "Other 'cheaper' routes are slightly less bright. Abstract, "
        "elegant, like a microchip viewed from above."
        + HOUSE_STYLE
    ),
    "16_decision_terrain.png": (
        "A bird's-eye-view of an abstract topographic terrain where a "
        "scattering of small coral dots and a scattering of small blue "
        "dots live on opposite sides of a piecewise-linear crease that "
        "winds across the plane. The crease itself is a single, sharp, "
        "polygonal curve made of several straight segments meeting at "
        "vertices -- visibly piecewise-linear, NOT a smooth curve. The "
        "terrain is divided into faint pastel regions by the segments, "
        "as if a topographic map had been folded along straight lines. "
        "No labels, no axes."
        + HOUSE_STYLE
    ),
}

SIZE = "1536x1024"
MODELS_TO_TRY = ["gpt-image-2", "gpt-image-1.5", "gpt-image-1"]


def make_client() -> OpenAI:
    return OpenAI()


def generate(client: OpenAI, prompt: str) -> bytes:
    last_err: Exception | None = None
    for model in MODELS_TO_TRY:
        try:
            print(f"  -- trying model {model!r}")
            resp = client.images.generate(
                model=model,
                prompt=prompt,
                size=SIZE,
                n=1,
            )
            data = resp.data[0]
            if getattr(data, "b64_json", None):
                return base64.b64decode(data.b64_json)
            if getattr(data, "url", None):
                import urllib.request

                with urllib.request.urlopen(data.url) as r:
                    return r.read()
            raise RuntimeError("response did not include image bytes or URL")
        except Exception as e:  # noqa: BLE001
            last_err = e
            print(f"     model {model!r} failed: {e}")
            time.sleep(1.0)
            continue
    assert last_err is not None
    raise last_err


def main() -> int:
    client = make_client()
    for filename, prompt in PROMPTS.items():
        out = FIG_DIR / filename
        print(f"[{filename}]  generating...")
        bytes_ = generate(client, prompt)
        out.write_bytes(bytes_)
        print(f"  -> wrote {out.relative_to(REPO_ROOT)}  ({len(bytes_)} bytes)")
    print("all four illustrations generated.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
