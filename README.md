# Stratum

A layered image editor in Common Lisp, built on Gendl. Status: first
spike. It has non-destructive layers, curves, masks and blend modes,
rendered on the GPU in the browser and checked against a CPU reference
renderer. It does not paint yet.

## The idea

A non-destructive layer stack is a dependency graph that recomputes
lazily and caches what it can, and that is what a Gendl `define-object`
tree already is. Every layer setting is a slot. The settings that change
the compositing *program* (a layer's kind, its blend mode, whether it
has a mask, its place in the stack) feed one slot, `plan`. Only `plan`
feeds the two code generators:

- `shader-source`: one GLSL ES 3.00 fragment shader that composites the
  whole stack in a single pass (`source/glsl.lisp`);
- `cpu-kernel`: the same plan as an unrolled lambda handed to
  `compile` (`source/cpu.lisp`), used as the reference and for export.

Opacity, visibility and curve points feed uniform data only. Dragging a
slider never recompiles anything, and changing a blend mode rebuilds
each program exactly once, without any code saying when to.
`build-counts` on the document records both.

There are four kinds of layer, each its own class on an `image-layer`
mixin (the document picks them with `:type (:sequence LIST)`):

- **raster**: pixels and a blend mode;
- **curves**: a master curve and red, green and blue curves;
- **levels**: input black and white points, gamma, and output black
  and white points;
- **hue/saturation**: hue, saturation and lightness, in HSL.

Curves and levels become per-channel tables (`lut-layer`). Hue and
saturation travel in a four-number uniform for each layer
(`u_params`). Every layer can carry a mask.

Curves use monotone cubic interpolation (Fritsch–Carlson). The table
functions are *dual* (`source/dual.lisp`): each is an ordinary Lisp
function, and the same form is translated by Parenscript into
`/stratum/dual.js`. So the browser builds a table while a point or a
slider is dragged exactly as the server does, equal to the last bit.

Compositing happens in linear light on premultiplied color, following
the W3C Compositing and Blending formulas (normal, multiply, screen,
add). Raster layers are stored as sRGB RGBA8 with straight alpha. The
GPU decodes them through `SRGB8_ALPHA8` textures, and the CPU through a
256-entry table.

## Running it

In a Gendl image (an SBCL or CCL room):

```lisp
(load "/projects/gw/stratum/dev.lisp")
```

Then browse `/stratum/` on that image's HTTP port: `?doc=demo` (1920 ×
1080, nine layers), `?doc=parity` (the same stack at 320 × 200) or
`?doc=stress` (3840 × 2160, twenty layers). Drag to pan, use the wheel
to zoom and double-click to fit. In a curves editor, drag a point, click
empty space to add a point, or double-click a point to remove it.

## API

| path | answer |
|---|---|
| `/stratum/api/doc?doc=N` | JSON: size, plan version, shader source, layers |
| `/stratum/api/raster?doc=N&i=K` | raw RGBA8 of raster layer K |
| `/stratum/api/mask?doc=N&i=K` | raw R8 of mask K |
| `/stratum/api/cpu?doc=N` | raw RGBA8 of the CPU render |
| `/stratum/api/export.png?doc=N` | the CPU render as PNG |
| `/stratum/api/luts?doc=N` | JSON: the server's curve and levels tables |
| `POST /stratum/api/set?doc=N` | `{layer, slot, value}` for `opacity`, `visible`, `blend`; `{layer, slot: "setting", name, value}` for a levels or hue/saturation setting; `{layer, slot: "curve", channel, points}` |

## Tests

The Lisp half, in any Gendl image with Stratum loaded:

```lisp
(load "/projects/gw/stratum/tests/run.lisp")
(stratum::run-tests)   ; => T when every check passed
```

It checks:
- which edits rebuild the programs (an opacity, curve or visibility
  edit rebuilds nothing; a blend edit rebuilds each program once);
- that each spec's kind picks its layer type;
- the properties of the curve tables;
- that a threaded render equals a single-threaded one;
- the FNV-1a checksums of the CPU renders against `tests/golden.sexp`.

The checksums must come out the same on every implementation (SBCL and
CCL agree today). CI (`.gitlab-ci.yml`) runs this half on every push,
in the published `genworks/gendl` images for CCL and SBCL, through
`ci/run-tests.lisp`. That script loads Stratum on a cold image and fails
the job on any warning while Stratum loads, or on any failed check. Run `(stratum::record-golden)` only for a change that
is meant to alter the pictures, and say so in its commit message.

The browser half runs `STRATUM.selfTest()` in headless Chromium
(SwiftShader) against a published image, for each document named:

```sh
BASE=http://engine-room:9090 tests/browser.sh parity demo
```

The self-test checks three things:
- `parity()`: the document rendered at 1:1 with `texelFetch`, read back
  and compared pixel by pixel with `/stratum/api/cpu`, within one level;
- `lutCheck()`: the curve tables built in the browser equal the
  server's;
- a blend-mode edit and its undo, each of which must rebuild the
  program exactly once and leave parity intact.

The panel's Benchmark button (or `STRATUM.bench(n, curves?)`) draws
frames back to back with a pan step and a curve-point drag, and reports
the time per frame. Headless SwiftShader numbers only prove the code
path; real frame rates come from a real GPU.

## Open pieces

These are stubbed in `source/stubs.lisp`, each with its contract in its
docstring:

1. **The brush engine** (`apply-brush-stroke`). Dab placement and
   spacing, pressure from Pointer Events, linear-light compositing, a
   live preview in the browser that uses the same dab code as the
   server, and dirty tiles sent back. Acceptance test: a stroke painted
   in the browser and then reloaded from the server matches pixel for
   pixel, and painting stays at 60 fps with a 300 px brush on a 4K
   layer.
2. **Tiled layer storage** (`make-tile-store`). Copy-on-write tiles in
   foreign memory, undo as shared versions, and virtual texturing on
   the GPU once a document no longer fits in a texture array.
   Acceptance test: 100 undo steps on a 4K document cost no more than
   the tiles they touched, and a 40-layer 4K document still pans at
   60 fps.
3. **ICC color management** (`convert-with-profile`). LittleCMS over
   CFFI, so images are not all assumed to be sRGB.

Smaller items still to do: per-session documents (today one document is
shared per sample name), loading real files (PNG through `png-read` or
an equivalent), and premultiplying before mipmap generation to remove
edge fringes when zoomed out.  The CPU render runs on
`*render-threads*` threads (8 by default): 2 s for the 4K twenty-layer
document, against 10 s on one thread.

## License

GNU Affero General Public License, version 3 or later. See `LICENSE`.
