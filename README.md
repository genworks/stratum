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

Curves use monotone cubic interpolation (Fritsch–Carlson). The curve
functions are *dual* (`source/dual.lisp`): each is an ordinary Lisp
function, and the same form is translated by Parenscript into
`/stratum/dual.js`, so the browser builds a curve's table while a point
is dragged exactly as the server does.

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
1080, seven layers), `?doc=parity` (the same stack at 320 × 200) or
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
| `POST /stratum/api/set?doc=N` | `{layer, slot, value}` for `opacity`, `visible`, `blend`; `{layer, slot: "curve", channel, points}` |

## Checks

`window.STRATUM.parity()` renders the document at 1:1 with `texelFetch`
into an offscreen target, reads it back and compares it with
`/stratum/api/cpu` pixel by pixel. `window.STRATUM.bench(n, curves?)`
draws `n` frames back to back with a pan step (and, with `curves?`, a
curve point moved and its table re-uploaded), finishing each frame on
the GPU.

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
an equivalent), multithreaded CPU rendering, and premultiplying before
mipmap generation to remove edge fringes when zoomed out.

## License

GNU Affero General Public License, version 3 or later. See `LICENSE`.
