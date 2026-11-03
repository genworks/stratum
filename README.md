# Stratum

A layered image editor in Common Lisp, built on Gendl. Status: first
spike. It has non-destructive layers, curves, masks and blend modes,
rendered on the GPU in the browser and checked against a CPU reference
renderer, and a brush with pressure, a live preview and undo.

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

## The brush

A stroke is its samples, (x, y, pressure) in document pixels from
Pointer Events (a pen's pressure; a mouse paints at full pressure),
and a brush: radius, hardness, spacing, flow, opacity, color and mode
(paint or erase). Two more dual functions (`source/brush.lisp`) do the
part both sides must agree on: `place-dabs` lays dabs along the stroke
by arc length, a dab every spacing × radius × pressure, and `paint-dab`
accumulates one dab's coverage into a float buffer, full inside radius
× hardness and falling smoothly to the edge, each dab raising a pixel's
coverage by flow times what it still lacks.

The browser previews with them: as samples arrive, the new dabs are
painted into a float coverage buffer the size of the document, each
frame uploads only the rectangle that changed into an `R32F` stroke
texture, and the generated shader composites the brush color through
that coverage onto the stroke's layer, before the layer blends. That
stage is a permanent part of every raster block, idle until a stroke
names its layer, so painting never recompiles anything. When the
pointer lifts, the samples go to the server, which runs the same two
functions, composites the coverage into the layer's pixels once in
linear light (one 8-bit rounding per stroke, not per dab) and answers
the dirty rectangle; the browser re-uploads that rectangle alone and
drops its preview. The server's pixels are the document.

A painted layer gets its own pixel buffer on its first stroke, so the
shared procedural pixels are never painted, and a `paint-version` slot
signals that the buffer changed: it feeds the layer's `pixels` and so
the CPU render, and never the plan. Undo keeps the rectangle each
change overwrote and puts the bytes back; it keeps what they replaced
for Redo, and any new change empties the redo list. A change of the
stack forgets both lists, since their layer indices would be stale.

Image ▸ Adjustments are the destructive versions of the three
adjustment layers: the same arithmetic, done once to one raster
layer's own pixels on the encoded scale, alpha untouched. While the
dialog is open the shader applies the adjustment to that layer alone,
through a third live stage (three spare rows of the table texture, or
hue/saturation parameters), and OK sends the settings to the server,
which bakes them into the bytes (`adjust-layer!`), puts the whole
layer on the undo list and answers it for re-upload.

A selection is an 8-bit coverage over the document with its bounding
rectangle, made on the server from a marquee rectangle, a lasso
polygon, All or Inverse (`source/selection.lisp`); nothing selected
paints everywhere. A stroke's coverage and a fill are multiplied by
it, on the server and in the browser with the same arithmetic, so
painting under a selection stays bit-exact between the two. The
shader draws the marching ants from the same bytes, never in an exact
render, since a selection is not part of the picture; the gesture
itself is drawn on a 2D overlay while the pointer is down. The Magic
Wand selects the pixels within a tolerance of the clicked one, by the
largest channel difference with alpha included, the connected patch
when contiguous and every one otherwise, on the active raster layer or
on the composited picture; with any selection tool Shift adds to the
selection and Alt takes from it. Feather softens the selection's edge
with a Gaussian blur of the mask. Edit ▸ Fill paints the selection
with the foreground, background, black, white or gray at an opacity,
and Clear empties it.

The Move tool drags the active layer's selected pixels (all of them
without a selection). While the pointer is down the shader's floating
stage shows them cut from where they were and composited at the
offset, from the textures already on the GPU, with the ants moving
along; on release the server does the same arithmetic once
(`move-pixels!`), the selection moves with the pixels, and the layer
comes back whole. Copy and Cut keep the selection's pixels on the
server's clipboard, shared by every document; Paste makes a new layer
of them above the active one, where they came from, or centered when
they do not fit.

Image ▸ Crop, Image Size and Canvas Size change the document's size,
which is an input of its whole tree, so each remakes the document
under its name (`source/resize.lisp`): every layer is first
materialized, its pixels and its mask as they are now, then cropped
to the selection, resampled (bilinear, in premultiplied linear light,
masks on their own scale) or placed on the new canvas around an
anchor, and the page reloads. A cropped document renders as exactly
that rectangle of the original, edits and all.

Filters bake into one raster layer inside the selection, with the
whole layer on the undo list, and preview through a replacement
texture: the viewer computes the filtered layer itself and the main
shader's fifth live stage swaps it in for the layer's pixels where the
selection covers. Gaussian Blur (`source/filter.lisp`) runs the
server's blur shader in two passes into float textures, rows then
columns, and the server's `gaussian-blur-rgba` is its twin: the same
discrete kernel over three sigmas, premultiplied linear light, edges
clamped, the intermediate in single floats as the GPU keeps it,
threaded across rows and then columns. Unsharp Mask adds a pass over
that blur: each color channel moved away from its blur by the amount
of the difference, where the difference exceeds the threshold. Add
Noise draws uniform noise for each pixel from an integer hash of its
coordinates, channel and a seed, written the same way in the shader
and in Lisp, so the preview and the bake draw the same numbers;
monochromatic noise uses one draw per pixel. A browser without float
render targets gets no preview and still bakes the filter on OK.

Free Transform (Ctrl+T) is the same floating stage with a matrix in
place of the offset: a box with eight handles around the selection or
the layer; drag inside to move, a handle to scale about the center
(Shift keeps the proportions), outside to turn; Enter or a
double-click commits, Escape cancels. The preview samples the copy
through the inverse matrix, bilinear over four texels in premultiplied
linear light with the selection as a weight, and the server resamples
the same way on commit (`transform-pixels!`), taking the selection
through the matrix too. The Move tool keeps its exact whole-pixel
path.

Masks paint the same way. Any layer can have one (Layer ▸ Layer Mask ▸
Reveal All or Hide All makes a white or black one; Delete takes it
away); clicking the mask's thumbnail in the Layers panel makes it the
brush's target, and the layer's thumbnail the pixels again. A stroke
on a mask moves it toward the brush color's luminance, so black hides,
white reveals, and the eraser paints the background color. The shader
has a second live-stroke stage for masks (`masked()`), the server's
twin is `composite-mask-stroke!`, and undo covers mask strokes too.

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

The menu bar is the one a classic image editor has, in its usual order
with its usual items and keys: File, Edit, Image, Layer, Select,
Filter, View, Window and Help. What is not built yet shows disabled,
as unavailable commands do. Built so far: File ▸ Open (Ctrl+O), Open
Sample, Place, Save (Ctrl+S), Save As PNG (Ctrl+Shift+S), Export As
PNG and Close; Edit ▸
Undo (Ctrl+Z), Redo (Ctrl+Shift+Z or Ctrl+Y), Cut (Ctrl+X), Copy
(Ctrl+C), Paste (Ctrl+V), Fill (Shift+F5), Clear (Delete) and Free
Transform (Ctrl+T); Image ▸ Adjustments ▸ Levels
(Ctrl+L), Curves (Ctrl+M) and Hue/Saturation (Ctrl+U), each a dialog
with a live preview that bakes the adjustment into the active raster
layer's pixels on OK; Image ▸ Image Size (Ctrl+Alt+I), Canvas Size
(Ctrl+Alt+C) and Crop; Select ▸ All (Ctrl+A), Deselect (Ctrl+D),
Inverse (Ctrl+Shift+I) and Feather (Shift+F6); Filter ▸ Blur ▸
Gaussian Blur, Sharpen ▸
Unsharp Mask and Noise ▸ Add Noise; Layer ▸ New ▸ Layer, Duplicate Layer,
Delete Layer, New Adjustment Layer (Levels, Curves, Hue/Saturation),
Layer Mask (Reveal All, Hide All, Delete), Arrange (Bring Forward,
Send Backward), Merge Down (Ctrl+E) and Flatten Image; View ▸ Zoom In,
Zoom Out,
Fit on Screen, Actual Pixels; Window ▸ Layers and Brush Settings.

The tool palette down the left keeps the order other editors use, with
the single-letter keys: Move (V), Marquee (M), Lasso (L), Magic Wand
(W), Crop (C), Eyedropper (I), Healing Brush (J), Brush (B), Clone
Stamp (S), History Brush (Y), Eraser (E), Gradient (G), Blur (R), Dodge
(O), Pen (P), Type (T), Path Selection (A), Shape (U), Hand (H) and
Zoom (Z). Built: Move, Marquee, Lasso, Magic Wand, Eyedropper, Brush,
Eraser, Hand and Zoom; the rest are drawn dimmed and refuse to be
picked. The foreground and
background swatches below them hold the brush color; X swaps them and
D resets them. The Layers panel lists the stack top first, each row an
eye for visibility, a thumbnail of the layer and of its mask, its name
and kind; the active layer's own controls sit below the list.

The keys are the usual ones: H is the hand, B the brush, E the eraser,
Space held pans whatever the tool, `[` and `]` change the brush size,
Ctrl+Z undoes the last stroke, Ctrl+Shift+N makes a layer, Ctrl+J
duplicates one, Ctrl+] and Ctrl+[ move it, and Ctrl+0, Ctrl+1, Ctrl++
and Ctrl+- are the views. Click a layer's name to make it the active
one; the brush paints on it. The panel has the brush's size, hardness,
flow, opacity and color.

Files go in through the browser, which decodes every image format it
knows, and exactly: an ImageBitmap with no premultiplication and no
color conversion, read back through a texture, so the bytes the server
gets are the file's. Open makes a new document of them, one Background
layer; Place adds them as a layer of the current document, centered
and cropped or padded to its size. Save As PNG and Export write the
flattened picture as a PNG on the server (`zpng`).

Save writes the document itself as a `.stratum` file, and Open reads
one back (`source/file.lisp`). The file is a line `STRATUM1`, a line
with the header's length, the header, one s-expression holding the
materialized layer specs with the pixel and mask bytes replaced by
blob references, and then the blobs, each a zlib stream (`salza2` to
write, `chipz` to read). The header is read with `*read-eval*` off.
Nothing else goes in: no undo, no selection, no clipboard. A saved
document loads back rendering byte for byte the same, edits, strokes
and masks included.

Merge Down and Flatten Image are the compositing the document already
does, pointed at part of the stack (`source/merge.lisp`): Merge Down
renders the active layer and the one below it, with their opacities,
blend modes and masks, over transparency into one raster layer that
takes the lower layer's place and name; Flatten renders the whole
document over its background into one Background layer, which then
renders exactly as the stack did. The CPU kernel takes the background's
alpha for this, 1 for a document as shown and 0 for a merge.

A change of the stack (a layer added, deleted, moved or duplicated)
goes through the document's `layer-specs`, which makes every layer
object anew; each layer first writes its current state back into its
spec (`spec-now`: name, opacity, visibility, mask, blend mode, curve
points, settings and painted pixels), so nothing is lost across the
change, and the plan, the programs and the layer panel follow.

## API

| path | answer |
|---|---|
| `/stratum/api/doc?doc=N` | JSON: size, plan version, shader source, layers |
| `/stratum/api/raster?doc=N&i=K` | raw RGBA8 of raster layer K; with `&x=&y=&w=&h=`, that rectangle |
| `/stratum/api/mask?doc=N&i=K` | raw R8 of mask K; with `&x=&y=&w=&h=`, that rectangle |
| `/stratum/api/thumb?doc=N&i=K&w=&h=` | a w × h nearest-neighbor thumbnail of raster layer K (RGBA8), or with `&mask=1` of mask K (R8) |
| `/stratum/api/cpu?doc=N` | raw RGBA8 of the CPU render |
| `/stratum/api/export.png?doc=N` | the CPU render as PNG |
| `/stratum/api/luts?doc=N` | JSON: the server's curve and levels tables |
| `POST /stratum/api/set?doc=N` | `{layer, slot, value}` for `opacity`, `visible`, `blend`; `{layer, slot: "setting", name, value}` for a levels or hue/saturation setting; `{layer, slot: "curve", channel, points}` |
| `POST /stratum/api/stroke?doc=N` | `{layer, target: "layer" or "mask", samples: [[x, y, pressure], ...], brush: {radius, hardness, spacing, flow, opacity, color, mode}}`; answers the dirty rectangle with the layer's texture or mask index and new version; with `&dry=1` paints nothing and answers the coverage buffer |
| `POST /stratum/api/undo?doc=N` | undoes the last change; answers the rectangle put back |
| `POST /stratum/api/redo?doc=N` | redoes the last undo; answers the rectangle put back |
| `POST /stratum/api/open?name=&w=&h=` | body: raw RGBA8; a new document of one Background layer; answers `{doc, width, height}` |
| `POST /stratum/api/place?doc=N&name=&w=&h=&above=` | body: raw RGBA8; a new layer of the document, centered and fitted to its size; answers as `layers` does |
| `POST /stratum/api/close?doc=N` | forgets the document |
| `/stratum/api/save?doc=N` | the document as a `.stratum` file |
| `POST /stratum/api/load?name=` | body: a `.stratum` file; a new document; answers `{doc, width, height}` |
| `POST /stratum/api/select?doc=N` | `{op: "rect", x, y, w, h}`, `{op: "polygon", points}`, `{op: "wand", x, y, tolerance, contiguous, layer}`, `{op: "feather", radius}`, `{op: "all"}`, `{op: "none"}` or `{op: "inverse"}`, with `mode: "new"`, `"add"` or `"subtract"` on the first three; answers `{selection: {x, y, w, h} or null}` |
| `/stratum/api/selection?doc=N` | raw R8 of the selection, empty when none |
| `POST /stratum/api/fill?doc=N` | `{layer, color, opacity, mode: "paint" or "erase"}` inside the selection; answers the rectangle as `stroke` does |
| `POST /stratum/api/move?doc=N` | `{layer, dx, dy}`: the selected pixels moved; answers the whole layer as a rectangle, with the selection's new rectangle |
| `POST /stratum/api/transform?doc=N` | `{layer, matrix: [a, b, c, d, e, f]}` (x′ = ax + by + c, y′ = dx + ey + f): the selected pixels resampled through it; answers the whole layer as a rectangle, with the selection's new rectangle |
| `POST /stratum/api/clipboard?doc=N` | `{op: "copy" or "cut", layer}` or `{op: "paste", above}`; Copy answers the clipboard's size, Cut the rectangle cleared, Paste the new layer as `layers` does |
| `POST /stratum/api/resize?doc=N` | `{op: "crop"}` (to the selection), `{op: "image-size", width, height}` or `{op: "canvas-size", width, height, anchor: [ax, ay]}`; the document is remade; answers `{width, height}` |
| `POST /stratum/api/filter?doc=N` | `{layer, kind: "gaussian-blur", "unsharp-mask" or "add-noise", settings: {radius, amount, threshold, monochrome, seed}}` baked into the layer inside the selection; answers the whole layer as a rectangle |
| `POST /stratum/api/adjust?doc=N` | `{layer, kind: "levels", "curves" or "hue-saturation", settings: {name: value}, curves: {channel: points}}` baked into the layer; answers the whole layer as a rectangle |
| `POST /stratum/api/layers?doc=N` | `{op: "add", kind, above}`, `{op: "delete", layer}`, `{op: "move", layer, delta}`, `{op: "duplicate", layer}`, `{op: "mask", layer, value: "reveal", "hide" or "delete"}`, `{op: "merge", layer}` or `{op: "flatten"}`; answers the new plan version and the layer to work on |

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
- the brush: dab spacing, a dab's shape and flow, the rectangle form of
  `paint-dab` against the whole-buffer form, that a stroke changes its
  rectangle only and rebuilds nothing, that the shared pixels are never
  painted, erasing, that undo gives the layer back exactly, that redo
  brings changes back exactly and in order, that a new change empties
  the redo list, and that undo and redo round trip through the whole
  list;
- masks: a stroke on a mask is refused where there is none, lowers the
  mask inside its rectangle only and rebuilds nothing, shows in the
  CPU render, and undoes exactly; Hide All, Reveal All and Delete; a
  painted mask surviving a change of the stack;
- adjustments baked in: identity curves change nothing, levels change
  the pixels and leave alpha alone, saturation -1 makes gray, an
  adjustment layer is refused, undo is exact, and the undo list keeps
  within its byte budget;
- selections: a rectangle's pixels and bounds, a square lasso equal
  to the rectangle, a triangle filled by pixel centers, an empty
  rectangle selecting nothing, Inverse and Deselect, a stroke under a
  selection changing pixels inside it only, clipping as the browser
  does it, Fill and Clear inside the selection with exact undo, and no
  rebuild;
- the wand: a contiguous wand taking the connected patch only, a
  non-contiguous one every matching pixel, the tolerance deciding
  what matches, the wand on a layer with Shift adding and Alt taking
  away, and Feather softening the edge and widening the bounds;
- the floating layer: a move without a selection shifting the whole
  layer and emptying what it left, a move under a selection moving
  those pixels only and the selection with them, undo, Copy and
  Paste putting the bytes back where they were on a new layer, Cut
  clearing them and undoing;
- transforms: the identity leaves the layer as it is, a whole-pixel
  translation transforms as a move does, a singular matrix is refused,
  the selection goes through the matrix, and a 2× scale keeps the
  interior opaque while the edge feathers;
- filters: a blurred dot spreads symmetrically with its center the
  brightest, a uniform layer blurs to itself, a blur changes the layer
  and rebuilds nothing, undoes exactly, and under a selection changes
  the selected pixels only; Unsharp Mask at no amount changes nothing,
  pushes the two sides of an edge apart and leaves the flat ends, and
  a threshold above the edge leaves it alone; the noise hash is in
  [0, 1) and differs by pixel, channel and seed; Add Noise at no
  amount changes nothing, draws the same numbers for a seed, and
  monochromatic noise moves the three channels together;
- merging: Merge Down folds a screened layer into the opaque one below
  within two levels of the picture and an adjustment layer into the
  raster below within three (the merged layer's one rounding to 8 bits
  through the stack above), refuses the bottom layer and a non-raster
  below, forgets the undo list, and Flatten renders exactly as the
  stack did into one Background;
- the native file: a saved document loads back rendering the same with
  its edits, strokes and masks, is smaller than its pixels, and a file
  that is not a Stratum file or a header that tries to evaluate is
  refused;
- sizes: placing pads and crops, resampling leaves the same size
  unchanged and keeps the ends in place when doubling, a mask
  resamples on its own scale, a cropped document renders as that
  rectangle of the original with its edits, a same-size Image Size
  renders the same, a larger canvas keeps the picture centered on a
  white border, a smaller one anchored top-left is that corner;
- the stack: a layer added above another, deleted, moved and
  duplicated, with painted pixels and edits surviving each change, a
  duplicate's pixels its own, a new layer transparent, and the
  programs following;
- files: an image fitted into another size is centered, padded or
  cropped, and an opened image renders back as it came;
- that a threaded render equals a single-threaded one;
- the FNV-1a checksums of the CPU renders against `tests/golden.sexp`,
  the sample documents and the parity document with a test stroke
  painted and erased.

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

The self-test checks:
- `parity()`: the document rendered at 1:1 with `texelFetch`, read back
  and compared pixel by pixel with `/stratum/api/cpu`, within one level;
- `lutCheck()`: the curve tables built in the browser equal the
  server's;
- a blend-mode edit and its undo, each of which must rebuild the
  program exactly once and leave parity intact;
- a rectangle re-upload, which must leave parity intact;
- the stroke round trip: a synthetic pen stroke through the brush tool
  is previewed, committed and its rectangle re-uploaded. Every sample
  must be recorded in document pixels; `strokeCheck()` must find the
  browser's coverage equal to the server's (`stroke?dry=1`) to the
  last bit; the preview must match the committed picture within two
  levels (the commit rounds the layer to 8 bits once, which the
  preview did not); parity must hold afterwards; undo must give the
  picture back byte for byte; and redo must bring the stroke back
  exactly;
- the stack through the menus: Layer ▸ New Adjustment Layer ▸ Curves
  and then Delete Layer, each a new plan with parity intact, and the
  stack as found;
- the palette: a tool not built is refused, the eyedropper picks the
  color under the pointer, X swaps the swatches, and a layer's
  thumbnail comes from the server;
- files: an image drawn in the browser and encoded as a PNG decodes
  back byte for byte, lands centered on its own layer when placed and
  reads back from the server byte for byte, and makes a new document
  of its size when opened;
- masks: Hide All on the active layer makes a mask the target and
  hides the layer with parity intact; a white stroke on the mask
  reveals some of it, the preview matching the commit; undo; Delete
  gives the picture back;
- adjustments: Levels on the bottom layer previews on that layer
  alone, OK bakes what the preview showed, parity holds, and undo is
  exact;
- selections: a marquee rectangle through the tool, a stroke under it
  matching the server's coverage to the last bit and changing nothing
  outside it, Fill inside it, undo, Inverse and Deselect; then the
  Magic Wand selecting a band of the Sky gradient, Shift-click adding
  a second, and Feather widening the bounds;
- the floating layer: a rectangle dragged with the Move tool,
  previewed through the shader and committed within two levels of the
  preview with the selection moving along, parity, undo; Copy and
  Paste making a layer above with the same bytes where they came from;
  Cut clearing them; undo;
- Gaussian Blur on the Discs layer: previewed through the blur passes
  and the replacement stage, OK baking what the preview showed within
  two levels, parity, undo exact; then Unsharp Mask and monochromatic
  Add Noise on the same layer, each baked within two levels of its
  preview, parity, undo exact;
- Free Transform: a rectangle scaled, turned and moved, previewed and
  committed within two levels, the selection following, parity, undo,
  and Escape leaving the picture alone;
- sizes, on a throwaway document opened from a small image: Crop,
  Image Size and Canvas Size each remake it at the new size; the
  Image Size dialog opens and cancels;
- merging, on a throwaway document: an image placed on another and
  merged down leaves one raster layer with the lower name, and a
  curves layer added then flattened leaves one Background;
- the native file: Save brings a Stratum file of the document, and
  loaded back as a new document it has the same stack and renders
  byte for byte the same on the server.

The panel's Benchmark button (or `STRATUM.bench(n, curves?)`) draws
frames back to back with a pan step and a curve-point drag, and reports
the time per frame. Headless SwiftShader numbers only prove the code
path; real frame rates come from a real GPU.

## Open pieces

These are stubbed in `source/stubs.lisp`, each with its contract in its
docstring:

1. **Tiled layer storage** (`make-tile-store`). Copy-on-write tiles in
   foreign memory, undo as shared versions, and virtual texturing on
   the GPU once a document no longer fits in a texture array.
   Acceptance test: 100 undo steps on a 4K document cost no more than
   the tiles they touched, and a 40-layer 4K document still pans at
   60 fps.
2. **ICC color management** (`convert-with-profile`). LittleCMS over
   CFFI, so images are not all assumed to be sRGB.

The brush engine was the first open piece and is built (`source/brush.lisp`);
its first acceptance test, that a stroke painted in the browser and
reloaded from the server matches, is the self-test's stroke round trip.
Its second, 60 fps with a 300 px brush on a 4K layer, is kept by
design: the preview touches only the rectangle each frame's dabs
changed, with no whole-layer upload, but the frame rate itself is a
real-GPU measurement still to take. What the brush still lacks: a
rectangle's re-upload rebuilds the mipmaps of the whole texture array
(a per-region rebuild if a commit's timing shows it); the first stroke
on a 4K layer copies the layer's 33 MB once; undo keeps whole
rectangles until the tile store exists; and the dab is a round, soft
disc, with no texture, tilt or scatter yet.

Smaller items still to do: per-session documents (today one document is
shared per name), a server-side PNG reader for files on disk (`chipz`
is loaded now), and premultiplying before mipmap generation to remove
edge fringes when zoomed out.  The CPU render runs on
`*render-threads*` threads (8 by default): 2 s for the 4K twenty-layer
document, against 10 s on one thread.

## License

GNU Affero General Public License, version 3 or later. See `LICENSE`.
