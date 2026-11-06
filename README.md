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
add, and color: the layer's hue and saturation at the luminance of
what is under it, the one mode no channel decides alone). Raster
layers are stored as sRGB RGBA8 with straight alpha. The
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

The Gradient tool (G) lays a ramp between two colors over the active
raster layer inside the selection: drag from where the first color
ends to where the second begins, Shift holding the drag to a multiple
of 45 degrees. The ramp is the classic one, the two colors
interpolated on the encoded scale so a half-way gray is 128, then
composited over the layer in premultiplied linear light, so a ramp to
transparency fades the way a brush stroke does. Five shapes: linear,
reflected, radial, diamond and angle; presets Foreground to
Background, Foreground to Transparent and Black, White; an opacity;
Reverse. While the pointer drags, pass 4 of the filter shader lays the
ramp over the layer into the replacement texture, so the preview is
live and exact; when it lifts the server bakes the same ramp with
`gradient-rgba` (`source/gradient.lisp`), the twin of that pass, the
whole layer on the undo list.

The Clone Stamp (S) is the brush with the layer's own pixels for
paint: Alt-click sets the source, and a stroke paints what lies at the
offset from the source to where the stroke began, as it was before the
stroke, transparent beyond the layer, the source's alpha scaling the
coverage; Aligned, the default, keeps that offset for every later
stroke, otherwise each stroke starts from the source again. A
crosshair over the pixels being read follows the pointer. The live
stroke stage samples the layer texture at the offset for its preview,
and the server's `composite-clone-stroke!` is its twin, reading from a
copy of the source rectangle taken before the stroke lands.

The Healing Brush (J) is the stamp's sampling with the destination's
tone kept: the paint at each pixel is the destination's blur plus what
the source rises above its own blur, in linear light, so a blemish
painted over from clean skin nearby takes the skin's grain and keeps
the local shading, where the stamp would paste the source's color as
it is. The blur's sigma is half the brush radius, at least one pixel.
The viewer blurs the whole layer once at the stroke's start, with the
filter shader's blur passes, and the live stroke stage samples that
texture for its preview; the server blurs just the two rectangles,
each widened by the kernel's reach, and `composite-heal-stroke!`
(`source/heal.lisp`) is the stage's twin. Without float render targets
the stroke shows when the pointer lifts.

The History Brush (Y) paints the document as it was opened back onto
the current layer, through the brush: its source is the composite of
every layer at the opened state, or at the state the panel's Set
Source button chose since. Keeping the source costs nothing until it
is painted from, since the specs the document was made of still hold
the opened pixels, and the composite is rendered once when first
wanted (`source/history.lisp`). The viewer uploads that composite as a
texture the first time the tool is used and the live stroke stage
samples it; the server composites the same bytes, through the stamp's
compositing. In Grayscale the brush paints gray. A crop or resize
starts the history at the cropped state.

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

Documents belong to sessions. The page keeps a session id in the
browser's own storage and sends it on every call, and the server
keeps a table of documents per session, so two browsers opening
"demo" get two documents and one's edits never show in the other. A
call without a session id reaches the shared pool, which Lisp callers
and the earlier API use as before. A session idle for four hours is
forgotten with its documents. The Window menu lists the session's open
documents, the current one checked.

Image ▸ Image Rotation turns or flips the whole document: 180°, 90°
Clockwise, 90° Counter Clockwise, Flip Canvas Horizontal and Vertical
move every layer's and mask's pixels whole, exactly, and Arbitrary
turns by any angle onto a canvas grown to hold the turned picture,
bilinear in premultiplied linear light like Image Size, the exposed
corners transparent; a multiple of 90 turns whole pixels. Each remakes
the document through the resize path, so the page reloads.

Image ▸ Mode ▸ Grayscale turns every raster layer gray, each pixel's
luminance in linear light with its alpha kept, and from then on turns
whatever is painted, filled, placed or pasted to gray on its way in,
on the server and in the swatches, so no color gets back into the
picture until RGB Color is picked again, which changes nothing but the
mode (`source/mode.lisp`). The pixels are RGBA8 in either mode. A mode
change is one undo step, a group entry holding every layer it turned
with the mode itself, and the mode is kept in the file and through a
crop or resize. The other modes on the menu are shown and disabled.

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

### The Lisp API and the prompt box

Everything the editor does from its menus and tools is a function in
the package `stratum-api`, each of literal arguments on a current
document: `(stratum-api:use-document "demo")`, `(stratum-api:levels
0 :gamma 1.2)`, `(stratum-api:add-layer :curves :name "Warmer")`.
[docs/api.md](docs/api.md) is the reference, also served at
`/stratum/api/reference`.

The panel's prompt box takes an edit in words.  `POST
/stratum/api/prompt` sends the reference, the document described in
words with a small picture of it, and the request to a language model
(Gendl's `sexp-agent`, gwl/source/sexp-agent.lisp); the model answers
with forms, which run through a sandbox that admits only the API's
operations with literal arguments; the page then redraws.  The model
can look before it acts: `(look x y w h)` among its forms brings it a
picture of that part of the document at full resolution, captioned
with how picture pixels map to document pixels and with the selection
drawn on it, and it answers again -- to outline a thing closely, then
to check the outline, then the result (`stratum::*prompt-looks*`
rounds, five by default; this needs a Gendl whose sexp-agent carries
`:looks`, and without one the model works from the first picture
alone).  A prompt is one step of undo, whatever its forms did to the
stack, the settings and the selection, and the undo list before it
stays good; only a prompt that remade the document (a crop, a resize)
has none, as those have none from the menus.  The work runs on its
own thread: the door answers a job id and the page asks after it,
showing how far it has got.  Where the model is reached is `(stratum:configure-agent!
:messages-url ...)`: a key-holding gate in production, or the API with
`:api-key-file` on a trusted machine.  When the model cannot be
reached the box shows what its server said, changes nothing, and
keeps the prompt for another try.

The browser decodes an opened image and sends its pixels as rows of
differences (each byte less the same channel of the pixel to its
left), deflated by the browser's own compressor: every pixel exact, at
about a fifth of the raw bytes for a photograph.  The limit is on the
decoded image, not the file: 100 MB of RGBA, about 25 megapixels
(`stratum::*max-upload-bytes*`).  The page says so before it sends
anything, and the server refuses in words when its heap has no room.
On a secure page the reload that follows takes the pixels it has just
sent from the browser's cache storage instead of fetching them back.

Every raster layer's pixels and every mask carry a content stamp in
the document's JSON (`pixelStamp`, `maskStamp`): equal stamps are
equal bytes.  After any change the page fetches only content new to
it, and moves what it already holds between texture layers on the
GPU, so adding an adjustment layer or reordering the stack of a
photograph sends no pixels at all.

What is fetched comes down packed.  A page that can inflate asks the
pixel doors (`raster`, `mask`, `history`, `selection`) with `?enc=d`
and is answered one byte, the step, then a zlib stream of the bytes,
each less the byte that many before it (step 0: as they are): exact,
like the way up.  A new, mostly empty layer or a mask comes to a
hundredth of its bytes or less, the 1920 x 1080 demo sample to 2 MB
from 50, and a photograph to about a third (the pure-Lisp compressor
writes fixed codes only).

The menus, tools and panel show as soon as the document's structure is
known, and wait, dimmed, for its pixels.  After File > Open the status
line says where the time went: decode, pack, send (with the server's
own count of how long the body took to arrive and the document to
make), and the load of the page that followed.  The page's two scripts
are addressed with a version, so a cache between the server and the
browser cannot pair an old script with a new server.

### Generating pixels

`(stratum:configure-generator! :url "http://generator:7860" :steps 28
:cfg-scale 7 :strength 0.9 :size 1024)` points Stratum at an image
generator: stable-diffusion.cpp's `sd-server`, through its job API
(`/sdcpp/v1/img_gen`).  There is none by default.
`(generate-fill prompt)` sends a crop around the selection, scaled to
`:size`, with the selection as its mask; the answer lands as a new
raster layer whose alpha is the selection's coverage, faded in from
its edge so no seam shows, and, by default, only where the generator
changed the picture (`:keep :changed`; `:keep :all` keeps every pixel):
a generator repaints the whole of its mask, and its copy of the
surroundings is never quite the picture's.  Nothing outside the
selection can change, and the layer takes masks, blends and undo like
any other.  `(generate-layer prompt)` makes a picture of
the document's shape.  Both run outside the server's lock.

### Keeping a record of prompts

`(stratum:configure-prompt-log! :directory "/path/" :turns 300)` keeps a
directory per prompt under that path, closed to everyone but its owner:
`turn.sexp` (who asked, by the name on their invite, the prompt, the
document described, the forms with their results, the reply, the
model and any failure) and small pictures, at most 640 pixels on a
side, of what the model was shown (`before.png`, `look-1.png` ...) and
how it ended (`after.png`).  The oldest go past `:turns`.  There is no
log by default: the pictures are the visitors', so keeping them is the
operator's decision, and the visitors should be told.  A log that
cannot be written costs nobody their prompt.

### Private beta: invite links

With an invites file set, `(stratum:configure-gate! :invites-file
"/path/invites.sexp" :public-base "https://example.com")` (no file by
default), every door under `/stratum/` admits only a request carrying a
live invite: the cookie an invite link sets, or `?invite=TOKEN` (which
sets the cookie as well, so the page's own calls follow). An invite
link is `<base>/stratum/invite/TOKEN`: `(stratum:mint-invite! "Name")`
appends a token to the file and returns the link,
`(stratum:revoke-invite! token)` removes it, and
`(stratum:invites-report)` lists every invite with its uses. A line
removed from the file by hand revokes as well; the file is re-read
whenever it changes. Anything else gets a short refusal page (403).
The gate is Gendl's `invite-gate` (gwl/source/invite-gate.lisp), one
per application; `(stratum:gate)` hands it to that package's own
functions, so several applications on one server are run the same way:
`(invite-gate:mint-invite! (stratum:gate) "Name")`.

The menu bar is the one a classic image editor has, in its usual order
with its usual items and keys: File, Edit, Image, Layer, Select,
Filter, View, Window and Help. What is not built yet shows disabled,
as unavailable commands do. Built so far: File ▸ New (Ctrl+N), Open
(Ctrl+O), Open Sample, Place, Place Gendl Drawing, Save (Ctrl+S), Save As PNG
(Ctrl+Shift+S), Save As OpenRaster, Export As PNG and Close; Edit ▸
Undo (Ctrl+Z), Redo (Ctrl+Shift+Z or Ctrl+Y), Cut (Ctrl+X), Copy
(Ctrl+C), Paste (Ctrl+V), Fill (Shift+F5), Clear (Delete) and Free
Transform (Ctrl+T); Image ▸ Adjustments ▸ Levels
(Ctrl+L), Curves (Ctrl+M) and Hue/Saturation (Ctrl+U), each a dialog
with a live preview that bakes the adjustment into the active raster
layer's pixels on OK; Image ▸ Mode ▸ Grayscale and RGB Color; Image ▸
Image Size (Ctrl+Alt+I), Canvas Size (Ctrl+Alt+C), Image Rotation
(180°, 90° either way, Arbitrary, the two flips) and Crop; Select ▸ All (Ctrl+A), Deselect (Ctrl+D),
Inverse (Ctrl+Shift+I) and Feather (Shift+F6); Filter ▸ Blur ▸
Gaussian Blur, Sharpen ▸
Unsharp Mask and Noise ▸ Add Noise; Layer ▸ New ▸ Layer, Duplicate Layer,
Delete Layer, New Adjustment Layer (Levels, Curves, Hue/Saturation),
Layer Mask (Reveal All, Hide All, Delete), Arrange (Bring Forward,
Send Backward), Merge Down (Ctrl+E) and Flatten Image; View ▸ Zoom In,
Zoom Out,
Fit on Screen, Actual Pixels; Window ▸ Layers, Brush Settings and the
session's open documents.

The tool palette down the left keeps the order other editors use, with
the single-letter keys: Move (V), Marquee (M), Lasso (L), Magic Wand
(W), Crop (C), Eyedropper (I), Healing Brush (J), Brush (B), Clone
Stamp (S), History Brush (Y), Eraser (E), Gradient (G), Blur (R), Dodge
(O), Pen (P), Type (T), Path Selection (A), Shape (U), Hand (H) and
Zoom (Z). Built: Move, Marquee, Lasso, Magic Wand, Crop, Eyedropper,
Healing Brush, Brush, Clone Stamp, History Brush, Eraser, Gradient,
Hand and Zoom; the rest are drawn dimmed and refuse to be picked. The Crop tool's drag leaves a box with everything outside
it darkened; Enter or a double-click crops, Escape drops it. The foreground and
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

File ▸ New makes a document of a given size with one Background layer,
white, transparent, or the background color.

Files go in through the browser, which decodes every image format it
knows, and exactly: an ImageBitmap with no premultiplication and no
color conversion, read back through a texture, so the bytes the server
gets are the file's. Open makes a new document of them, one Background
layer; Place adds them as a layer of the current document, centered
and cropped or padded to its size. Save As PNG and Export write the
flattened picture as a PNG on the server (`zpng`).

The server reads PNG files itself as well (`read-png` in
`source/png.lisp`, about two hundred lines over `chipz`): every color
type at every bit depth, palettes with or without transparency, a
transparent key color, the five filters and Adam7 interlace, with
every chunk's CRC checked. Samples come out as the file holds them, 16
bits rounded to 8 and smaller depths scaled up, with no gamma or
color conversion, so a file reads the same on both halves. The `open`
and `place` endpoints take a PNG file whole, and `open-png-file` opens
one from Lisp.

Save writes the document itself as a `.stratum` file, and Open reads
one back (`source/file.lisp`). The file is a line `STRATUM1`, a line
with the header's length, the header, one s-expression holding the
materialized layer specs with the pixel and mask bytes replaced by
blob references, and then the blobs, each a zlib stream (`salza2` to
write, `chipz` to read). The header is read with `*read-eval*` off.
Nothing else goes in: no undo, no selection, no clipboard. A saved
document loads back rendering byte for byte the same, edits, strokes
and masks included.

A Gendl model is a layer source too (`source/drawing.lisp`): Place
Gendl Drawing draws a registered model by the drawing system's svg
lens in process, any of the ten standard views with hidden lines
drawn, removed or dashed, and rasterizes the edges anti-aliased
(Xiaolin Wu's lines, cubic curves flattened) into a layer of the ink
color with the coverage as its alpha, at the document's size. The
layer's source is `(:drawing :source NAME :projection ...)`, NAME a
model registered with `register-drawing-source`, so it draws afresh
at any size; `open-drawing` makes a document of a model from Lisp,
`place-drawing!` adds one to a document. No Ghostscript and no
subprocess: the sample assembly draws in about a tenth of a second.
On a Gendl that has its own in-process rasterizer (`gendl-raster` in
gwl-graphics, from November 2026) the layer is drawn by that instead,
which also fills the arrowheads and draws the text of dimensions and
labels in the Hershey simplex stroke font; an older Gendl draws the
lines alone.

For interchange with other editors there is OpenRaster, the layered
file of Krita, GIMP, MyPaint and Scribus (`.ora`, openraster.org):
Open reads one and Save As OpenRaster writes one. The file is a zip
archive, read and written by `source/zip.lisp` (stored and deflated
entries, nothing more), holding `stack.xml`, one PNG per layer,
`mergedimage.png` and a thumbnail (`source/ora.lisp`: an `ora-image`
of `ora-layer`s, top first; `read-ora`, `ora-octets`, a small XML
reader of its own). The mapping onto a document is
`source/ora-document.lisp`. A raster layer goes out as its pixels
with the mask baked into the alpha, Normal, Multiply, Screen, Add and
Color as `svg:src-over`, `svg:multiply`, `svg:screen`, `svg:plus` and
`svg:color`. An
adjustment layer has no OpenRaster shape, so it goes out as a hidden,
empty layer carrying its spec in Stratum's own attribute,
`stratum:spec`, which Stratum reads back whole and other editors show
as an empty hidden layer; the merged image is the full render either
way. Coming in, a layer at an offset is placed on the canvas, a
nested stack (a group) is flattened with its opacity and visibility
applied, and a blend mode Stratum has not got becomes Normal and is
reported. A document saved as OpenRaster loads back the same stack,
rendering within two levels (the baked mask rounds once).

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
| `/stratum/invite/TOKEN` | a live invite sets the invite cookie and redirects to `/stratum/`; anything else is a 403 (only when an invites file is set) |
| `/stratum/api/doc?doc=N` | JSON: size, plan version, shader source, layers |
| `/stratum/api/raster?doc=N&i=K` | raw RGBA8 of raster layer K; with `&x=&y=&w=&h=`, that rectangle |
| `/stratum/api/mask?doc=N&i=K` | raw R8 of mask K; with `&x=&y=&w=&h=`, that rectangle |
| `/stratum/api/thumb?doc=N&i=K&w=&h=` | a w × h nearest-neighbor thumbnail of raster layer K (RGBA8), or with `&mask=1` of mask K (R8) |
| `/stratum/api/cpu?doc=N` | raw RGBA8 of the CPU render |
| `/stratum/api/export.png?doc=N` | the CPU render as PNG |
| `/stratum/api/luts?doc=N` | JSON: the server's curve and levels tables |
| `POST /stratum/api/set?doc=N` | `{layer, slot, value}` for `opacity`, `visible`, `blend`; `{layer, slot: "setting", name, value}` for a levels or hue/saturation setting; `{layer, slot: "curve", channel, points}` |
| `POST /stratum/api/stroke?doc=N` | `{layer, target: "layer" or "mask", samples: [[x, y, pressure], ...], brush: {radius, hardness, spacing, flow, opacity, color, mode}, clone: [dx, dy] or null, heal: true or false, history: true or false}`; answers the dirty rectangle with the layer's texture or mask index and new version; with `&dry=1` paints nothing and answers the coverage buffer |
| `POST /stratum/api/undo?doc=N` | undoes the last change; answers the rectangle put back, or for a mode change `{mode, group: [rectangles]}` |
| `/stratum/api/history?doc=N` | raw RGBA8 of the History Brush's source, the document's history composite |
| `POST /stratum/api/history-set?doc=N` | sets the history source to the document's current state; answers `{historyVersion}` |
| `/stratum/api/documents?s=S` | the session's open documents: `{documents: [names], session}`; every path above takes `&s=S` as well, the session id, and without it reaches the shared pool |
| `POST /stratum/api/mode?doc=N` | `{mode: "rgb" or "grayscale"}`; answers `{mode, group: [the layers turned, as rectangles]}` |
| `POST /stratum/api/redo?doc=N` | redoes the last undo; answers the rectangle put back |
| `POST /stratum/api/new` | `{name, width, height, background: "white", "transparent" or [r, g, b]}`; a new document of one Background layer; answers `{doc, width, height}` |
| `POST /stratum/api/open?name=&w=&h=` | body: a PNG file, or raw RGBA8 with `w` and `h`; a new document of one Background layer; answers `{doc, width, height}` |
| `POST /stratum/api/place?doc=N&name=&w=&h=&above=` | body: a PNG file, or raw RGBA8 with `w` and `h`; a new layer of the document, centered and fitted to its size; answers as `layers` does |
| `/stratum/api/drawing-sources` | the Gendl models registered to be drawn, the standard views and the hidden-line treatments: `{sources, projections, hiddenLines}` |
| `POST /stratum/api/drawing?doc=N` | `{source, projection, hiddenLines, color: [r, g, b], weight, margin, above}`: a new layer drawn from the registered Gendl model; answers as `layers` does |
| `POST /stratum/api/close?doc=N` | forgets the document |
| `/stratum/api/save?doc=N` | the document as a `.stratum` file |
| `POST /stratum/api/load?name=` | body: a `.stratum` file or an OpenRaster `.ora` file; a new document; answers `{doc, width, height, unsupported: [[layer, composite-op] ...]}` |
| `/stratum/api/save.ora?doc=N` | the document as an OpenRaster `.ora` file |
| `POST /stratum/api/select?doc=N` | `{op: "rect", x, y, w, h}`, `{op: "polygon", points}`, `{op: "wand", x, y, tolerance, contiguous, layer}`, `{op: "feather", radius}`, `{op: "all"}`, `{op: "none"}` or `{op: "inverse"}`, with `mode: "new"`, `"add"` or `"subtract"` on the first three; answers `{selection: {x, y, w, h} or null}` |
| `/stratum/api/selection?doc=N` | raw R8 of the selection, empty when none |
| `POST /stratum/api/fill?doc=N` | `{layer, color, opacity, mode: "paint" or "erase"}` inside the selection; answers the rectangle as `stroke` does |
| `POST /stratum/api/move?doc=N` | `{layer, dx, dy}`: the selected pixels moved; answers the whole layer as a rectangle, with the selection's new rectangle |
| `POST /stratum/api/transform?doc=N` | `{layer, matrix: [a, b, c, d, e, f]}` (x′ = ax + by + c, y′ = dx + ey + f): the selected pixels resampled through it; answers the whole layer as a rectangle, with the selection's new rectangle |
| `POST /stratum/api/clipboard?doc=N` | `{op: "copy" or "cut", layer}` or `{op: "paste", above}`; Copy answers the clipboard's size, Cut the rectangle cleared, Paste the new layer as `layers` does |
| `POST /stratum/api/resize?doc=N` | `{op: "crop"}` (to the selection), `{op: "crop-rect", x, y, w, h}`, `{op: "image-size", width, height}`, `{op: "canvas-size", width, height, anchor: [ax, ay]}`, `{op: "rotate", turn: "cw", "ccw", "180", "flip-h" or "flip-v"}` or `{op: "rotate-arbitrary", degrees}` (clockwise); the document is remade; answers `{width, height}` |
| `POST /stratum/api/filter?doc=N` | `{layer, kind: "gaussian-blur", "unsharp-mask" or "add-noise", settings: {radius, amount, threshold, monochrome, seed}}` baked into the layer inside the selection; answers the whole layer as a rectangle |
| `POST /stratum/api/gradient?doc=N` | `{layer, x0, y0, x1, y1, start: [r, g, b, a], end: [r, g, b, a], shape: "linear", "reflected", "radial", "diamond" or "angle", opacity}` laid over the layer inside the selection; answers the whole layer as a rectangle |
| `POST /stratum/api/adjust?doc=N` | `{layer, kind: "levels", "curves" or "hue-saturation", settings: {name: value}, curves: {channel: points}}` baked into the layer inside the selection; answers the whole layer as a rectangle |
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
- Image Rotation: the quarter turns, the half turn and the flips move
  the pixels whole, a turn each way and four the same way give the
  picture back, a mask turns on one channel, an arbitrary 90 degrees
  through the resampler equals the whole-pixel turn within a level, 45
  degrees grows the canvas and keeps the center and clears the
  corners, the half turn is its own inverse through the resampler, a
  turned document has its sides swapped with every layer and mask
  turned and its mode kept, and an arbitrary multiple of 90 turns
  whole pixels;
- the History Brush: the history composite is the document as opened,
  rendered once and kept; a history stroke paints it onto the layer
  inside its rectangle and rebuilds nothing; undo gives the layer
  back and the source stands; Set Source takes the current state,
  copied, and bumps the version; in Grayscale it paints gray; a
  cropped document starts its history at the cropped state;
- sessions: each session has its own documents and no session is the
  shared pool; an idle session is swept and comes back empty;
- Image ▸ Mode: a color turns to its luminance gray with its alpha
  kept; Grayscale turns every raster layer gray in one undo entry and
  rebuilds nothing; in Grayscale a red stroke, a red fill and a
  red-to-blue gradient paint gray; undo of the mode change brings
  every layer and the mode back and redo turns it gray again; RGB
  Color changes the mode alone and undoes; a paste into a Grayscale
  document is gray, and the file and a crop keep the mode;
- the PNG reader: a PNG written by zpng reads back byte for byte; PNGs
  built by hand read back: 8-bit RGBA through every filter type, Adam7
  interlace through all seven passes and with passes skipped, 16-bit
  RGB rounded to bytes, 1-bit and 4-bit gray scaled up, a 2-bit palette
  with partial transparency, gray with alpha and a gray key color; a
  bad CRC and a file that is not a PNG are refused; a PNG file opens as
  a document under its own name;
- the Clone Stamp: the source rectangle is copied and clear beyond the
  picture, a clone stroke paints the layer's own pixels from the
  offset inside its rectangle only and rebuilds nothing, a stroke over
  its own source reads the pixels as they were, a source beyond the
  layer paints nothing, and undo gives the layer back;
- the Healing Brush: the rectangle blur equals the whole layer's blur
  bit for bit and is zero beyond the layer, healing from a dark flat
  onto a light flat keeps the light tone where a clone pastes the dark,
  a bright pixel's texture carries onto its destination, a healing
  stroke on the Sky ramp keeps the destination's tone inside its
  rectangle only and rebuilds nothing, and undo gives the layer back;
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
  cropped, an opened image renders back as it came, and a new document
  is white, transparent or a color, one Background;
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
  of its size when opened; the same PNG file sent whole is read by the
  server to the bytes the browser decoded;
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
  Image Size, a quarter turn, an arbitrary 30 degrees and Canvas Size
  each remake it at the new size; the Image Size and Rotate Canvas
  dialogs open and cancel;
- New and Crop: the New dialog opens and cancels, New makes a document
  of the asked size with a transparent Background, the Crop tool's
  drag leaves a box that Escape drops, and a crop to a rectangle
  remakes the document at that size;
- the Gradient tool on the lowest raster layer: a drag previews the
  ramp and bakes it on release within two levels of the preview, the
  layer's bytes the two colors at the ends and their encoded mean in
  the middle, parity, undo exact; a radial ramp under a selection
  changes nothing outside it, undo exact;
- the Clone Stamp on the lowest raster layer: Alt-click sets the
  source, a stroke carries the offset and is committed within two
  levels of its preview, its first dab takes the pixel at its source,
  parity, Aligned keeps the offset for a second stroke, and undo twice
  gives the layer back; then the Healing Brush from the same source:
  the stroke carries the heal flag and is committed within two levels
  of its preview, on the parity document's Sky ramp the healed pixel
  keeps the destination's tone rather than the source's, parity, and
  undo gives the layer back;
- the History Brush on the lowest raster layer: after a red brush
  stroke, a history stroke over its middle puts the opened composite
  there, committed within two levels of its preview, the layer's
  pixel under it equal to the history composite's, parity, and undo
  twice gives the layer back;
- sessions: the page has a session id, a document made here is listed
  for this session and not for another nor for the shared pool, and
  Close drops it;
- Image ▸ Mode ▸ Grayscale on the document: every pixel of a layer and
  the swatches turn gray, the Mode menu checks Grayscale, parity holds,
  and one undo brings the color and the mode back exactly;
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

Smaller items still to do: premultiplying before mipmap generation to
remove edge fringes when zoomed out.  The CPU render runs on
`*render-threads*` threads (8 by default): 2 s for the 4K twenty-layer
document, against 10 s on one thread.

## License

GNU Affero General Public License, version 3 or later. See `LICENSE`.
