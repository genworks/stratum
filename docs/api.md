# The Stratum Lisp API

Everything the editor does from its menus and tools can be done by
evaluating s-expressions: at a REPL in the image that serves Stratum,
from a program, or by the prompt box, where a language model writes
the forms for an edit described in words.  The operations live in the
package `stratum-api`; each is a function of literal arguments on the
current document, and each returns a plist or a list that prints
plainly.

```lisp
(stratum-api:use-document "demo")
(stratum-api:levels 0 :gamma 1.2)
(stratum-api:add-layer :curves :name "Warmer")
(stratum-api:set-layer "Warmer" :red '((0 0) (0.5 0.56) (1 1)) :blue '((0 0) (0.5 0.45) (1 1)))
(stratum-api:describe-document)
```

The current document is `stratum-api:*document*`: `use-document`
sets it, and the prompt door binds it to the page's document.
Layers are named by index, 0 at the bottom of the stack, or by name.
Pixel coordinates have their origin at the top left; colors are `(r
g b)` on 0-255; the scales that run 0-1 say so.  A form's arguments
are literals only: numbers, strings, keywords, `t`, `nil` and lists of
those (a list may be quoted or not).

The prompt box runs the model's forms through a sandbox that admits
the operations below except those under Files and the choice of
document, which belong to the page.

## Reading the document

- `(describe-document)` — the document in words: size, mode, each
  layer with its state, the selection, the undo depth.
- `(document-info)` — the same as a plist: `:name :width :height :mode
  :layers :selection :undo-depth :redo-depth`.
- `(layers)` — every layer's `layer-info`, bottom first.
- `(layer-info layer)` — one layer: `:index :name :kind :opacity
  :visible :mask`, `:blend` for a raster layer, each setting of an
  adjustment layer, and the four channels' points of a curves layer.
- `(selection)` — the selection's bounds `(x y w h)`, or `nil` when
  everything is selected.
- `(documents)` — the names of the open documents.
- `(look)`, `(look x y w h)` — a picture of the document, or of that
  rectangle of it, flattened as the page shows it.  A rectangle up to
  1280 pixels on its longer side comes at full resolution, one picture
  pixel a document pixel; a larger one is reduced by a whole factor,
  and one under 400 pixels enlarged by one.  The picture's caption
  gives the mapping, for example "picture pixel (px, py) is document
  pixel (3300 + px, 1400 + py)".  When something is selected the
  picture is dimmed outside the selection and the selection's edge is
  drawn in magenta; `:selection nil` after the other arguments leaves
  that out.

## Seeing what you are doing

The picture that comes with a request shows the whole document, most
often much reduced: its caption says by how much.  It is enough to
find things and to judge tone and color.  It is not enough to outline
anything: on a photograph a point read off it may be twenty document
pixels from where it should be.  So before any operation that needs
exact places, end the block with `look` at a rectangle around the
thing, with a margin, small enough to come at full resolution; the
forms up to it run, the picture arrives, and you answer again with
the forms that remain.  Only a few looks may be asked in one request,
so spend them where they matter: once to see a thing closely, once to
check its selection, and the last on the result.

To change one thing in a photograph (recolor the horns, brighten the
eyes, blur a sign):

1. `look` at it closely.
2. Select it.  `select-polygon` with a point every 10 to 25 document
   pixels along its edge, on the edge and not inside or outside it:
   thirty points and more for a curved shape, each a document pixel
   worked out from the caption's mapping.  Or, when its color differs
   from what surrounds it, a loose polygon around it and then
   `select-wand` on a point inside it with `:mode :intersect`.
   Separate things are separate selections joined with `:mode :add`.
   Where the thing stands against something plain (sky, snow, a
   wall), outline it generously on that side and take the plain part
   away: `select-wand` on a point of the background with `:mode
   :subtract` (and `:contiguous nil` when the background shows
   through in several places) leaves the selection on the thing's
   own edge, closer than points can.
3. `look` again, at the same rectangle: the magenta edge should lie on
   the thing's edge.  Correct it with `:add`, `:subtract` or
   `:intersect` where it does not.  `:intersect` and `:subtract` act
   on the whole selection, so with several things selected an
   `:intersect` meant for one drops the others: correct each thing
   before adding the next.
4. `(feather 1)` to `(feather 3)` for a thing with a hard edge, so the
   change does not end in a jagged line; more only for soft things
   (fur, smoke, a glow).
5. Make the change.  To lighten or darken it, or shift its color a
   little, `levels`, `curves` or `hue-saturation` on the raster layer
   it is in; they stay inside the selection.  To recolor, fill the selection on a new raster
   layer and set that layer's `:blend` to `:color`: the layer gives
   hue and saturation, what is under it gives light and dark, so
   shading and texture stay.  `:multiply` darkens as it tints,
   `:normal` paints over.
6. `(select-none)`, then `look` at the thing once more to see the
   result, and put right what is wrong.  Keep your last look for this:
   the caption says how many are left.

## Making new pixels

Everything else here changes what is in the picture.  When a request
needs something that is not there and cannot be painted, a realistic
object, a filled-in background, a thing removed with what was behind
it, these two ask an image generator.  The message with each request
says whether this server has one.  Each takes a minute or so, and the
pictures are small, a megapixel or so scaled to fit.  Use them only for
that; a recolor, a tone change or a simple shape is better done with
the operations above, which are exact and immediate.

- `(generate-fill prompt &key name seed keep)` — the selection
  repainted, to match what surrounds it, as a new raster layer visible
  only inside the selection, so nothing else can change.  Select the
  area first and generously, well beyond the thing you want: the result
  fades out toward the selection's edge, and anything reaching the edge
  is cut off there.  By default the layer keeps only what the generator
  changed (`:keep :changed`): where it painted back much the same as
  before, the layer is clear, so a new thing comes without a repainted
  copy of its surroundings and the picture shows through.  `:keep :all`
  keeps every pixel of the selection: for a fill that must replace
  everything it covers, such as a new background or a changed texture,
  where the new may resemble the old.  To remove something, select it
  and describe what should be there instead.
- `(generate-layer prompt &key name seed)` — a whole new picture of the
  document's shape as a new raster layer on top.
- `(generate-picture prompt &key name seed reference strength)` — the
  whole picture made over as a new raster layer on top: a change of
  style, season or light across all of it.  With `:reference t`, the
  default, the generator is shown the picture and `prompt` is an
  instruction, unlike the other two: say what to change and to keep the
  rest, as in "turn this into an oil painting, keeping the same
  composition, pose and framing".  Without those last words it redraws
  the subject in a pose of its own.  With `:reference nil` it starts from
  the picture with `:strength` of noise, 0 to 1, default 0.35: 0.3 keeps
  the pose and little of a new style, 0.6 redraws the subject.  The
  result is a megapixel or so, scaled up: fine detail of a large
  photograph is lost.

`prompt` is a description of the result in plain visual words, not an
instruction: "weathered stone wall in soft afternoon light", not "make
this a wall".  `name` names the layer; `seed` repeats a result, and -1,
the default, varies it.  `look` at the result; a poor one is taken off
with `(delete-layer name)` before trying again with other words.

## Layers

Layer kinds are `:raster` (pixels), and the adjustment layers
`:curves`, `:levels` and `:hue-saturation`, which change what is
under them without touching any pixels.  Prefer an adjustment layer
for a change to the whole picture that may be revisited; bake with the
operations under Adjustments when the pixels themselves should change.

An adjustment layer changes everything under it, and `set-layer`'s
`:opacity` the whole of its layer, whatever is selected: the selection
binds neither.  To change only part of the picture, select the part
and use an operation under Adjustments, or add the adjustment layer
and give it `(set-mask layer :selection)`.  Lowering the opacity of the
only layer fades the whole picture and is never the way to lighten
something.

- `(add-layer kind &key above name)` — a new layer above layer
  `above`, or on top.  Returns its `layer-info`.
- `(delete-layer layer)`, `(duplicate-layer layer)`,
  `(move-layer layer &optional (delta 1))` — delta places up the stack,
  negative for down.
- `(merge-down layer)` — a raster layer merged into the raster layer
  under it.  `(flatten)` — everything into one.
- `(set-layer layer &key opacity visible blend ...)` — the layer's
  state: `:opacity` 0-1; `:visible` `t` or `nil`; `:blend` `:normal`,
  `:multiply`, `:screen`, `:add` or `:color` (the layer's hue and
  saturation at the lightness of what is under it) on a raster layer;
  a levels layer's
  `:in-black :in-white :gamma :out-black :out-white`; a hue/saturation
  layer's `:hue` (degrees) `:saturation` (-1..1) `:lightness` (-1..1);
  a curves layer's `:master :red :green :blue`, each a list of points
  `((x y) ...)` on 0-1 from `(0 0)` to `(1 1)`.
- `(set-mask layer value)` — `:reveal` (a white mask), `:hide` (a black
  one), `:selection` (the selection, so what the layer does stays
  inside it) or `:delete`.

```lisp
(add-layer :hue-saturation :name "Less yellow")
(set-layer "Less yellow" :hue -8 :saturation -0.15)
(add-layer :levels)
(set-layer "Levels 1" :in-black 0.05 :gamma 1.1)
(set-layer 0 :opacity 0.8 :blend :multiply)
```

## The selection

Every operation under Pixels and Adjustments acts inside the
selection; with no selection it acts on the whole layer.  A selection
operation that would leave nothing selected fails and the selection
stays, since nothing selected means everything.  `mode` is `:new` (the default), `:add`,
`:subtract` or `:intersect` (what the old selection and this one both
cover).

- `(select-rect x y w h &key mode)`
- `(select-polygon points &key mode)` — points `((x y) ...)`.
- `(select-wand x y &key (tolerance 32) (contiguous t) mode layer)` —
  the Magic Wand at a pixel, on a layer or the composite.
- `(feather radius)` — the edge softened over `radius` pixels.
- `(select-all)`, `(select-none)`, `(invert-selection)`.

## Pixels

These change a raster layer's pixels and go on the undo list.

- `(fill-selection layer color &key (opacity 1))`,
  `(clear-selection layer)` — made transparent.
- `(paint layer points &key radius hardness spacing flow opacity color
  mode target)` — a brush stroke through `points` `((x y) ...)` or
  `((x y pressure) ...)`: `radius` in pixels, `hardness` 0-1, `spacing`
  as a fraction of the radius, `flow` and `opacity` 0-1, `color`
  `(r g b)`, `mode` `:paint` or `:erase`, `target` `:layer` or `:mask`.
- `(gradient layer x0 y0 x1 y1 &key shape start end opacity)` — from
  the first point (color `start`, `(r g b a)`) to the second (`end`);
  `shape` `:linear`, `:reflected`, `:radial`, `:diamond` or `:angle`.
- `(move-pixels layer dx dy)` — the selected pixels moved.
- `(transform layer matrix)` — through the affine `(a b c d e f)`,
  x' = ax + by + c, y' = dx + ey + f.
- `(copy layer)`, `(cut layer)`, `(paste &optional above)` — the
  clipboard; paste makes a new raster layer.

## Adjustments and filters

Baked into a raster layer's pixels, inside the selection, on the
undo list.

- `(levels layer &key (in-black 0) (in-white 1) (gamma 1) (out-black
  0) (out-white 1))` — on 0-1; gamma above 1 lifts the midtones.
- `(curves layer &key master red green blue)` — each a list of
  points on 0-1.
- `(hue-saturation layer &key (hue 0) (saturation 0) (lightness 0))`
  — hue in degrees, saturation -1 (gray) to 1 (doubled), lightness -1
  (black) to 1 (white).
- `(gaussian-blur layer radius)` — radius in pixels.
- `(unsharp-mask layer &key (radius 2) (amount 100) (threshold 0))` —
  amount in percent.
- `(add-noise layer &key (amount 10) monochrome (seed 1))` — amount in
  percent.

```lisp
;; warmer: red up and blue down through the midtones
(curves 0 :red '((0 0) (0.5 0.55) (1 1)) :blue '((0 0) (0.5 0.45) (1 1)))
;; a soft vignette: select the middle, invert, feather, darken
(select-rect 100 80 824 608)
(invert-selection)
(feather 120)
(levels 0 :in-white 1 :gamma 0.7)
(select-none)
```

```lisp
;; recolor one thing, first reply: look closely and stop
(look 3250 1400 650 800)
```

```lisp
;; second reply, after the picture: outline it and check
(select-polygon ((3391 1489) (3402 1512) (3410 1538) (3421 1566) (3436 1590)))
(look 3250 1400 650 800)
```

```lisp
;; third reply, the selection being right: recolor, keeping the shading
(feather 2)
(add-layer :raster :name "Red horn")
(fill-selection "Red horn" (200 30 30))
(set-layer "Red horn" :blend :color)
(select-none)
```

## The whole image

Each of these remakes the document at a new size; the page reloads.

- `(crop &optional x y w h)` — to the rectangle, or to the selection.
- `(image-size width height)` — resampled.
- `(canvas-size width height &key (anchor '(0.5 0.5)))` — the canvas
  changed around the picture; the anchor `(ax ay)` on 0-1 says where
  the old picture sits, `(0 0)` the top left.
- `(rotate turn)` — `:cw`, `:ccw`, `:180`, `:flip-h`, `:flip-v`, or
  degrees clockwise.
- `(color-mode mode)` — `:rgb` or `:grayscale`.
- `(undo)`, `(redo)` — a whole earlier prompt comes back as one step,
  layers, settings, selection and all, so "undo that" is one `(undo)`.
  Within a prompt, a form that changes the stack ends what `undo` can
  reach of the forms before it.  A prompt that remade the document
  (crop, a resize, a rotation) has no undo.
- `(set-history-source)` — the History Brush's source set to the
  document as it is now.

## Gendl drawings

- `(drawing-sources)` — the names of the Gendl models registered to
  be drawn.
- `(place-drawing source &key (projection :trimetric) (hidden-lines
  :draw) (color '(0 0 0)) (weight 1.5) (margin 0.05) above)` — the
  model drawn as a new raster layer; views `:top :bottom :left :right
  :front :rear :trimetric :isometric`; hidden lines `:draw`, `:remove`
  or `:dashed`.

## Documents and files

Lisp callers only; the prompt box works on the page's document.

- `(use-document name)` — make it current (a sample is made on first
  sight).
- `(new-document name width height &key (background :white))` —
  background `:white`, `:transparent` or `(r g b)`.
- `(close-document &optional name)`.
- `(open-image path &optional name)` — a PNG file as a new document.
- `(export-png path)` — the document flattened to a PNG file.
- `(save-document path)` — a `.stratum` file, layers and all.
- `(load-document path &optional name)` — a `.stratum` or OpenRaster
  `.ora` file as a new document.

## Running forms the prompt box's way

`(stratum:run-forms "(levels 0 :gamma 1.2) (undo)")` reads the string
in `stratum-api` with read-time evaluation off, admits only the
operations above with literal arguments, runs them in order on the
current document under the server's lock, and answers a list of
`(:form f :ok t :value v)` or `(:form f :ok nil :error text)`, one
per form, stopping at the first failure.  The prompt door, `POST
/stratum/api/prompt?doc=NAME&s=SESSION` with `{"prompt": "..."}`,
starts the model's work and answers `{job}`; `GET
/stratum/api/prompt?job=ID` answers `{status: "working"}` until the
forms have run (with `note`, how far the work is), then `{status:
"done", reply, forms, results, ok, width, height, declined}`, or `{status:
"failed", error}`.  `declined` is true when the model declined the
request; `reply` then says so, and forms that ran before it stand.  The model may look and answer again up to
`stratum::*prompt-looks*` times in one prompt.  `GET
/stratum/api/reference` serves this document.
