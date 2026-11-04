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

## Layers

Layer kinds are `:raster` (pixels), and the adjustment layers
`:curves`, `:levels` and `:hue-saturation`, which change what is
under them without touching any pixels.  Prefer an adjustment layer
for a tonal or color change that may be revisited; bake with the
operations under Adjustments when the pixels themselves should
change.

- `(add-layer kind &key above name)` — a new layer above layer
  `above`, or on top.  Returns its `layer-info`.
- `(delete-layer layer)`, `(duplicate-layer layer)`,
  `(move-layer layer &optional (delta 1))` — delta places up the stack,
  negative for down.
- `(merge-down layer)` — a raster layer merged into the raster layer
  under it.  `(flatten)` — everything into one.
- `(set-layer layer &key opacity visible blend ...)` — the layer's
  state: `:opacity` 0-1; `:visible` `t` or `nil`; `:blend` `:normal`,
  `:multiply`, `:screen` or `:add` on a raster layer; a levels layer's
  `:in-black :in-white :gamma :out-black :out-white`; a hue/saturation
  layer's `:hue` (degrees) `:saturation` (-1..1) `:lightness` (-1..1);
  a curves layer's `:master :red :green :blue`, each a list of points
  `((x y) ...)` on 0-1 from `(0 0)` to `(1 1)`.
- `(set-mask layer value)` — `:reveal` (a white mask), `:hide` (a black
  one) or `:delete`.

```lisp
(add-layer :hue-saturation :name "Less yellow")
(set-layer "Less yellow" :hue -8 :saturation -0.15)
(add-layer :levels)
(set-layer "Levels 1" :in-black 0.05 :gamma 1.1)
(set-layer 0 :opacity 0.8 :blend :multiply)
```

## The selection

Every pixel operation acts inside the selection; with no selection
it acts on the whole layer.  `mode` is `:new` (the default), `:add` or
`:subtract`.

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
- `(undo)`, `(redo)`.
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
forms have run, then `{status: "done", reply, forms, results, ok,
width, height}`, or `{status: "failed", error}`.  `GET
/stratum/api/reference` serves this document.
