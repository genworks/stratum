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
   `select-wand` on a point inside it with `:mode :intersect`.  Or a
   polygon near its edge and then `refine-selection`, which finds the
   edge by color.  Separate things are separate selections joined with `:mode :add`.
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

## Putting a subject on a new background

To put a person or thing from the photograph in front of a new
background (the generator and its prompts are under Making new
pixels):

1. Decide the frame.  A subject that touches the edge of the photograph
   is cut off there, and moved inward that cut shows as a straight line
   through an arm or a shoulder.  Leave such a subject where it is, and
   move the others toward it if they must move.  If it must move too,
   crop at the end so that its cut lies on the new edge.
2. Cut it out.  Outline it with `select-polygon` as above, leaving out
   any pocket of the old background between an arm and the body; the
   points need only come near the edge, but keep them a few pixels
   OUTSIDE it, never inside: refining takes background off far better
   than it brings back subject, and hair or a dark coat that the
   polygon left out may be judged background, since its colors are
   found outside too.  Then `(refine-selection
   :radius 10)` moves the selection's edge onto the subject's own
   outline, judging by color within that many pixels either side of
   it, five passes over, so the edge can travel about five times the
   radius from where you drew it, out of a face the polygon cut into or
   in past a strip of wall it took in.  Radius 8 to 12 on a picture
   under a thousand pixels wide, about one for each hundred pixels of
   width on a larger one.  What lies away from the polygon's edge it
   never touches: a patch of background deep inside the polygon, and
   above all one against the picture's own edge, which is no edge of a
   selection, stays selected; take it away with `select-wand` and
   `:mode :subtract`, or a polygon.  `look` at the selection, all the
   way round each subject, both sides of every head included: where
   the subject and the background share a color the edge may follow the
   wrong one.  To take back a part of the subject that was left out,
   `select-polygon` over it with `:mode :add`, reaching a few pixels
   into the background, then `refine-selection` again: it trims the
   background and keeps the part.  To take off a patch of background,
   `select-wand` on it or a polygon, with `:mode :subtract`.  Then `(feather 1)` and `(cut-out layer :name
   "Subject")`, which puts the subject on a layer of its own without
   the old background's tint at its edge.  A selection made without
   `refine-selection`, on the outline by eye, needs
   `(contract-selection 2.5)` before the feather, or it brings a rim of
   the old background.  Subjects that stay together can share one
   selection and one layer.  To move it, `(select-none)` and then
   `(move-pixels layer dx dy)`.
3. Make the background a layer of its own, never a fill around the
   subject: `(generate-layer prompt :name "New background")`, then
   `move-layer` it down past the subject's layers (with one,
   `(move-layer "New background" -1)`), and `(set-layer 0 :visible nil)`
   to hide the original, whose layer is most often named "Background"
   already.  `generate-fill` fades out at the selection's
   edge, so a fill through `invert-selection` leaves the old picture
   showing around the subject in a band about six pixels wide.
4. Blur the background, with nothing selected so that the blur reaches
   all of it: `(select-none)`, then `(gaussian-blur "New background" 2)`,
   about 2 for each thousand pixels of the picture's width.  A sharp
   background behind a sharp subject reads as a sticker; this step does
   more than any other here.
5. Leave the subject's colors alone unless the new light is plainly
   colored; then move them a quarter of the way at most, with `levels`
   or `hue-saturation` on the subject's layer, since a strong match
   makes skin look dead.  Add no drop shadow to a subject in front of a
   distant view.  Run no generator over the subject, neither a fill nor
   `generate-picture`: over a person it paints someone else.
6. `look` closely along the whole of the subject's outline, a part at
   a time at full resolution, and at the corners of the frame.  Where
   the outline ran outside the subject's edge, a strip of the old
   background came with it: whole pixels, which `cut-out` cannot clean,
   and against the new background they show plainly.  Take such a
   strip off the subject's layer: `select-wand` on a pixel of it with
   `:layer` the subject's layer and a moderate tolerance, then
   `(expand-selection 1)`, `(feather 1)` and `(clear-selection layer)`
   on that layer, and `(select-none)`.  If the wand reaches into the
   subject, undo and use a lower tolerance.  `crop` only now, after
   everything else, and only when step 1 needs it or the person asked:
   a prompt that crops has no undo.

## Keeping a face, changing the rest

To change a person's build, clothes or pose and keep their face, make
the new body in ONE generation and put the person's own head over it.
Never build the body up from fills patch by patch: each fill fades out
at its edge, every patch over a patch leaves another line across the
body, and each repaint puts the neck somewhere new.

1. Cut out the head first, from the photograph as it is.  Select the
   head and the neck down to just above the collar, on the edge where
   they meet the background, and straight across the neck below.
   `(contract-selection 2)`, `(feather 2)`, then `(cut-out 0 :name
   "Head")`.
2. Make the new picture with the photograph as its reference:
   `(select-none)`, then `(generate-picture "make this person's build
   much more muscular, keeping the same face, room, pose, framing, shirt
   and light" :name "New body")`.  Say what stays: the clothes as they are ("the same
   navy cotton shirt"), the light as it is ("the same soft indoor
   light"), so the new body is not lit or dressed from another
   picture.  Ask for a neck to match: a much broader body on the
   person's own neck makes the head look small and set on top.
3. `(move-layer "New body" -1)` puts it under the head.  `look` at the
   neck closely: the cut must fall on the new neck.  If the new neck is
   a few pixels off, `(move-pixels "Head" dx dy)` with nothing
   selected; if it is far off or the wrong width, delete the layer and
   generate again with another seed, rather than patching it.
4. The new picture redraws the whole frame, the room too, from a
   megapixel or so.  To keep the room sharp, cut the new body out of
   it as well (step 2 of the recipe above, on "New body") and hide
   that layer: that works only where the new body covers the old one,
   so `look` along its edges for the old shoulders showing.
5. No generator over the face, ever: the head layer is the person.
   `look` at the face, the neck and the edges last.

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
  fades out over a few pixels at the selection's edge, and anything
  reaching the edge is cut off there.  The generator works at about a
  megapixel over the selection's whole bounding box, so give areas far
  apart a fill each: one fill across both is painted coarser.  When a
  fill looks wrong, check the selection as well as the words.  By default the layer keeps only what the generator
  changed (`:keep :changed`): where it painted back much the same as
  before, the layer is clear, so a new thing comes without a repainted
  copy of its surroundings and the picture shows through.  `:keep :all`
  keeps every pixel of the selection: for a fill that must replace
  everything it covers, such as a new background or a changed texture,
  where the new may resemble the old.  To remove something, select it
  and describe what should be there instead.  When a fill leaves a seam,
  a line or a change of texture where it ends, never fill over the
  seam: that adds a fade of its own and another line.  Delete the fill
  and do it again over the whole region at once, selected generously.
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
  the pose and little of a new style, 0.6 redraws the subject.  A model
  that cannot be shown a picture starts from it in either case, and the
  answer's `:note` says so.  The result is a megapixel or so, scaled up:
  fine detail of a large photograph is lost.

`prompt` is a description of the result in plain visual words, not an
instruction: "weathered stone wall in soft daylight", not "make this a
wall".  `name` names the layer; `seed` repeats a result, and -1, the
default, varies it.  `look` at the result; a poor one is taken off with
`(delete-layer name)` before trying again with other words.

**Writing a prompt.**  Say what the person's words mean, not what they
sound like.  A name, a title or a figure of speech is not a color or a
material: "the Golden Age of Athens" means Athens at the height of its
glory, not a city made of gold.  Use a color word only for a color the
person asked for, and add no mood, weather, time of day or style they
did not ask for, such as "sunset", "epic", "ornate" or "rich": the
generator takes every word at full strength.

Match the medium.  Behind or into a photograph, ask for a photograph:
begin with "photograph of" and say "natural color".  Say "painting" or
"illustration" only when the person asked for a painted result, or the
generator may put a painting into the scene as an object.  Name what
belongs rather than what does not: "wooden sailing ships in the
harbor", not "no modern boats".

Match the subject.  When a person or thing from the photograph will
stand in front of the new picture, describe the camera and the light it
was photographed with: "taken at eye level from a stone terrace", not a
view from above, and the same light, most often "soft overcast
daylight".  Say where the important thing goes, in the gap the subject
leaves: "one tall temple tower in the exact center of the picture".
Then `look` at the result for anything wrong for the place or the
period, such as the wrong symbol on a building.

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
- `(feather radius)` — the edge softened over `radius` pixels.  A
  feather spreads both ways.
- `(contract-selection pixels)`, `(expand-selection pixels)` — the edge
  moved in or out, except that what reaches the picture's edge stays
  there.  A soft edge comes out hard, so feather after.  To cut out a
  subject outlined by eye and not refined, `(contract-selection 2.5)`
  then `(feather 1)`: on the outline itself, a rim of what was behind it
  comes too.
- `(refine-selection &key (radius 8) (passes 5) layer)` — the edge moved
  onto the subject's own outline by color, within RADIUS pixels (1 to
  100) either side of it, judged again around the new edge for each of
  PASSES (1 to 10), on a layer or the composite; nothing farther than
  RADIUS times PASSES from the edge changes.  For a polygon drawn near a subject, before cutting
  it out.  The edge comes out hard: feather after, and contract none.
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
- `(cut-out layer &key name (defringe 3))` — the selection on `layer`
  as a new raster layer on top, for a subject to go in front of
  something else.  Where the selection covers a pixel only partly, the
  pixel is a mix of the subject and of what was behind it; `copy` keeps
  the mix, which shows as a rim of the old background, and `cut-out`
  works out the subject's own color there.  Within `defringe` pixels of
  the edge the color is drawn from deeper inside, which also takes away
  the bright rim a sharpened photograph has; 0 leaves it.  The rest of
  the subject is the layer's pixels unchanged.  A few seconds for a
  large subject.

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
