;;;; -*- Mode: Lisp; Package: stratum -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :stratum)

;;
;; The GPU backend: a document's plan becomes one GLSL ES 3.00 fragment
;; shader that composites every layer in a single pass.  The plan holds
;; only what changes the program -- layer kinds, blend modes, which
;; layers carry masks, texture and table slots -- so dragging an
;; opacity or a curve point changes uniforms and textures and never
;; recompiles.  Raster layers live in one SRGB8_ALPHA8 texture array
;; (the sampler decodes to linear light), masks in an R8 array, curve
;; tables in an R32F texture of 256-wide rows.  The accumulator is
;; premultiplied linear RGB over an opaque background; blending follows
;; the W3C Compositing and Blending formulas.  cpu.lisp writes the same
;; arithmetic in Lisp, step for step.
;;

(defparameter *glsl-prelude* "#version 300 es
precision highp float;
precision highp int;
precision highp sampler2D;
precision highp sampler2DArray;
uniform sampler2DArray u_layers;
uniform sampler2DArray u_masks;
uniform sampler2D u_luts;
uniform float u_opacity[~d];
uniform vec4 u_params[~:*~d];
uniform vec3 u_background;
uniform vec2 u_doc_size;
uniform vec2 u_view_offset;
uniform float u_view_scale;
uniform float u_canvas_h;
uniform int u_exact;
uniform sampler2D u_stroke;
uniform int u_stroke_layer;
uniform vec4 u_stroke_color;
uniform int u_stroke_erase;
uniform int u_stroke_mask;
uniform int u_stroke_clone;
uniform vec2 u_stroke_offset;
uniform int u_adjust_layer;
uniform int u_adjust_kind;
uniform int u_adjust_row;
uniform vec4 u_adjust_params;
uniform sampler2D u_selection;
uniform int u_selection_on;
uniform float u_ants;
uniform int u_float_layer;
uniform mat3 u_float_inv;
uniform sampler2D u_replace;
uniform int u_replace_layer;
uniform sampler2D u_blur;
uniform sampler2D u_history;
out vec4 o_color;

vec3 srgb_decode(vec3 c) {
  return mix(c / 12.92, pow((c + 0.055) / 1.055, vec3(2.4)), step(vec3(0.04045), c));
}
vec3 srgb_encode(vec3 c) {
  c = clamp(c, 0.0, 1.0);
  return mix(c * 12.92, 1.055 * pow(c, vec3(1.0 / 2.4)) - 0.055, step(vec3(0.0031308), c));
}
vec4 layer_px(int i, vec2 p) {
  if (u_exact == 1) return texelFetch(u_layers, ivec3(ivec2(p), i), 0);
  return texture(u_layers, vec3(p / u_doc_size, float(i)));
}
float mask_px(int i, vec2 p) {
  if (u_exact == 1) return texelFetch(u_masks, ivec3(ivec2(p), i), 0).r;
  return texture(u_masks, vec3(p / u_doc_size, float(i))).r;
}
float stroke_px(vec2 p) {
  return texelFetch(u_stroke, ivec2(p), 0).r;
}
// What the Clone Stamp paints at p: layer i's own pixel at the stroke's
// offset, transparent beyond the document (clone-source-rect).
vec4 clone_px(int i, vec2 p) {
  vec2 q = p + u_stroke_offset;
  if (q.x < 0.0 || q.y < 0.0 || q.x >= u_doc_size.x || q.y >= u_doc_size.y) return vec4(0.0);
  return layer_px(i, q);
}
// What the History Brush paints at p: the document's history composite
// there (u_history, the opened state or the state Set Source chose).
vec4 history_px(vec2 p) {
  return texelFetch(u_history, ivec2(p), 0);
}
// What the Healing Brush paints at p: the source's texture on the
// destination's tone, blur(destination) + source - blur(source), the
// blur being u_blur, the layer blurred by the brush's sigma at the
// stroke's start (heal-stroke!, composite-heal-stroke!).
vec4 heal_px(int i, vec2 p) {
  vec2 q = p + u_stroke_offset;
  if (q.x < 0.0 || q.y < 0.0 || q.x >= u_doc_size.x || q.y >= u_doc_size.y) return vec4(0.0);
  vec4 src = layer_px(i, q);
  vec3 bs = texelFetch(u_blur, ivec2(q), 0).rgb;
  vec3 bd = texelFetch(u_blur, ivec2(p), 0).rgb;
  return vec4(clamp(bd + src.rgb - bs, 0.0, 1.0), src.a);
}
// The selection at q, with everything beyond the document unselected,
// so a selection touching the edge shows its ants there.
float selected(vec2 q) {
  if (q.x < 0.0 || q.y < 0.0 || q.x >= u_doc_size.x || q.y >= u_doc_size.y) return 0.0;
  return texelFetch(u_selection, ivec2(q), 0).r;
}
// The floating copy of layer i at source point q: bilinear over the
// four texels around it, premultiplied linear with the selection as a
// weight, nothing beyond the document; as transform-pixels! samples.
vec4 float_sample(int i, vec2 q) {
  vec2 f = q - 0.5;
  vec2 b = floor(f);
  vec2 t = f - b;
  vec4 acc = vec4(0.0);
  for (int j = 0; j < 2; j++) {
    for (int k = 0; k < 2; k++) {
      vec2 c = b + vec2(float(k), float(j));
      float wgt = (k == 0 ? 1.0 - t.x : t.x) * (j == 0 ? 1.0 - t.y : t.y);
      if (c.x >= 0.0 && c.y >= 0.0 && c.x < u_doc_size.x && c.y < u_doc_size.y) {
        vec4 px = texelFetch(u_layers, ivec3(ivec2(c), i), 0);
        float sv = u_selection_on == 1 ? texelFetch(u_selection, ivec2(c), 0).r : 1.0;
        acc += vec4(px.rgb * px.a * sv, px.a * sv) * wgt;
      }
    }
  }
  return acc;
}
// Mask i at p, with the live stroke painted on it when the stroke is on
// that mask: the mask moves toward the stroke's gray (u_stroke_color.r)
// by its coverage, as composite-mask-stroke! commits it.
float masked(int i, vec2 p) {
  float m = mask_px(i, p);
  if (u_stroke_mask == i) {
    float cv = stroke_px(p) * u_stroke_color.a;
    m = m + (u_stroke_color.r - m) * cv;
  }
  return m;
}
float lut(int row, float x) {
  float q = clamp(x, 0.0, 1.0) * 255.0;
  int i = int(q);
  float f = q - float(i);
  int j = min(i + 1, 255);
  return mix(texelFetch(u_luts, ivec2(i, row), 0).r, texelFetch(u_luts, ivec2(j, row), 0).r, f);
}
// The Color blend: the layer's hue and saturation at the backdrop's
// luminance, on linear light (color-blend in cpu.lisp is its twin).
float luminance(vec3 c) { return dot(c, vec3(0.2126, 0.7152, 0.0722)); }
vec3 blend_color(vec3 b, vec3 s) {
  float l = luminance(b);
  vec3 c = s + vec3(l - luminance(s));
  float n = min(c.r, min(c.g, c.b));
  if (n < 0.0) c = vec3(l) + (c - vec3(l)) * (l / (l - n));
  float x = max(c.r, max(c.g, c.b));
  if (x > 1.0) c = vec3(l) + (c - vec3(l)) * ((1.0 - l) / (x - l));
  return c;
}
float hue_channel(float p, float q, float t) {
  t = t - floor(t);
  if (t < 1.0 / 6.0) return p + (q - p) * 6.0 * t;
  if (t < 0.5) return q;
  if (t < 2.0 / 3.0) return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
  return p;
}
vec3 hue_saturation(vec3 c, vec4 k) {
  float mx = max(c.r, max(c.g, c.b));
  float mn = min(c.r, min(c.g, c.b));
  float l = (mx + mn) * 0.5;
  float d = mx - mn;
  float h = 0.0;
  float s = 0.0;
  if (d > 0.0) {
    s = l > 0.5 ? d / (2.0 - mx - mn) : d / (mx + mn);
    if (mx == c.r) h = (c.g - c.b) / d + (c.g < c.b ? 6.0 : 0.0);
    else if (mx == c.g) h = (c.b - c.r) / d + 2.0;
    else h = (c.r - c.g) / d + 4.0;
    h = h / 6.0;
  }
  h = h + k.x;
  h = h - floor(h);
  s = clamp(s * (1.0 + k.y), 0.0, 1.0);
  vec3 rgb;
  if (s <= 0.0) rgb = vec3(l);
  else {
    float q = l < 0.5 ? l * (1.0 + s) : l + s - l * s;
    float p = 2.0 * l - q;
    rgb = vec3(hue_channel(p, q, h + 1.0 / 3.0), hue_channel(p, q, h), hue_channel(p, q, h - 1.0 / 3.0));
  }
  return k.z > 0.0 ? rgb + (1.0 - rgb) * k.z : rgb * (1.0 + k.z);
}

void main() {
  vec2 p = u_view_offset + vec2(gl_FragCoord.x, u_canvas_h - gl_FragCoord.y) / u_view_scale;
  if (p.x < 0.0 || p.y < 0.0 || p.x >= u_doc_size.x || p.y >= u_doc_size.y) {
    vec2 q = floor(gl_FragCoord.xy / 12.0);
    o_color = vec4(vec3(0.13 + 0.025 * mod(q.x + q.y, 2.0)), 1.0);
    return;
  }
  vec3 co = u_background;
  float ao = 1.0;
  vec3 cb;
  vec3 adj;
  vec4 s;
  float a;
")

(defun glsl-blend (mode)
  (ecase mode
    (:normal "s.rgb")
    (:multiply "cb * s.rgb")
    (:screen "cb + s.rgb - cb * s.rgb")
    (:add "min(cb + s.rgb, vec3(1.0))")
    (:color "blend_color(cb, s.rgb)")))

(defun glsl-mask (mask)
  (if mask (format nil " * masked(~d, p)" mask) ""))

(defun glsl-entry (entry)
  (destructuring-bind (kind &key tex blend mask param lut) entry
    (ecase kind
      (:raster
       ;; The live stroke: a permanent stage of every raster block, idle
       ;; (u_stroke_layer -1) except on the layer being painted, where
       ;; the stroke texture's coverage composites the brush color over
       ;; the layer's own pixel before the layer blends, exactly as
       ;; composite-stroke! (brush.lisp) will commit it.  Then the
       ;; adjustment preview (u_adjust_layer, idle at -1): Image >
       ;; Adjustments shown on one layer's pixels, a table (kind 1, three
       ;; rows of u_luts from u_adjust_row) or hue/saturation (kind 2),
       ;; on the encoded scale, inside the selection, as adjust-layer! will
       ;; bake it.
       ;; Before both, the floating layer (u_float_layer, idle at -1): a
       ;; Move drag or a Free Transform shown live, the pixel being what
       ;; the cut leaves (alpha times one minus the selection) with the
       ;; copy, sampled through the inverse matrix, composited over it,
       ;; as move-pixels! and transform-pixels! (selection.lisp) commit.
       ;; Then the replacement (u_replace_layer, idle at -1): a filter's
       ;; preview, computed by the viewer into a texture of its own,
       ;; swapped in for the layer's pixels where the selection covers,
       ;; as filter-layer! (filter.lisp) will bake it.
       (format nil "  // layer ~d: raster, ~(~a~)
  s = layer_px(~d, p);
  if (u_float_layer == ~:*~d) {
    float sh = u_selection_on == 1 ? selected(p) : 1.0;
    vec2 q = (u_float_inv * vec3(p, 1.0)).xy;
    vec4 m = float_sample(~:*~d, q);
    float ba = s.a * (1.0 - sh);
    float na = m.a + ba * (1.0 - m.a);
    s.rgb = na > 0.0 ? (m.rgb + s.rgb * ba * (1.0 - m.a)) / na : vec3(0.0);
    s.a = na;
  }
  if (u_replace_layer == ~:*~d) {
    vec4 r = texelFetch(u_replace, ivec2(p), 0);
    float sv = u_selection_on == 1 ? selected(p) : 1.0;
    vec3 pm = mix(s.rgb * s.a, r.rgb * r.a, sv);
    float ra = mix(s.a, r.a, sv);
    s = vec4(ra > 0.0 ? pm / ra : vec3(0.0), ra);
  }
  if (u_stroke_layer == ~:*~d) {
    float cv = stroke_px(p) * u_stroke_color.a;
    if (cv > 0.0) {
      if (u_stroke_erase == 1) s.a = s.a * (1.0 - cv);
      else {
        vec4 paint = u_stroke_clone == 3 ? history_px(p)
                   : u_stroke_clone == 2 ? heal_px(~:*~d, p)
                   : u_stroke_clone == 1 ? clone_px(~:*~d, p) : vec4(u_stroke_color.rgb, 1.0);
        float pa = cv * paint.a;
        if (pa > 0.0) {
          float na = pa + s.a * (1.0 - pa);
          s.rgb = (paint.rgb * pa + s.rgb * s.a * (1.0 - pa)) / na;
          s.a = na;
        }
      }
    }
  }
  if (u_adjust_layer == ~:*~d) {
    adj = srgb_encode(s.rgb);
    if (u_adjust_kind == 1) adj = vec3(lut(u_adjust_row, adj.r), lut(u_adjust_row + 1, adj.g), lut(u_adjust_row + 2, adj.b));
    else adj = hue_saturation(adj, u_adjust_params);
    s.rgb = mix(s.rgb, srgb_decode(adj), u_selection_on == 1 ? selected(p) : 1.0);
  }
  a = s.a * u_opacity[~d]~a;
  cb = ao > 0.0 ? co / ao : vec3(0.0);
  co = ((1.0 - ao) * s.rgb + ao * (~a)) * a + co * (1.0 - a);
  ao = a + ao * (1.0 - a);
"
               param blend tex param (glsl-mask mask) (glsl-blend blend)))
      ((:curves :levels)
       (format nil "  // layer ~d: ~(~a~)
  a = u_opacity[~d]~a;
  cb = ao > 0.0 ? co / ao : vec3(0.0);
  adj = srgb_encode(cb);
  adj = srgb_decode(vec3(lut(~d, adj.r), lut(~d, adj.g), lut(~d, adj.b)));
  co = co * (1.0 - a) + adj * ao * a;
"
               param kind param (glsl-mask mask) lut (+ lut 1) (+ lut 2)))
      (:hue-saturation
       (format nil "  // layer ~d: hue-saturation
  a = u_opacity[~d]~a;
  cb = ao > 0.0 ? co / ao : vec3(0.0);
  adj = srgb_decode(hue_saturation(srgb_encode(cb), u_params[~d]));
  co = co * (1.0 - a) + adj * ao * a;
"
               param param (glsl-mask mask) param)))))

(defparameter *filter-glsl* "#version 300 es
precision highp float;
precision highp int;
precision highp sampler2D;
precision highp sampler2DArray;
uniform sampler2DArray u_layers;
uniform sampler2D u_src;
uniform int u_layer;
uniform int u_pass;
uniform vec2 u_size;
uniform float u_sigma;
uniform int u_extent;
uniform float u_amount;
uniform float u_threshold;
uniform int u_mono;
uniform int u_seed;
uniform vec2 u_g0;
uniform vec2 u_g1;
uniform vec4 u_c0;
uniform vec4 u_c1;
uniform int u_shape;
uniform float u_opacity;
out vec4 o_color;
// The filter passes the viewer runs for a preview, each the twin of a
// function in filter.lisp.  Pass 0 reads the layer's texels as
// premultiplied linear and blurs along rows; pass 1 reads pass 0's
// output and blurs along columns, giving straight linear RGBA (a
// Gaussian Blur ends here).  Pass 2 reads the layer and the blur
// (u_src, from pass 1) and sharpens: the premultiplied color moved
// away from its blur by u_amount of the difference where that exceeds
// u_threshold (unsharp-mask-rgba).  Pass 3 adds uniform noise to the
// encoded color channels from an integer hash of pixel, channel and
// seed (noise-hash, add-noise-rgba).  Pass 4 lays the Gradient tool's
// ramp over the layer: colors u_c0 to u_c1 (encoded, straight alpha)
// interpolated on the encoded scale along the drag u_g0 -> u_g1 by
// u_shape (0 linear, 1 reflected, 2 radial, 3 diamond, 4 angle), at
// u_opacity, composited in premultiplied linear light (gradient-rgba,
// gradient.lisp).  Edges are clamped.
vec4 fetch(ivec2 c) {
  c = clamp(c, ivec2(0), ivec2(u_size) - 1);
  if (u_pass == 0) {
    vec4 px = texelFetch(u_layers, ivec3(c, u_layer), 0);
    return vec4(px.rgb * px.a, px.a);
  }
  return texelFetch(u_src, c, 0);
}
vec3 encode(vec3 c) {
  c = clamp(c, 0.0, 1.0);
  return mix(c * 12.92, 1.055 * pow(c, vec3(1.0 / 2.4)) - 0.055, step(vec3(0.0031308), c));
}
vec3 decode(vec3 c) {
  return mix(c / 12.92, pow((c + 0.055) / 1.055, vec3(2.4)), step(vec3(0.04045), c));
}
float noise_hash(int x, int y, int c, int seed) {
  uint h = uint(x) * 374761393u + uint(y) * 668265263u + uint(c) * 1274126177u + uint(seed) * 2246822519u;
  h = (h ^ (h >> 13)) * 1274126177u;
  h = h ^ (h >> 16);
  return float(h & 0xFFFFFFu) / 16777216.0;
}
void main() {
  ivec2 p = ivec2(gl_FragCoord.xy);
  if (u_pass == 2) {
    vec4 px = texelFetch(u_layers, ivec3(p, u_layer), 0);
    vec4 orig = vec4(px.rgb * px.a, px.a);
    vec4 b = texelFetch(u_src, p, 0);
    vec3 d = orig.rgb - b.rgb * b.a;
    bool on = max(abs(d.r), max(abs(d.g), abs(d.b))) > u_threshold / 255.0;
    vec3 pm = on ? max(orig.rgb + u_amount * d, vec3(0.0)) : orig.rgb;
    // clamped as the bake's encoding clamps it: a sharpened color at
    // low alpha can overshoot white once unpremultiplied
    o_color = vec4(orig.a > 0.0 ? clamp(pm / orig.a, 0.0, 1.0) : vec3(0.0), orig.a);
    return;
  }
  if (u_pass == 4) {
    vec4 px = texelFetch(u_layers, ivec3(p, u_layer), 0);
    vec2 d = u_g1 - u_g0;
    float len2 = dot(d, d);
    vec2 q = vec2(p) + 0.5 - u_g0;
    float u = len2 > 0.0 ? dot(q, d) / len2 : 0.0;
    float v = len2 > 0.0 ? (q.y * d.x - q.x * d.y) / len2 : 0.0;
    float t;
    if (u_shape == 1) t = min(1.0, abs(u));
    else if (u_shape == 2) t = min(1.0, sqrt(u * u + v * v));
    else if (u_shape == 3) t = min(1.0, max(abs(u), abs(v)));
    else if (u_shape == 4) { float a = atan(v, u); t = (a < 0.0 ? a + 6.283185307179586 : a) / 6.283185307179586; }
    else t = clamp(u, 0.0, 1.0);
    vec4 c = u_c0 * (1.0 - t) + u_c1 * t;
    float sa = c.a * u_opacity;
    float keep = px.a * (1.0 - sa);
    float na = sa + keep;
    vec3 pm = decode(c.rgb) * sa + px.rgb * keep;
    o_color = vec4(na > 0.0 ? pm / na : vec3(0.0), na);
    return;
  }
  if (u_pass == 3) {
    vec4 px = texelFetch(u_layers, ivec3(p, u_layer), 0);
    vec3 e = encode(px.rgb) * 255.0;
    for (int c = 0; c < 3; c++) {
      float r = noise_hash(p.x, p.y, u_mono == 1 ? 0 : c, u_seed);
      e[c] = clamp(e[c] + 255.0 * u_amount * (2.0 * r - 1.0), 0.0, 255.0);
    }
    o_color = vec4(decode(e / 255.0), px.a);
    return;
  }
  vec4 acc = vec4(0.0);
  float wsum = 0.0;
  for (int i = -u_extent; i <= u_extent; i++) {
    float w = exp(-float(i * i) / (2.0 * u_sigma * u_sigma));
    acc += w * fetch(u_pass == 0 ? p + ivec2(i, 0) : p + ivec2(0, i));
    wsum += w;
  }
  acc /= wsum;
  o_color = u_pass == 0 ? acc : vec4(acc.a > 0.0 ? acc.rgb / acc.a : vec3(0.0), acc.a);
}
"
  "The filter passes the viewer runs for a preview: blur, sharpen, noise, gradient.")

(defun plan->glsl (plan)
  "The fragment shader that composites PLAN, a list of plan entries
bottom first."
  (with-output-to-string (s)
    (format s *glsl-prelude* (max 1 (length plan)))
    (dolist (entry plan) (write-string (glsl-entry entry) s))
    ;; The marching ants: where the selection changes between a pixel and
    ;; its neighbor one screen pixel away, a black-and-white dash that
    ;; u_ants moves along.  Never in an exact render: a selection is
    ;; not part of the picture.
    (write-string "  vec3 c = ao > 0.0 ? co / ao : vec3(0.0);
  o_color = vec4(srgb_encode(c), ao);
  if (u_selection_on == 1 && u_exact == 0) {
    float d = 1.0 / u_view_scale;
    vec2 pa = (u_float_inv * vec3(p, 1.0)).xy;
    bool in0 = selected(pa) >= 0.5;
    if ((selected(pa + vec2(d, 0.0)) >= 0.5) != in0 || (selected(pa - vec2(d, 0.0)) >= 0.5) != in0 ||
        (selected(pa + vec2(0.0, d)) >= 0.5) != in0 || (selected(pa - vec2(0.0, d)) >= 0.5) != in0) {
      float t = mod(gl_FragCoord.x + gl_FragCoord.y + u_ants, 8.0);
      o_color = vec4(vec3(t < 4.0 ? 0.0 : 1.0), 1.0);
    }
  }
}
" s)))
