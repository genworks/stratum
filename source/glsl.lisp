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
uniform vec3 u_background;
uniform vec2 u_doc_size;
uniform vec2 u_view_offset;
uniform float u_view_scale;
uniform float u_canvas_h;
uniform int u_exact;
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
float lut(int row, float x) {
  float q = clamp(x, 0.0, 1.0) * 255.0;
  int i = int(q);
  float f = q - float(i);
  int j = min(i + 1, 255);
  return mix(texelFetch(u_luts, ivec2(i, row), 0).r, texelFetch(u_luts, ivec2(j, row), 0).r, f);
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
    (:add "min(cb + s.rgb, vec3(1.0))")))

(defun glsl-mask (mask)
  (if mask (format nil " * mask_px(~d, p)" mask) ""))

(defun glsl-entry (entry)
  (destructuring-bind (kind &key tex blend mask param lut) entry
    (ecase kind
      (:raster
       (format nil "  // layer ~d: raster, ~(~a~)
  s = layer_px(~d, p);
  a = s.a * u_opacity[~d]~a;
  cb = ao > 0.0 ? co / ao : vec3(0.0);
  co = ((1.0 - ao) * s.rgb + ao * (~a)) * a + co * (1.0 - a);
  ao = a + ao * (1.0 - a);
"
               param blend tex param (glsl-mask mask) (glsl-blend blend)))
      (:curves
       (format nil "  // layer ~d: curves
  a = u_opacity[~d]~a;
  cb = ao > 0.0 ? co / ao : vec3(0.0);
  adj = srgb_encode(cb);
  adj = srgb_decode(vec3(lut(~d, adj.r), lut(~d, adj.g), lut(~d, adj.b)));
  co = co * (1.0 - a) + adj * ao * a;
"
               param param (glsl-mask mask) lut (+ lut 1) (+ lut 2))))))

(defun plan->glsl (plan)
  "The fragment shader that composites PLAN, a list of plan entries
bottom first."
  (with-output-to-string (s)
    (format s *glsl-prelude* (max 1 (length plan)))
    (dolist (entry plan) (write-string (glsl-entry entry) s))
    (write-string "  vec3 c = ao > 0.0 ? co / ao : vec3(0.0);
  o_color = vec4(srgb_encode(c), ao);
}
" s)))
