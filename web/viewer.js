// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// The Stratum viewer.  The server sends the document's description and
// the fragment shader it generated for the current layer stack; this
// file uploads the layers' pixels once, then renders on the GPU.
// Opacity and curve drags change uniforms and the curve table only, so
// they never recompile; a structural edit (a blend mode) comes back
// with a new planVersion and the viewer fetches the new shader.  Curve
// tables are computed by composedLut, the Lisp function the server
// uses, translated by Parenscript into /stratum/dual.js, and so are a
// brush stroke's dabs (placeDabs, paintDab): the stroke is previewed
// here with them, committed by the server with the same two, and only
// the rectangle it changed comes back.
//
// window.STRATUM exposes the test hooks: parity() compares a GPU
// render with the server's CPU render pixel for pixel, strokeCheck()
// compares the browser's stroke coverage with the server's, and
// bench() measures frame times.

(function () {
  'use strict';

  var S = window.STRATUM = { stats: { draws: 0, programBuilds: 0, lutUploads: 0, strokeUploads: 0 } };
  var qs = new URLSearchParams(location.search);
  var docName = qs.get('doc') || 'demo';

  function api(path) {
    return '/stratum/api/' + path + (path.indexOf('?') < 0 ? '?' : '&') +
      'doc=' + encodeURIComponent(docName);
  }

  var VERT = '#version 300 es\n' +
    'const vec2 P[3] = vec2[3](vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));\n' +
    'void main() { gl_Position = vec4(P[gl_VertexID], 0.0, 1.0); }\n';

  var canvas, overlay, gl, vao, prog = null, uni = {}, doc = null;
  var texLayers = null, texMasks = null, texLuts = null;
  var opacity = null, params = null, lutData = null;
  var view = { scale: 1, ox: 0, oy: 0 };
  var pending = false;
  var statusEl, layersEl;

  // ---- GL plumbing ------------------------------------------------------

  function compileShader(type, src) {
    var sh = gl.createShader(type);
    gl.shaderSource(sh, src);
    gl.compileShader(sh);
    if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
      var log = gl.getShaderInfoLog(sh);
      gl.deleteShader(sh);
      throw new Error('shader compile: ' + log);
    }
    return sh;
  }

  function buildProgram(fragSrc) {
    var p = gl.createProgram();
    var vs = compileShader(gl.VERTEX_SHADER, VERT);
    var fs = compileShader(gl.FRAGMENT_SHADER, fragSrc);
    gl.attachShader(p, vs);
    gl.attachShader(p, fs);
    gl.linkProgram(p);
    gl.deleteShader(vs);
    gl.deleteShader(fs);
    if (!gl.getProgramParameter(p, gl.LINK_STATUS)) {
      throw new Error('program link: ' + gl.getProgramInfoLog(p));
    }
    if (prog) gl.deleteProgram(prog);
    prog = p;
    uni = {};
    ['u_layers', 'u_masks', 'u_luts', 'u_opacity', 'u_params', 'u_background', 'u_doc_size',
     'u_view_offset', 'u_view_scale', 'u_canvas_h', 'u_exact',
     'u_stroke', 'u_stroke_layer', 'u_stroke_color', 'u_stroke_erase', 'u_stroke_mask',
     'u_stroke_clone', 'u_stroke_offset',
     'u_adjust_layer', 'u_adjust_kind', 'u_adjust_row', 'u_adjust_params',
     'u_selection', 'u_selection_on', 'u_ants', 'u_float_layer', 'u_float_inv',
     'u_replace', 'u_replace_layer'].forEach(function (n) {
      uni[n] = gl.getUniformLocation(p, n);
    });
    S.stats.programBuilds++;
  }

  function mipLevels(w, h) { return Math.floor(Math.log2(Math.max(w, h))) + 1; }

  function arrayTexture(internal, w, h, depth) {
    var t = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, t);
    gl.texStorage3D(gl.TEXTURE_2D_ARRAY, mipLevels(w, h), internal, w, h, Math.max(1, depth));
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    return t;
  }

  function fetchBytes(url) {
    return fetch(url).then(function (r) {
      if (!r.ok) return r.text().then(function (t) { throw new Error(url + ': ' + r.status + ' ' + t); });
      return r.arrayBuffer();
    }).then(function (b) { return new Uint8Array(b); });
  }

  function fetchJSON(url, init) {
    return fetch(url, init).then(function (r) {
      if (!r.ok) return r.text().then(function (t) { throw new Error(url + ': ' + r.status + ' ' + t); });
      return r.json();
    });
  }

  // Pixels go up one layer at a time, so only one layer's bytes are in
  // flight at once (a 4K layer is 33 MB).
  function uploadPixels() {
    var w = doc.width, h = doc.height;
    if (texLayers) gl.deleteTexture(texLayers);
    if (texMasks) gl.deleteTexture(texMasks);
    texLayers = arrayTexture(gl.SRGB8_ALPHA8, w, h, doc.rasterCount);
    texMasks = arrayTexture(gl.R8, w, h, doc.maskCount);
    gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
    var chain = Promise.resolve();
    for (var i = 0; i < doc.rasterCount; i++) {
      chain = chain.then((function (i) {
        return function () {
          status('loading layer ' + (i + 1) + ' of ' + doc.rasterCount);
          return fetchBytes(api('raster?i=' + i)).then(function (bytes) {
            gl.bindTexture(gl.TEXTURE_2D_ARRAY, texLayers);
            gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, 0, 0, i, w, h, 1, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
          });
        };
      })(i));
    }
    for (var j = 0; j < doc.maskCount; j++) {
      chain = chain.then((function (j) {
        return function () {
          return fetchBytes(api('mask?i=' + j)).then(function (bytes) {
            gl.bindTexture(gl.TEXTURE_2D_ARRAY, texMasks);
            gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, 0, 0, j, w, h, 1, gl.RED, gl.UNSIGNED_BYTE, bytes);
          });
        };
      })(j));
    }
    return chain.then(function () {
      gl.bindTexture(gl.TEXTURE_2D_ARRAY, texLayers);
      gl.generateMipmap(gl.TEXTURE_2D_ARRAY);
      gl.bindTexture(gl.TEXTURE_2D_ARRAY, texMasks);
      gl.generateMipmap(gl.TEXTURE_2D_ARRAY);
    });
  }

  // ---- parameters: opacity and params uniforms, curve and levels tables ----

  // Each layer's four-number u_params entry, as its Lisp class's
  // `params' slot computes it.
  function layerParams(L) {
    if (L.kind === 'hue-saturation') {
      return [L.settings.hue / 360, L.settings.saturation, L.settings.lightness, 0];
    }
    return [0, 0, 0, 0];
  }

  function syncOpacity() {
    var n = Math.max(1, doc.layers.length);
    if (!opacity || opacity.length !== n) {
      opacity = new Float32Array(n);
      params = new Float32Array(4 * n);
    }
    doc.layers.forEach(function (L, i) {
      opacity[i] = L.visible ? L.opacity : 0;
      params.set(layerParams(L), 4 * i);
    });
  }

  function column(points, k) { return points.map(function (p) { return p[k]; }); }

  // A table layer's three tables (red, green, blue), from the same dual
  // functions the server's slots call.
  function layerLuts(L) {
    if (L.kind === 'levels') {
      var s = L.settings;
      var t = levelsLut(s['in-black'], s['in-white'], s.gamma, s['out-black'], s['out-white'], 256);
      return [t, t, t];
    }
    var m = L.curves.master;
    return ['red', 'green', 'blue'].map(function (ch) {
      var c = L.curves[ch];
      return composedLut(column(m, 0), column(m, 1), column(c, 0), column(c, 1), 256);
    });
  }

  function fillLutRows(L) {
    layerLuts(L).forEach(function (lut, k) { lutData.set(lut, (L.lutRow + k) * 256); });
  }

  // The table texture: one row per channel of each table layer, then
  // three rows for an adjustment dialog's preview (Image > Adjustments).
  function uploadLuts() {
    var rows = Math.max(1, doc.lutRows) + 3;
    if (!lutData || lutData.length !== rows * 256) {
      lutData = new Float32Array(rows * 256);
      if (texLuts) gl.deleteTexture(texLuts);
      texLuts = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texLuts);
      gl.texStorage2D(gl.TEXTURE_2D, 1, gl.R32F, 256, rows);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    }
    doc.layers.forEach(function (L) { if (L.lutRow !== null && L.lutRow !== undefined) fillLutRows(L); });
    if (adjust.luts) adjust.luts.forEach(function (lut, k) { lutData.set(lut, (rows - 3 + k) * 256); });
    gl.bindTexture(gl.TEXTURE_2D, texLuts);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, 256, rows, gl.RED, gl.FLOAT, lutData);
    S.stats.lutUploads++;
  }

  // ---- drawing ------------------------------------------------------------

  function bindAndSet(canvasH, v, exact) {
    flushStroke();
    gl.useProgram(prog);
    gl.bindVertexArray(vao);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, texLayers);
    gl.uniform1i(uni.u_layers, 0);
    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, texMasks);
    gl.uniform1i(uni.u_masks, 1);
    gl.activeTexture(gl.TEXTURE2);
    gl.bindTexture(gl.TEXTURE_2D, texLuts);
    gl.uniform1i(uni.u_luts, 2);
    gl.activeTexture(gl.TEXTURE3);
    gl.bindTexture(gl.TEXTURE_2D, texStroke);
    gl.uniform1i(uni.u_stroke, 3);
    gl.uniform1i(uni.u_stroke_layer, preview.layer);
    gl.uniform4fv(uni.u_stroke_color, preview.color);
    gl.uniform1i(uni.u_stroke_erase, preview.erase);
    gl.uniform1i(uni.u_stroke_mask, preview.mask);
    gl.uniform1i(uni.u_stroke_clone, preview.clone);
    gl.uniform2f(uni.u_stroke_offset, preview.offset[0], preview.offset[1]);
    gl.uniform1i(uni.u_adjust_layer, adjust.layer);
    gl.uniform1i(uni.u_adjust_kind, adjust.kind);
    gl.uniform1i(uni.u_adjust_row, Math.max(1, doc.lutRows));
    gl.uniform4fv(uni.u_adjust_params, adjust.params);
    gl.activeTexture(gl.TEXTURE4);
    gl.bindTexture(gl.TEXTURE_2D, texSelection);
    gl.uniform1i(uni.u_selection, 4);
    gl.uniform1i(uni.u_selection_on, selectionMask ? 1 : 0);
    gl.uniform1f(uni.u_ants, antsPhase);
    gl.uniform1i(uni.u_float_layer, floating.layer);
    gl.uniformMatrix3fv(uni.u_float_inv, false, floating.inv);
    gl.activeTexture(gl.TEXTURE5);
    gl.bindTexture(gl.TEXTURE_2D, replace.layer >= 0 ? replace.texture : texIdle);
    gl.uniform1i(uni.u_replace, 5);
    gl.uniform1i(uni.u_replace_layer, replace.layer);
    gl.uniform1fv(uni.u_opacity, opacity);
    gl.uniform4fv(uni.u_params, params);
    gl.uniform3fv(uni.u_background, doc.backgroundLinear);
    gl.uniform2f(uni.u_doc_size, doc.width, doc.height);
    gl.uniform2f(uni.u_view_offset, v.ox, v.oy);
    gl.uniform1f(uni.u_view_scale, v.scale);
    gl.uniform1f(uni.u_canvas_h, canvasH);
    gl.uniform1i(uni.u_exact, exact ? 1 : 0);
  }

  function draw() {
    pending = false;
    if (!prog || !doc || !texLuts) return;
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, canvas.width, canvas.height);
    bindAndSet(canvas.height, view, false);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    S.stats.draws++;
  }

  function requestDraw() {
    if (!pending) { pending = true; requestAnimationFrame(draw); }
  }

  function status(extra) {
    if (!statusEl) return;
    var lines = [];
    if (doc) {
      lines.push(doc.name + '  ' + doc.width + ' x ' + doc.height + '  ' + doc.layers.length + ' layers');
      lines.push('zoom ' + Math.round(view.scale * 1000) / 10 + '%  plan ' + doc.planVersion);
      lines.push('shader builds: server ' + doc.shaderBuilds + ', page ' + S.stats.programBuilds);
    }
    if (extra) lines.push(extra);
    statusEl.textContent = lines.join('\n');
  }

  function fitView() {
    var cw = canvas.width, ch = canvas.height;
    view.scale = Math.min(cw / doc.width, ch / doc.height) * 0.95;
    view.ox = doc.width / 2 - cw / (2 * view.scale);
    view.oy = doc.height / 2 - ch / (2 * view.scale);
  }

  // Zoom about the canvas's center to SCALE (device pixels per document pixel).
  function zoomTo(scale) {
    var cx = canvas.width / 2, cy = canvas.height / 2;
    var px = view.ox + cx / view.scale, py = view.oy + cy / view.scale;
    view.scale = Math.min(64, Math.max(0.02, scale));
    view.ox = px - cx / view.scale;
    view.oy = py - cy / view.scale;
    status();
    requestDraw();
  }

  S.zoomIn = function () { zoomTo(view.scale * 1.25); };
  S.zoomOut = function () { zoomTo(view.scale / 1.25); };
  S.fitOnScreen = function () { fitView(); status(); requestDraw(); };
  S.actualPixels = function () { zoomTo(1); };

  function resize() {
    var dpr = window.devicePixelRatio || 1;
    var w = Math.max(1, Math.round(canvas.clientWidth * dpr));
    var h = Math.max(1, Math.round(canvas.clientHeight * dpr));
    if (canvas.width !== w || canvas.height !== h) {
      var cx = view.ox + canvas.width / (2 * view.scale);
      var cy = view.oy + canvas.height / (2 * view.scale);
      canvas.width = w;
      canvas.height = h;
      if (overlay) { overlay.width = w; overlay.height = h; }
      view.ox = cx - w / (2 * view.scale);
      view.oy = cy - h / (2 * view.scale);
      requestDraw();
    }
  }

  // ---- tools: pan, and brush stroke capture ------------------------------

  // The canvas's pointer goes to the current tool: 'pan' drags the
  // view; 'brush' and 'eraser' record a stroke -- every sample,
  // coalesced ones included, in document pixels with pressure and tilt
  // -- and hand it to the stroke hooks below: S.onStrokeSamples(stroke,
  // fresh) as samples arrive (the live preview) and S.onStroke(stroke)
  // when the pointer lifts (the commit).  Space held is pan whatever the
  // tool, and the keys are the usual ones: B brush, E eraser, H hand.
  S.tool = 'pan';
  S.activeLayer = null;
  var spaceHeld = false;

  function effectiveTool() { return spaceHeld ? 'pan' : S.tool; }
  function paints(tool) { return tool === 'brush' || tool === 'eraser' || tool === 'stamp'; }

  var CURSORS = { brush: 'crosshair', eraser: 'crosshair', stamp: 'crosshair', eyedropper: 'crosshair', zoom: 'zoom-in',
                  marquee: 'crosshair', lasso: 'crosshair', move: 'move', wand: 'crosshair', crop: 'crosshair',
                  gradient: 'crosshair' };
  function selects(tool) { return tool === 'marquee' || tool === 'lasso' || tool === 'crop'; }

  // Shift adds to the selection, Alt takes from it, as in other editors.
  function selectionMode(e) { return e.shiftKey ? 'add' : (e.altKey ? 'subtract' : 'new'); }

  // The Magic Wand's options.
  S.wand = { tolerance: 32, contiguous: true };

  // The Gradient tool's options: which two colors, the ramp's shape,
  // its opacity, and whether it runs the other way.  The presets are
  // functions of the swatches, read when a drag starts.
  S.gradient = { preset: 'Foreground to Background', shape: 'linear', opacity: 1, reverse: false };
  var GRADIENT_PRESETS = {
    'Foreground to Background': function () { return [S.brush.color.concat(255), S.background.concat(255)]; },
    'Foreground to Transparent': function () { return [S.brush.color.concat(255), S.brush.color.concat(0)]; },
    'Black, White': function () { return [[0, 0, 0, 255], [255, 255, 255, 255]]; }
  };
  var GRADIENT_SHAPES = ['linear', 'reflected', 'radial', 'diamond', 'angle'];
  S.gradientColors = function () {
    var c = GRADIENT_PRESETS[S.gradient.preset]();
    return S.gradient.reverse ? [c[1], c[0]] : c;
  };

  // The Clone Stamp: Alt-click sets the source point on the active
  // layer; a stroke then paints the layer's own pixels from the source,
  // offset by the distance from the source to where the stroke began.
  // Aligned keeps that offset for every later stroke; otherwise each
  // stroke starts from the source again.
  S.clone = { aligned: true };
  S.cloneSource = null;
  S.cloneOffset = null;

  S.setCloneSource = function (x, y) {
    S.cloneSource = { x: x, y: y };
    S.cloneOffset = null;
    status('clone source set at ' + Math.round(x) + ', ' + Math.round(y));
  };

  function cloneOffsetFor(p0) {
    if (S.clone.aligned && S.cloneOffset) return S.cloneOffset;
    var off = [Math.round(S.cloneSource.x - p0.x), Math.round(S.cloneSource.y - p0.y)];
    if (S.clone.aligned) S.cloneOffset = off;
    return off;
  }

  // The crosshair over the pixels a clone stroke is reading.
  function drawCloneMark(q) {
    if (!overlay) return;
    var ctx = overlay.getContext('2d'), c = toCanvas(q);
    ctx.clearRect(0, 0, overlay.width, overlay.height);
    ctx.setLineDash([]);
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(c[0] - 8, c[1]); ctx.lineTo(c[0] - 3, c[1]);
    ctx.moveTo(c[0] + 3, c[1]); ctx.lineTo(c[0] + 8, c[1]);
    ctx.moveTo(c[0], c[1] - 8); ctx.lineTo(c[0], c[1] - 3);
    ctx.moveTo(c[0], c[1] + 3); ctx.lineTo(c[0], c[1] + 8);
    ctx.strokeStyle = '#000';
    ctx.stroke();
    ctx.strokeStyle = '#fff';
    ctx.beginPath();
    ctx.arc(c[0], c[1], 5, 0, 2 * Math.PI);
    ctx.stroke();
  }

  // The drag's end, held to a multiple of 45 degrees with Shift.
  function gradientEnd(a, b, constrain) {
    if (!constrain) return b;
    var dx = b[0] - a[0], dy = b[1] - a[1], len = Math.hypot(dx, dy);
    if (!len) return b;
    var ang = Math.round(Math.atan2(dy, dx) / (Math.PI / 4)) * (Math.PI / 4);
    return [a[0] + len * Math.cos(ang), a[1] + len * Math.sin(ang)];
  }

  function gradientBody(g) {
    var colors = g.colors;
    return { layer: g.layer, x0: g.start[0], y0: g.start[1], x1: g.end[0], y1: g.end[1],
             start: colors[0], end: colors[1], shape: S.gradient.shape, opacity: S.gradient.opacity };
  }

  function setCursor(dragging) {
    canvas.style.cursor = CURSORS[effectiveTool()] || (dragging ? 'grabbing' : 'grab');
  }

  // The tools that work: a tool from the palette that is not built
  // yet is refused with a word in the status lines.
  S.setTool = function (tool) {
    var t = toolSpec(tool);
    if (t && !t.built) { status(t.label + ' is not built yet'); return false; }
    S.tool = t ? tool : 'pan';
    document.querySelectorAll('#stratum-tools .st-tool-btn').forEach(function (b) {
      b.classList.toggle('st-on', b.getAttribute('data-tool') === S.tool);
    });
    setCursor(false);
    return true;
  };

  // The eyedropper: the composited color under the pointer, from the
  // canvas as drawn, into the foreground.
  function pickColor(e) {
    var dpr = window.devicePixelRatio || 1;
    var r = canvas.getBoundingClientRect();
    var x = Math.floor((e.clientX - r.left) * dpr), y = Math.floor((e.clientY - r.top) * dpr);
    draw();
    var px = new Uint8Array(4);
    gl.readPixels(x, canvas.height - 1 - y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, px);
    S.setBrush({ color: [px[0], px[1], px[2]] });
    status('picked ' + hexColor(S.brush.color));
  }

  // The zoom tool: in about the pointer; out with Alt held.
  function zoomAt(e, factor) {
    var dpr = window.devicePixelRatio || 1;
    var r = canvas.getBoundingClientRect();
    var cx = (e.clientX - r.left) * dpr, cy = (e.clientY - r.top) * dpr;
    var px = view.ox + cx / view.scale, py = view.oy + cy / view.scale;
    view.scale = Math.min(64, Math.max(0.02, view.scale * factor));
    view.ox = px - cx / view.scale;
    view.oy = py - cy / view.scale;
    status();
    requestDraw();
  }

  // A pointer event as a stroke sample: document pixels (x right, y
  // down), pressure 0..1 (a pen's; a mouse or finger paints at full
  // pressure), tilt in degrees, and the event's time in milliseconds.
  function strokeSample(e) {
    var dpr = window.devicePixelRatio || 1;
    var r = canvas.getBoundingClientRect();
    return {
      x: view.ox + (e.clientX - r.left) * dpr / view.scale,
      y: view.oy + (e.clientY - r.top) * dpr / view.scale,
      pressure: e.pointerType === 'pen' ? e.pressure : 1,
      tiltX: e.tiltX || 0,
      tiltY: e.tiltY || 0,
      t: e.timeStamp
    };
  }

  function coalesced(e) {
    var list = e.getCoalescedEvents ? e.getCoalescedEvents() : [];
    return list.length ? list : [e];
  }

  // A marquee or lasso gesture: its points in document pixels, drawn on
  // the overlay while the pointer is down, sent to the server when it
  // lifts.
  function docPoint(e) {
    var s = strokeSample(e);
    return [s.x, s.y];
  }

  function toCanvas(p) {
    var dpr = window.devicePixelRatio || 1;
    return [(p[0] - view.ox) * view.scale, (p[1] - view.oy) * view.scale];
  }

  function drawGesture(g) {
    if (!overlay) return;
    var ctx = overlay.getContext('2d');
    ctx.clearRect(0, 0, overlay.width, overlay.height);
    if (!g) return;
    if (g.tool === 'crop') {
      // the crop box: everything outside it darkened
      var a0 = toCanvas(g.points[0]), b0 = toCanvas(g.points[g.points.length - 1]);
      var x0 = Math.min(a0[0], b0[0]), y0 = Math.min(a0[1], b0[1]);
      var x1 = Math.max(a0[0], b0[0]), y1 = Math.max(a0[1], b0[1]);
      ctx.fillStyle = 'rgba(0, 0, 0, 0.55)';
      ctx.fillRect(0, 0, overlay.width, y0);
      ctx.fillRect(0, y1, overlay.width, overlay.height - y1);
      ctx.fillRect(0, y0, x0, y1 - y0);
      ctx.fillRect(x1, y0, overlay.width - x1, y1 - y0);
      ctx.setLineDash([]);
      ctx.lineWidth = 1;
      ctx.strokeStyle = '#fff';
      ctx.strokeRect(x0 + 0.5, y0 + 0.5, x1 - x0 - 1, y1 - y0 - 1);
      return;
    }
    ctx.setLineDash([4, 4]);
    ctx.lineWidth = 1;
    ctx.beginPath();
    if (g.tool === 'marquee') {
      var a = toCanvas(g.points[0]), b = toCanvas(g.points[g.points.length - 1]);
      ctx.rect(Math.min(a[0], b[0]), Math.min(a[1], b[1]), Math.abs(b[0] - a[0]), Math.abs(b[1] - a[1]));
    } else {
      g.points.forEach(function (p, k) { var c = toCanvas(p); if (k === 0) ctx.moveTo(c[0], c[1]); else ctx.lineTo(c[0], c[1]); });
    }
    ctx.strokeStyle = '#000';
    ctx.stroke();
    ctx.lineDashOffset = 4;
    ctx.strokeStyle = '#fff';
    ctx.stroke();
  }

  // The crop box stays up after the drag until Enter or a double-click
  // commits it, or Escape drops it; a new drag replaces it.
  S.cropBox = null;

  function cropRect(g) {
    var a = g.points[0], b = g.points[g.points.length - 1];
    var x = Math.round(Math.min(a[0], b[0])), y = Math.round(Math.min(a[1], b[1]));
    return { x: x, y: y, w: Math.round(Math.max(a[0], b[0])) - x, h: Math.round(Math.max(a[1], b[1])) - y };
  }

  S.commitCrop = function () {
    var r = S.cropBox;
    if (!r) return Promise.resolve(null);
    S.cropBox = null;
    drawGesture(null);
    return S.resize({ op: 'crop-rect', x: r.x, y: r.y, w: r.w, h: r.h });
  };

  S.cancelCrop = function () {
    S.cropBox = null;
    drawGesture(null);
  };

  function finishGesture(g) {
    if (g.tool === 'crop') {
      var r = cropRect(g);
      if (r.w > 0 && r.h > 0) { S.cropBox = r; drawGesture(g); status('crop: Enter or double-click commits, Escape cancels'); }
      else S.cancelCrop();
      return Promise.resolve(S.cropBox);
    }
    drawGesture(null);
    if (g.tool === 'marquee') {
      var a = g.points[0], b = g.points[g.points.length - 1];
      var x = Math.round(Math.min(a[0], b[0])), y = Math.round(Math.min(a[1], b[1]));
      var w = Math.round(Math.max(a[0], b[0])) - x, h = Math.round(Math.max(a[1], b[1])) - y;
      return S.select(w > 0 && h > 0 ? { op: 'rect', x: x, y: y, w: w, h: h, mode: g.mode } : { op: 'none' });
    }
    return S.select(g.points.length >= 3 ? { op: 'polygon', points: g.points, mode: g.mode } : { op: 'none' });
  }

  function wandAt(e) {
    var p = docPoint(e);
    return S.select({ op: 'wand', x: Math.floor(p[0]), y: Math.floor(p[1]), tolerance: S.wand.tolerance,
                      contiguous: S.wand.contiguous, mode: selectionMode(e), layer: S.activeLayer });
  }

  function installNavigation() {
    var drag = null, stroke = null, gesture = null, mover = null, grad = null;
    canvas.addEventListener('pointerdown', function (e) {
      try { canvas.setPointerCapture(e.pointerId); } catch (err) {}
      if (S.transforming) { S.transforming.down(e); return; }
      var tool = effectiveTool();
      if (tool === 'stamp' && e.altKey) {
        var sp = docPoint(e);
        S.setCloneSource(sp[0], sp[1]);
      } else if (tool === 'stamp' && !S.cloneSource) {
        status('Alt-click to set the clone source');
      } else if (paints(tool)) {
        stroke = { layer: S.activeLayer, pointerType: e.pointerType, tool: tool,
                   samples: [strokeSample(e)] };
        S.onStrokeSamples(stroke, stroke.samples.slice());
        if (stroke.clone) drawCloneMark([stroke.samples[0].x + stroke.clone[0], stroke.samples[0].y + stroke.clone[1]]);
      } else if (tool === 'gradient') {
        var GL = activeL();
        if (!GL || GL.kind !== 'raster') { status('pick a raster layer for a gradient'); return; }
        var p0 = docPoint(e);
        grad = { start: p0, end: p0, layer: GL.index, texIndex: GL.texIndex, colors: S.gradientColors() };
      } else if (selects(tool)) {
        gesture = { tool: tool, points: [docPoint(e)], mode: selectionMode(e) };
        drawGesture(gesture);
      } else if (tool === 'wand') {
        wandAt(e);
      } else if (tool === 'move') {
        var L = activeL();
        if (!L || L.kind !== 'raster') { status('pick a raster layer to move'); return; }
        mover = { start: docPoint(e), layer: L.index, texIndex: L.texIndex, dx: 0, dy: 0 };
        floating.layer = L.texIndex;
        setFloatTranslation(0, 0);
      } else if (tool === 'eyedropper') {
        pickColor(e);
      } else if (tool === 'zoom') {
        zoomAt(e, e.altKey ? 1 / 1.5 : 1.5);
      } else {
        drag = { x: e.clientX, y: e.clientY };
        setCursor(true);
      }
    });
    canvas.addEventListener('pointermove', function (e) {
      if (S.transforming) { S.transforming.move(e); return; }
      if (stroke) {
        var fresh = coalesced(e).map(strokeSample);
        Array.prototype.push.apply(stroke.samples, fresh);
        S.onStrokeSamples(stroke, fresh);
        if (stroke.clone) {
          var last = stroke.samples[stroke.samples.length - 1];
          drawCloneMark([last.x + stroke.clone[0], last.y + stroke.clone[1]]);
        }
        return;
      }
      if (gesture) {
        if (gesture.tool === 'marquee') gesture.points = [gesture.points[0], docPoint(e)];
        else gesture.points.push(docPoint(e));
        drawGesture(gesture);
        return;
      }
      if (grad) {
        grad.end = gradientEnd(grad.start, docPoint(e), e.shiftKey);
        drawGesture({ tool: 'gradient', points: [grad.start, grad.end] });
        previewGradient(grad);
        return;
      }
      if (mover) {
        var q = docPoint(e);
        mover.dx = Math.round(q[0] - mover.start[0]);
        mover.dy = Math.round(q[1] - mover.start[1]);
        setFloatTranslation(mover.dx, mover.dy);
        requestDraw();
        return;
      }
      if (!drag) return;
      var dpr = window.devicePixelRatio || 1;
      view.ox -= (e.clientX - drag.x) * dpr / view.scale;
      view.oy -= (e.clientY - drag.y) * dpr / view.scale;
      drag = { x: e.clientX, y: e.clientY };
      requestDraw();
    });
    function end(e) {
      if (S.transforming) { S.transforming.up(e); return; }
      if (stroke) {
        var done = stroke;
        stroke = null;
        if (done.clone) {
          var cb = S.cropBox;
          drawGesture(cb ? { tool: 'crop', points: [[cb.x, cb.y], [cb.x + cb.w, cb.y + cb.h]] } : null);
        }
        if (e.type === 'pointerup') S.onStroke(done);
      }
      if (gesture) {
        var g = gesture;
        gesture = null;
        if (e.type === 'pointerup') finishGesture(g); else drawGesture(null);
      }
      if (grad) {
        var gd = grad;
        grad = null;
        drawGesture(null);
        var len = Math.hypot(gd.end[0] - gd.start[0], gd.end[1] - gd.start[1]);
        if (e.type === 'pointerup' && len >= 1) S.gradientFill(gradientBody(gd));
        else clearReplace();
      }
      if (mover) {
        var m = mover;
        mover = null;
        if (e.type === 'pointerup' && (m.dx !== 0 || m.dy !== 0)) S.move(m.layer, m.dx, m.dy);
        else { floating.layer = -1; requestDraw(); }
      }
      drag = null;
      setCursor(false);
    }
    canvas.addEventListener('pointerup', end);
    canvas.addEventListener('pointercancel', end);
    window.addEventListener('keydown', function (e) {
      if (e.target && /^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) return;
      var mod = e.ctrlKey || e.metaKey, key = e.key.toLowerCase();
      if (e.code === 'Space') { if (!spaceHeld) { spaceHeld = true; setCursor(false); } e.preventDefault(); }
      else if (e.key === 'Enter' && S.transforming) { e.preventDefault(); S.transforming.commit(); }
      else if (e.key === 'Escape' && S.transforming) { e.preventDefault(); S.transforming.cancel(); }
      else if (e.key === 'Enter' && S.cropBox) { e.preventDefault(); S.commitCrop(); }
      else if (e.key === 'Escape' && S.cropBox) { e.preventDefault(); S.cancelCrop(); }
      else if (mod && key === 'n' && !e.shiftKey) { e.preventDefault(); S.openNew(); }
      else if (e.key === 'Escape' && S.newDialog) S.newDialog.cancel();
      else if (e.key === 'Enter' && S.newDialog) S.newDialog.ok();
      else if (mod && key === 't') { e.preventDefault(); S.beginTransform(); }
      else if (mod && !e.shiftKey && key === 'z') { e.preventDefault(); S.undo(); }
      else if (mod && ((e.shiftKey && key === 'z') || key === 'y')) { e.preventDefault(); S.redo(); }
      else if (mod && key === 'o') { e.preventDefault(); chooseFile(S.openFile); }
      else if (mod && e.altKey && key === 'i') { e.preventDefault(); S.openImageSize(); }
      else if (mod && e.altKey && key === 'c') { e.preventDefault(); S.openCanvasSize(); }
      else if (e.key === 'Escape' && S.sizeDialog) S.sizeDialog.cancel();
      else if (e.key === 'Enter' && S.sizeDialog) S.sizeDialog.ok();
      else if (mod && key === 'l') { e.preventDefault(); S.openAdjust('levels'); }
      else if (mod && key === 'm') { e.preventDefault(); S.openAdjust('curves'); }
      else if (mod && key === 'u') { e.preventDefault(); S.openAdjust('hue-saturation'); }
      else if (mod && !e.shiftKey && key === 'a') { e.preventDefault(); S.select({ op: 'all' }); }
      else if (mod && key === 'x') { e.preventDefault(); S.clipboard('cut'); }
      else if (mod && key === 'c') { e.preventDefault(); S.clipboard('copy'); }
      else if (mod && key === 'v') { e.preventDefault(); S.clipboard('paste'); }
      else if (mod && key === 'd') { e.preventDefault(); S.select({ op: 'none' }); }
      else if (mod && e.shiftKey && key === 'i') { e.preventDefault(); S.select({ op: 'inverse' }); }
      else if (e.shiftKey && e.key === 'F5') { e.preventDefault(); S.openFill(); }
      else if (e.shiftKey && e.key === 'F6') { e.preventDefault(); S.openFeather(); }
      else if (e.key === 'Escape' && S.featherDialog) S.featherDialog.cancel();
      else if (e.key === 'Enter' && S.featherDialog) S.featherDialog.ok();
      else if ((e.key === 'Delete' || e.key === 'Backspace') && !mod) { e.preventDefault(); S.clear(); }
      else if (e.key === 'Escape' && S.fillDialog) S.fillDialog.cancel();
      else if (e.key === 'Enter' && S.fillDialog) S.fillDialog.ok();
      else if (e.key === 'Escape' && S.filterDialog) S.filterDialog.cancel();
      else if (e.key === 'Enter' && S.filterDialog) S.filterDialog.ok();
      else if (e.key === 'Escape' && S.adjustDialog) S.adjustDialog.cancel();
      else if (e.key === 'Enter' && S.adjustDialog) S.adjustDialog.ok();
      else if (mod && e.shiftKey && key === 's') { e.preventDefault(); S.saveAs(); }
      else if (mod && key === 's') { e.preventDefault(); S.saveDocument(); }
      else if (mod && e.shiftKey && key === 'n') { e.preventDefault(); S.layerOp({ op: 'add', kind: 'raster' }); }
      else if (mod && key === 'j') { e.preventDefault(); S.layerOp({ op: 'duplicate' }); }
      else if (mod && key === 'e') { e.preventDefault(); if (canMergeDown()) S.layerOp({ op: 'merge' }); }
      else if (mod && e.key === ']') { e.preventDefault(); S.layerOp({ op: 'move', delta: 1 }); }
      else if (mod && e.key === '[') { e.preventDefault(); S.layerOp({ op: 'move', delta: -1 }); }
      else if (mod && (e.key === '=' || e.key === '+')) { e.preventDefault(); S.zoomIn(); }
      else if (mod && e.key === '-') { e.preventDefault(); S.zoomOut(); }
      else if (mod && e.key === '0') { e.preventDefault(); S.fitOnScreen(); }
      else if (mod && e.key === '1') { e.preventDefault(); S.actualPixels(); }
      else if (mod || e.altKey) return;
      else if (e.key === 'Escape') closeMenus();
      else if (e.key === '[') S.setBrush({ radius: Math.max(1, Math.round(S.brush.radius / 1.25)) });
      else if (e.key === ']') S.setBrush({ radius: Math.min(300, Math.round(S.brush.radius * 1.25)) });
      else if (key === 'x') S.swapColors();
      else if (key === 'd') S.defaultColors();
      else {
        var t = TOOLS.filter(function (x) { return x !== '-' && x.key === e.key.toUpperCase(); })[0];
        if (t) S.setTool(t.id);
      }
    });
    window.addEventListener('keyup', function (e) {
      if (e.code === 'Space') { spaceHeld = false; setCursor(false); }
    });
    canvas.addEventListener('wheel', function (e) {
      e.preventDefault();
      var dpr = window.devicePixelRatio || 1;
      var r = canvas.getBoundingClientRect();
      var cx = (e.clientX - r.left) * dpr, cy = (e.clientY - r.top) * dpr;
      var px = view.ox + cx / view.scale, py = view.oy + cy / view.scale;
      view.scale = Math.min(64, Math.max(0.02, view.scale * Math.exp(-e.deltaY * 0.0015)));
      view.ox = px - cx / view.scale;
      view.oy = py - cy / view.scale;
      status();
      requestDraw();
    }, { passive: false });
    canvas.addEventListener('dblclick', function () {
      if (S.transforming) { S.transforming.commit(); return; }
      if (S.cropBox) { S.commitCrop(); return; }
      fitView(); status(); requestDraw();
    });
  }

  // Re-upload one rectangle of raster layer I (its texture index) from
  // the server, in document pixels -- what a committed stroke needs in
  // place of a whole-layer upload -- then rebuild the mipmaps.  Mipmaps
  // are rebuilt for the whole array: a per-region rebuild is the brush
  // engine's to add if a stroke's commit shows it.
  S.uploadRect = function (i, x, y, w, h) {
    return fetchBytes(api('raster?i=' + i + '&x=' + x + '&y=' + y + '&w=' + w + '&h=' + h))
      .then(function (bytes) {
        if (bytes.length !== w * h * 4) throw new Error('uploadRect: ' + bytes.length + ' bytes for ' + w + ' x ' + h);
        gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
        gl.bindTexture(gl.TEXTURE_2D_ARRAY, texLayers);
        gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, x, y, i, w, h, 1, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
        gl.generateMipmap(gl.TEXTURE_2D_ARRAY);
        requestDraw();
        return bytes.length;
      });
  };

  // ---- the brush ----------------------------------------------------------
  //
  // A stroke is previewed here and committed by the server with the
  // same two dual functions on both sides: placeDabs and paintDab in
  // dual.js are place-dabs and paint-dab in brush.lisp.  As samples
  // arrive, the dabs they add are painted into strokeBuf, a float64
  // coverage buffer the size of the document; each frame uploads only
  // the rectangle that changed into the R32F stroke texture, and the
  // generated shader composites the brush color through that coverage
  // onto the stroke's layer (u_stroke_layer), before the layer blends.
  // When the pointer lifts the SAMPLES go to POST /stratum/api/stroke,
  // the server paints them into the layer's pixels and answers the
  // dirty rectangle, and the viewer re-uploads that rectangle alone and
  // drops the preview.  Strokes and undos queue on one promise chain,
  // so a stroke begun before the last commit has returned waits for it
  // instead of painting into a buffer about to be cleared.

  S.brush = { radius: 24, hardness: 0.8, spacing: 0.25, flow: 1, opacity: 1,
              color: [230, 80, 70], mode: 'paint' };
  S.undoDepth = 0;
  S.redoDepth = 0;
  var strokeBuf = null, texStroke = null, strokeDirty = null, frameDirty = null;
  var preview = { layer: -1, mask: -1, color: [0, 0, 0, 0], erase: 0, clone: 0, offset: [0, 0] };
  var strokeChain = Promise.resolve();

  // Painting on the active layer's mask instead of its pixels: picked by
  // clicking the mask thumbnail in the Layers panel, as in other editors.
  S.maskTarget = false;

  function srgbToLinear(c) {
    c = c / 255;
    return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
  }

  function linearToSrgb(c) {
    c = Math.min(1, Math.max(0, c));
    return c <= 0.0031308 ? c * 12.92 : 1.055 * Math.pow(c, 1 / 2.4) - 0.055;
  }

  // The value a color paints on a mask, as stroke-gray computes it.
  function maskGray(color) {
    return linearToSrgb(0.2126 * srgbToLinear(color[0]) + 0.7152 * srgbToLinear(color[1]) +
                        0.0722 * srgbToLinear(color[2]));
  }

  function unionRect(a, b) {
    if (!a) return b;
    var x0 = Math.min(a.x, b.x), y0 = Math.min(a.y, b.y);
    var x1 = Math.max(a.x + a.w, b.x + b.w), y1 = Math.max(a.y + a.h, b.y + b.h);
    return { x: x0, y: y0, w: x1 - x0, h: y1 - y0 };
  }

  // The pixels a dab can touch, clipped to the document; null when none.
  function dabRect(cx, cy, r) {
    var x0 = Math.max(0, Math.floor(cx - r)), y0 = Math.max(0, Math.floor(cy - r));
    var x1 = Math.min(doc.width - 1, Math.ceil(cx + r)), y1 = Math.min(doc.height - 1, Math.ceil(cy + r));
    return x0 <= x1 && y0 <= y1 ? { x: x0, y: y0, w: x1 - x0 + 1, h: y1 - y0 + 1 } : null;
  }

  // The stroke texture: one pixel until the first stroke, then the
  // document's size, so a document that is only looked at pays nothing.
  function strokeTexture(w, h) {
    if (texStroke) gl.deleteTexture(texStroke);
    texStroke = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texStroke);
    gl.texStorage2D(gl.TEXTURE_2D, 1, gl.R32F, w, h);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  }

  function ensureStrokeBuffer() {
    if (strokeBuf && strokeBuf.length === doc.width * doc.height) return;
    strokeBuf = new Float64Array(doc.width * doc.height);
    strokeTexture(doc.width, doc.height);
  }

  // Rectangle R of strokeBuf into the stroke texture (float32 there),
  // multiplied by the selection on the way, as the server multiplies
  // its coverage (clip-coverage!): the buffer itself stays unclipped,
  // since later dabs still accumulate into it.
  function uploadStrokeRect(r) {
    var sub = new Float32Array(r.w * r.h);
    for (var j = 0; j < r.h; j++) {
      var src = (r.y + j) * doc.width + r.x, dst = j * r.w;
      for (var i = 0; i < r.w; i++) sub[dst + i] = strokeBuf[src + i];
    }
    clipCoverage(sub, r.w, r.h, r.x, r.y);
    gl.bindTexture(gl.TEXTURE_2D, texStroke);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, r.x, r.y, r.w, r.h, gl.RED, gl.FLOAT, sub);
    S.stats.strokeUploads++;
  }

  function flushStroke() {
    if (frameDirty) { uploadStrokeRect(frameDirty); frameDirty = null; }
  }

  function texIndexOf(layerIndex) {
    var L = doc.layers[layerIndex];
    return L && L.kind === 'raster' ? L.texIndex : -1;
  }

  function sampleVectors(stroke) {
    var n = stroke.samples.length;
    var v = { n: n, xs: new Float64Array(n), ys: new Float64Array(n), ps: new Float64Array(n) };
    for (var i = 0; i < n; i++) {
      v.xs[i] = stroke.samples[i].x;
      v.ys[i] = stroke.samples[i].y;
      v.ps[i] = stroke.samples[i].pressure;
    }
    return v;
  }

  // The dabs of a stroke's samples so far, as the server will place
  // them: placeDabs once to count, once to fill.
  function strokeDabs(stroke) {
    var v = sampleVectors(stroke), B = stroke.brush;
    var count = placeDabs(v.xs, v.ys, v.ps, v.n, B.radius, B.spacing, new Float64Array(0));
    var dabs = new Float64Array(3 * count);
    placeDabs(v.xs, v.ys, v.ps, v.n, B.radius, B.spacing, dabs);
    return dabs;
  }

  // Paint the dabs not yet painted into strokeBuf.  Earlier dabs never
  // move when samples are added, so the count painted is all the state.
  function rasterizeStroke(stroke) {
    var dabs = strokeDabs(stroke), B = stroke.brush, count = dabs.length / 3;
    for (var k = stroke.dabsDone || 0; k < count; k++) {
      var cx = dabs[3 * k], cy = dabs[3 * k + 1], r = B.radius * dabs[3 * k + 2];
      var rect = dabRect(cx, cy, r);
      if (!rect) continue;
      paintDab(strokeBuf, doc.width, doc.height, 0, 0, cx, cy, r, B.hardness, B.flow);
      frameDirty = unionRect(frameDirty, rect);
      strokeDirty = unionRect(strokeDirty, rect);
    }
    stroke.dabsDone = count;
  }

  function beginPreview(stroke) {
    var c = stroke.brush.color;
    if (stroke.target === 'mask') {
      var g = maskGray(c);
      preview.layer = -1;
      preview.mask = stroke.maskIndex;
      preview.color = [g, g, g, stroke.brush.opacity];
      preview.erase = 0;
    } else {
      preview.layer = stroke.texIndex;
      preview.mask = -1;
      preview.color = [srgbToLinear(c[0]), srgbToLinear(c[1]), srgbToLinear(c[2]), stroke.brush.opacity];
      preview.erase = stroke.brush.mode === 'erase' ? 1 : 0;
    }
    preview.clone = stroke.clone ? 1 : 0;
    preview.offset = stroke.clone || [0, 0];
  }

  function clearPreview() {
    if (strokeDirty) {
      for (var j = 0; j < strokeDirty.h; j++) {
        var from = (strokeDirty.y + j) * doc.width + strokeDirty.x;
        strokeBuf.fill(0, from, from + strokeDirty.w);
      }
      uploadStrokeRect(strokeDirty);
      strokeDirty = null;
    }
    frameDirty = null;
    preview.layer = -1;
    preview.mask = -1;
    requestDraw();
  }

  function strokeBody(stroke) {
    return JSON.stringify({
      layer: stroke.layer, target: stroke.target, brush: stroke.brush, clone: stroke.clone || null,
      samples: stroke.samples.map(function (s) { return [s.x, s.y, s.pressure]; })
    });
  }

  // One rectangle of mask I back from the server into the mask array.
  S.uploadMaskRect = function (i, x, y, w, h) {
    return fetchBytes(api('mask?i=' + i + '&x=' + x + '&y=' + y + '&w=' + w + '&h=' + h))
      .then(function (bytes) {
        if (bytes.length !== w * h) throw new Error('uploadMaskRect: ' + bytes.length + ' bytes for ' + w + ' x ' + h);
        gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
        gl.bindTexture(gl.TEXTURE_2D_ARRAY, texMasks);
        gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, 0, x, y, i, w, h, 1, gl.RED, gl.UNSIGNED_BYTE, bytes);
        gl.generateMipmap(gl.TEXTURE_2D_ARRAY);
        requestDraw();
        return bytes.length;
      });
  };

  function postJSON(path, body) {
    return fetchJSON(api(path), { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: body || '{}' });
  }

  function tookRect(res) {
    S.undoDepth = res.undoDepth;
    S.redoDepth = res.redoDepth || 0;
    if (res.target === 'mask') {
      doc.layers[res.layer].maskVersion = res.maskVersion;
      S.refreshMaskThumb(res.maskIndex);
      return S.uploadMaskRect(res.maskIndex, res.x, res.y, res.w, res.h);
    }
    doc.layers[res.layer].paintVersion = res.paintVersion;
    S.refreshThumb(res.texIndex);
    return S.uploadRect(res.texIndex, res.x, res.y, res.w, res.h);
  }

  function commitStroke(stroke) {
    return postJSON('stroke', strokeBody(stroke)).then(function (res) {
      return res.empty ? null : tookRect(res);
    }).then(function () { clearPreview(); status(); },
            function (e) { clearPreview(); status('stroke failed: ' + e.message); });
  }

  // A stroke goes to the active layer's pixels, or to its mask when the
  // mask is the target; on a mask the eraser paints the background color.
  S.onStrokeSamples = function (stroke) {
    if (stroke.texIndex === undefined) {
      var L = doc.layers[stroke.layer];
      stroke.brush = JSON.parse(JSON.stringify(S.brush));
      if (S.maskTarget && L && L.mask) {
        stroke.target = 'mask';
        stroke.maskIndex = L.maskIndex;
        stroke.texIndex = stroke.tool === 'stamp' ? -1 : 0;
        stroke.brush.mode = 'paint';
        if (stroke.tool === 'eraser') stroke.brush.color = S.background.slice();
      } else {
        stroke.target = 'layer';
        stroke.texIndex = texIndexOf(stroke.layer);
        stroke.brush.mode = stroke.tool === 'eraser' ? 'erase' : 'paint';
        if (stroke.tool === 'stamp') {
          if (!S.cloneSource) stroke.texIndex = -1;
          else stroke.clone = cloneOffsetFor(stroke.samples[0]);
        }
      }
    }
    if (stroke.texIndex < 0) return;
    stroke.previewed = strokeChain = strokeChain.then(function () {
      ensureStrokeBuffer();
      if (preview.layer < 0) beginPreview(stroke);
      rasterizeStroke(stroke);
      requestDraw();
    });
  };

  S.onStroke = function (stroke) {
    S.lastStroke = stroke;
    if (!(stroke.texIndex >= 0)) {
      status(stroke.tool === 'stamp' ? 'the Clone Stamp paints a raster layer\'s pixels, from an Alt-clicked source'
                                     : 'pick a raster layer, or a mask, to paint on');
      return;
    }
    strokeChain = strokeChain.then(function () { return commitStroke(stroke); });
  };

  // Resolves once every stroke and undo queued so far has been committed.
  S.strokesSettled = function () { return strokeChain.then(function () {}); };

  S.undo = function () {
    strokeChain = strokeChain.then(function () {
      return postJSON('undo').then(function (res) {
        if (res.empty) { status('nothing to undo'); return null; }
        return tookRect(res).then(function () { status(); });
      }).catch(function (e) { status('undo failed: ' + e.message); });
    });
    return strokeChain;
  };

  S.redo = function () {
    strokeChain = strokeChain.then(function () {
      return postJSON('redo').then(function (res) {
        if (res.empty) { status('nothing to redo'); return null; }
        return tookRect(res).then(function () { status(); });
      }).catch(function (e) { status('redo failed: ' + e.message); });
    });
    return strokeChain;
  };

  S.setBrush = function (changes) {
    Object.keys(changes).forEach(function (k) { S.brush[k] = changes[k]; });
    if (S.brushInputs) S.brushInputs.refresh();
  };

  // The browser's coverage for STROKE against the server's (POST
  // stroke?dry=1): painted here into a buffer the size of the server's
  // rectangle, through the same two functions, it must be equal to the
  // last bit, or the dual functions have drifted apart.
  S.strokeCheck = function (stroke) {
    stroke = stroke || S.lastStroke;
    return postJSON('stroke?dry=1', strokeBody(stroke)).then(function (res) {
      if (res.empty) return { empty: true };
      var dabs = strokeDabs(stroke), B = stroke.brush, count = dabs.length / 3;
      var buf = new Float64Array(res.w * res.h);
      for (var k = 0; k < count; k++) {
        paintDab(buf, res.w, res.h, res.x, res.y, dabs[3 * k], dabs[3 * k + 1],
                 B.radius * dabs[3 * k + 2], B.hardness, B.flow);
      }
      clipCoverage(buf, res.w, res.h, res.x, res.y);
      var max = 0, unequal = 0;
      for (var i = 0; i < buf.length; i++) {
        var d = Math.abs(buf[i] - res.coverage[i]);
        if (d > max) max = d;
        if (d !== 0) unequal++;
      }
      return { rect: [res.x, res.y, res.w, res.h], entries: buf.length, dabs: count,
               maxDiff: max, unequal: unequal };
    });
  };

  // ---- edits --------------------------------------------------------------

  var postTimers = {};

  function post(body) {
    return fetchJSON(api('set'), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body)
    }).then(function (res) {
      if (res.planVersion !== doc.planVersion) return reloadStructure();
    }).catch(function (e) { status('edit failed: ' + e.message); });
  }

  function postSoon(key, body) {
    clearTimeout(postTimers[key]);
    postTimers[key] = setTimeout(function () { post(body); }, 150);
  }

  // A plan change brings a new shader; the pixels stay where they are
  // unless the raster or mask count moved.  A change of the stack
  // itself (REBUILD_PANEL) also remakes the layer panel, whose rows
  // would otherwise hold the old layers.
  function reloadStructure(rebuildPanel) {
    return fetchJSON(api('doc')).then(function (d) {
      var repixel = d.rasterCount !== doc.rasterCount || d.maskCount !== doc.maskCount;
      doc = d;
      buildProgram(doc.shader);
      syncOpacity();
      uploadLuts();
      if (rebuildPanel) buildPanel();
      return repixel ? uploadPixels() : null;
    }).then(function () { status(); requestDraw(); });
  }

  // A change of the stack: {op: 'add', kind, above}, {op: 'delete',
  // layer}, {op: 'move', layer, delta}, {op: 'duplicate', layer}.  The
  // active layer stands in for a missing layer or above.
  S.layerOp = function (body) {
    if (body.op === 'add') { if (body.above === undefined) body.above = S.activeLayer; }
    else if (body.layer === undefined) body.layer = S.activeLayer;
    return S.strokesSettled().then(function () {
      return postJSON('layers', JSON.stringify(body));
    }).then(function (res) {
      S.activeLayer = res.active;
      S.undoDepth = 0;
      if (body.op === 'mask') S.maskTarget = body.value !== 'delete';
      return reloadStructure(true);
    }).catch(function (e) { status('layer edit failed: ' + e.message); });
  };

  // ---- the layer panel ----------------------------------------------------

  function el(tag, attrs, kids) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      if (k === 'text') e.textContent = attrs[k]; else e.setAttribute(k, attrs[k]);
    });
    (kids || []).forEach(function (k) { e.appendChild(k); });
    return e;
  }

  // The Layers panel as other editors lay it out: the stack top first,
  // each row an eye (visibility), a thumbnail of the layer (and of its
  // mask), its name and kind, the active row highlighted; below the
  // list, the active layer's own controls.
  var propsEl = null, thumbCanvases = {}, maskCanvases = {};

  var EYE_SVG = '<svg viewBox="0 0 20 20"><path d="M2 10s3-5 8-5 8 5 8 5-3 5-8 5-8-5-8-5z"/><circle cx="10" cy="10" r="2.5"/></svg>';
  var ADJ_ICONS = {
    curves: '<svg viewBox="0 0 20 20"><path d="M3 17C6 17 8 3 17 3M3 17L17 3" stroke-dasharray="1.5 2"/></svg>',
    levels: '<svg viewBox="0 0 20 20"><path d="M3 17V9M7 17V4M11 17V7M15 17V12M3 17h14"/></svg>',
    'hue-saturation': '<svg viewBox="0 0 20 20"><circle cx="10" cy="10" r="7"/><path d="M10 3v14M3 10h14M5 5l10 10M15 5L5 15"/></svg>'
  };

  function buildPanel() {
    layersEl.textContent = '';
    thumbCanvases = {};
    maskCanvases = {};
    if (S.activeLayer !== null && S.activeLayer >= doc.layers.length) S.activeLayer = null;
    if (S.activeLayer === null) {
      for (var t = doc.layers.length - 1; t >= 0 && S.activeLayer === null; t--) {
        if (doc.layers[t].kind === 'raster') S.activeLayer = doc.layers[t].index;
      }
    }
    layersEl.appendChild(el('div', { 'class': 'st-layers-head' }, [
      el('span', { text: 'Layers' }),
      el('span', { 'class': 'st-lkind', text: doc.layers.length + ' layers' })]));
    var list = el('div', { 'class': 'st-llist' });
    for (var n = doc.layers.length - 1; n >= 0; n--) list.appendChild(layerListRow(doc.layers[n]));
    layersEl.appendChild(list);
    propsEl = el('div', { 'class': 'st-props' });
    layersEl.appendChild(propsEl);
    showProperties();
    refreshThumbs();
  }

  function kindLabel(L) {
    return { raster: 'raster', curves: 'curves', levels: 'levels', 'hue-saturation': 'hue/sat' }[L.kind] +
      (L.mask ? ' + mask' : '');
  }

  function layerListRow(L) {
    var row = el('div', { 'class': 'st-lrow' + (L.index === S.activeLayer ? ' st-active' : ''),
                          'data-layer': String(L.index) });
    var eye = el('span', { 'class': 'st-eye' + (L.visible ? '' : ' st-off'), title: 'Visibility' });
    eye.innerHTML = EYE_SVG;
    eye.addEventListener('click', function (e) {
      e.stopPropagation();
      L.visible = !L.visible;
      eye.classList.toggle('st-off', !L.visible);
      syncOpacity();
      requestDraw();
      post({ layer: L.index, slot: 'visible', value: L.visible });
    });
    row.appendChild(eye);
    var onLayer = L.index === S.activeLayer && !(S.maskTarget && L.mask);
    var thumb = el('div', { 'class': 'st-thumb' + (onLayer ? ' st-target' : ''), title: 'Layer' });
    if (L.kind === 'raster') {
      var c = el('canvas', { width: '40', height: '24' });
      thumb.appendChild(c);
      thumbCanvases[L.texIndex] = c;
    } else {
      thumb.classList.add('st-thumb-adj');
      thumb.innerHTML = ADJ_ICONS[L.kind] || '';
    }
    thumb.addEventListener('click', function (e) { e.stopPropagation(); S.setActiveLayer(L.index, false); });
    row.appendChild(thumb);
    if (L.mask) {
      var m = el('canvas', { width: '40', height: '24' });
      var mt = el('div', { 'class': 'st-thumb' + (L.index === S.activeLayer && S.maskTarget ? ' st-target' : ''),
                           title: 'Layer mask' }, [m]);
      mt.addEventListener('click', function (e) { e.stopPropagation(); S.setActiveLayer(L.index, true); });
      row.appendChild(mt);
      maskCanvases[L.maskIndex] = m;
    }
    row.appendChild(el('span', { 'class': 'st-lname', text: L.name }));
    row.appendChild(el('span', { 'class': 'st-lkind', text: kindLabel(L) }));
    row.addEventListener('click', function () { S.setActiveLayer(L.index); });
    return row;
  }

  function drawThumb(c, bytes, channels) {
    var ctx = c.getContext('2d'), img = ctx.createImageData(c.width, c.height);
    if (channels === 4) img.data.set(bytes);
    else for (var i = 0; i < bytes.length; i++) {
      img.data[4 * i] = img.data[4 * i + 1] = img.data[4 * i + 2] = bytes[i];
      img.data[4 * i + 3] = 255;
    }
    ctx.putImageData(img, 0, 0);
  }

  // A raster layer's thumbnail from the server (its pixels as they are
  // now), after a load, a stroke or an undo.
  S.refreshThumb = function (texIndex) {
    var c = thumbCanvases[texIndex];
    if (!c) return Promise.resolve(null);
    return fetchBytes(api('thumb?i=' + texIndex + '&w=' + c.width + '&h=' + c.height))
      .then(function (b) { drawThumb(c, b, 4); return b.length; });
  };

  S.refreshMaskThumb = function (maskIndex) {
    var c = maskCanvases[maskIndex];
    if (!c) return Promise.resolve(null);
    return fetchBytes(api('thumb?mask=1&i=' + maskIndex + '&w=' + c.width + '&h=' + c.height))
      .then(function (b) { drawThumb(c, b, 1); return b.length; });
  };

  function refreshThumbs() {
    Object.keys(thumbCanvases).forEach(function (k) { S.refreshThumb(Number(k)); });
    Object.keys(maskCanvases).forEach(function (k) { S.refreshMaskThumb(Number(k)); });
  }

  // The active layer: what the brush paints and what the controls
  // below the list show.  Clicking a row picks it; clicking its mask
  // thumbnail makes the mask the target (MASK true), the layer
  // thumbnail the pixels again.
  S.setActiveLayer = function (i, mask) {
    S.activeLayer = i;
    if (mask !== undefined) S.maskTarget = !!mask;
    document.querySelectorAll('#stratum-layers .st-lrow').forEach(function (row) {
      var on = Number(row.getAttribute('data-layer')) === i;
      row.classList.toggle('st-active', on);
      var thumbs = row.querySelectorAll('.st-thumb');
      thumbs.forEach(function (t, k) {
        t.classList.toggle('st-target', on && (k === 1) === (S.maskTarget && thumbs.length > 1));
      });
    });
    showProperties();
  };

  function showProperties() {
    if (!propsEl) return;
    propsEl.textContent = '';
    var L = activeL();
    if (L) propsEl.appendChild(layerProperties(L));
  }

  // ---- the tool palette ---------------------------------------------------
  //
  // The tools down the left in the order other editors keep them, each
  // with its single-letter key.  The ones not built yet are drawn
  // dimmed and refuse to be picked; the foreground and background
  // swatches at the bottom (X swaps them, D resets them) hold the brush
  // color.

  var ICONS = {
    move: '<path d="M10 3v14M3 10h14M10 3l-2.5 2.5M10 3l2.5 2.5M10 17l-2.5-2.5M10 17l2.5-2.5M3 10l2.5-2.5M3 10l2.5 2.5M17 10l-2.5-2.5M17 10l-2.5 2.5"/>',
    marquee: '<rect x="4" y="4" width="12" height="12" stroke-dasharray="2.2 1.8"/>',
    lasso: '<path d="M10 3.5c4 0 6.5 1.8 6.5 4s-2.5 4-6.5 4-6.5-1.8-6.5-4 2.5-4 6.5-4zM7 11.5c-1.5 2-1 4.5-2 6"/>',
    wand: '<path d="M3.5 16.5l7.5-7.5M12 7l.7-3M12 7l3-.7M12 7l2.5-2.5M12 7l-2.5-2.5M12 7l2.3 2.3"/>',
    crop: '<path d="M6 2.5v11.5h11.5M2.5 6h11.5v11.5"/>',
    eyedropper: '<path d="M3.5 16.5l7-7M11.5 8.5l2-2a1.6 1.6 0 0 1 2.3 2.3l-2 2M10 7l3 3"/>',
    healing: '<rect x="3" y="7" width="14" height="6" rx="3" transform="rotate(-45 10 10)"/><path d="M8.5 10h3M10 8.5v3"/>',
    brush: '<path d="M3 17c0-3 1.2-4.5 3.5-5l8-8 2.5 2.5-8 8c-.5 2.3-2 3.5-6 2.5z"/>',
    stamp: '<path d="M7.5 3h5v5h-5zM4 8h12v3H4zM5 11v6h10v-6"/>',
    history: '<path d="M4.5 10a5.5 5.5 0 1 1 1.8 4M4.5 10l-1.5-1.5M4.5 10L6 8.5M10 7v3.5l2 1.5"/>',
    eraser: '<rect x="3.5" y="8.5" width="13" height="6" rx="1" transform="rotate(-45 10 10)"/><path d="M8 17h9"/>',
    gradient: '<rect x="3" y="4" width="14" height="12"/><path d="M7 4v12M11 4v12M14.5 4v12" stroke-dasharray="1.2 1.2"/>',
    blur: '<path d="M10 3c3 4 5 6.5 5 9.5a5 5 0 0 1-10 0C5 9.5 7 7 10 3z"/>',
    dodge: '<circle cx="12.5" cy="7.5" r="4.5"/><path d="M9.5 10.5l-6 6"/>',
    pen: '<path d="M4 16l8.5-8.5 2 2L6 18zM12.5 7.5l2-2 2 2-2 2M3 17.5l1-1.5"/>',
    type: '<path d="M4 4.5h12M10 4.5v11.5M7 16h6"/>',
    pathsel: '<path d="M5.5 3l10 8-5 1-2.5 5z"/>',
    shape: '<rect x="4" y="4" width="12" height="12" rx="2"/>',
    pan: '<path d="M7 11V6.5a1 1 0 0 1 2 0V10M9 10V4.5a1 1 0 0 1 2 0V10M11 10V5.5a1 1 0 0 1 2 0V10M13 10V7.5a1 1 0 0 1 2 0v5.5c0 3-2 4.5-5 4.5s-4-2-5-3.5l-2-3a1 1 0 0 1 1.7-1.2L7 11.5"/>',
    zoom: '<circle cx="11.5" cy="8.5" r="5"/><path d="M8 12l-4.5 4.5M9.5 8.5h4M11.5 6.5v4"/>'
  };

  var TOOLS = [
    { id: 'move', key: 'V', label: 'Move Tool', built: true },
    { id: 'marquee', key: 'M', label: 'Rectangular Marquee Tool', built: true },
    { id: 'lasso', key: 'L', label: 'Lasso Tool', built: true },
    { id: 'wand', key: 'W', label: 'Magic Wand Tool', built: true },
    '-',
    { id: 'crop', key: 'C', label: 'Crop Tool', built: true },
    { id: 'eyedropper', key: 'I', label: 'Eyedropper Tool', built: true },
    '-',
    { id: 'healing', key: 'J', label: 'Healing Brush Tool' },
    { id: 'brush', key: 'B', label: 'Brush Tool', built: true },
    { id: 'stamp', key: 'S', label: 'Clone Stamp Tool', built: true },
    { id: 'history', key: 'Y', label: 'History Brush Tool' },
    { id: 'eraser', key: 'E', label: 'Eraser Tool', built: true },
    { id: 'gradient', key: 'G', label: 'Gradient Tool', built: true },
    { id: 'blur', key: 'R', label: 'Blur Tool' },
    { id: 'dodge', key: 'O', label: 'Dodge Tool' },
    '-',
    { id: 'pen', key: 'P', label: 'Pen Tool' },
    { id: 'type', key: 'T', label: 'Horizontal Type Tool' },
    { id: 'pathsel', key: 'A', label: 'Path Selection Tool' },
    { id: 'shape', key: 'U', label: 'Rectangle Tool' },
    '-',
    { id: 'pan', key: 'H', label: 'Hand Tool', built: true },
    { id: 'zoom', key: 'Z', label: 'Zoom Tool', built: true }
  ];

  function toolSpec(id) {
    return TOOLS.filter(function (t) { return t !== '-' && t.id === id; })[0] || null;
  }

  S.background = [255, 255, 255];

  S.swapColors = function () {
    var fg = S.brush.color;
    S.setBrush({ color: S.background });
    S.background = fg;
    if (S.swatches) S.swatches.refresh();
  };

  S.defaultColors = function () {
    S.background = [255, 255, 255];
    S.setBrush({ color: [0, 0, 0] });
  };

  function installTools() {
    var bar = document.getElementById('stratum-tools');
    if (!bar) return;
    bar.textContent = '';
    TOOLS.forEach(function (t) {
      if (t === '-') { bar.appendChild(el('div', { 'class': 'st-tool-gap' })); return; }
      var b = el('div', { 'class': 'st-tool-btn' + (t.built ? '' : ' st-disabled'), 'data-tool': t.id,
                          title: t.label + ' (' + t.key + ')' });
      b.innerHTML = '<svg viewBox="0 0 20 20">' + ICONS[t.id] + '</svg>';
      b.addEventListener('click', function () { S.setTool(t.id); });
      bar.appendChild(b);
    });
    var fg = el('div', { 'class': 'st-swatch st-fg', title: 'Foreground color (X swaps, D resets)' });
    var bg = el('div', { 'class': 'st-swatch st-bg', title: 'Background color' });
    var bgInput = el('input', { type: 'color' });
    bgInput.style.display = 'none';
    bgInput.addEventListener('input', function () {
      S.background = [1, 3, 5].map(function (i) { return parseInt(bgInput.value.substr(i, 2), 16); });
      S.swatches.refresh();
    });
    fg.addEventListener('click', function () {
      var input = document.querySelector('#stratum-panel .st-brush input[type=color]');
      if (input) input.click();
    });
    bg.addEventListener('click', function () { bgInput.value = hexColor(S.background); bgInput.click(); });
    bar.appendChild(el('div', { 'class': 'st-swatches' }, [fg, bg, bgInput]));
    S.swatches = {
      refresh: function () {
        fg.style.background = hexColor(S.brush.color);
        bg.style.background = hexColor(S.background);
      }
    };
    S.swatches.refresh();
    S.setTool(S.tool);
  }

  // The brush box in the panel: size, hardness, flow, opacity, color,
  // and Undo (Ctrl+Z).
  var BRUSH_RANGES = { radius: ['size', 1, 300, 1], hardness: ['hardness', 0, 1, 0.01],
                       flow: ['flow', 0.01, 1, 0.01], opacity: ['opacity', 0, 1, 0.01] };

  function installToolbar() {
    var box = el('div', { 'class': 'st-brush' }, [el('div', { 'class': 'st-layers-head' }, [el('span', { text: 'Brush Settings' })])]);
    var inputs = {};
    Object.keys(BRUSH_RANGES).forEach(function (key) {
      var r = BRUSH_RANGES[key];
      var input = el('input', { type: 'range', min: String(r[1]), max: String(r[2]), step: String(r[3]),
                                value: String(S.brush[key]) });
      var val = el('span', { text: String(S.brush[key]) });
      input.addEventListener('input', function () {
        S.brush[key] = Number(input.value);
        val.textContent = input.value;
      });
      inputs[key] = { input: input, val: val };
      box.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: r[0] }), input, val]));
    });
    var color = el('input', { type: 'color', value: hexColor(S.brush.color) });
    color.addEventListener('input', function () {
      S.brush.color = [1, 3, 5].map(function (i) { return parseInt(color.value.substr(i, 2), 16); });
      if (S.swatches) S.swatches.refresh();
    });
    var undo = el('button', { type: 'button', text: 'Undo (Ctrl+Z)' });
    undo.addEventListener('click', function () { S.undo(); });
    box.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'color' }), color, undo]));
    S.brushInputs = {
      refresh: function () {
        Object.keys(inputs).forEach(function (key) {
          inputs[key].input.value = String(S.brush[key]);
          inputs[key].val.textContent = String(S.brush[key]);
        });
        color.value = hexColor(S.brush.color);
        if (S.swatches) S.swatches.refresh();
      }
    };
    // The Magic Wand's tolerance and contiguity, under the brush.
    var wand = el('div', { 'class': 'st-brush' }, [el('div', { 'class': 'st-layers-head' }, [el('span', { text: 'Magic Wand' })])]);
    wand.appendChild(settingSlider(S.wand, 'tolerance', function () {}));
    var contiguous = el('input', { type: 'checkbox' });
    contiguous.checked = true;
    contiguous.addEventListener('change', function () { S.wand.contiguous = contiguous.checked; });
    wand.appendChild(el('div', { 'class': 'st-row' }, [contiguous, el('span', { text: 'Contiguous' })]));
    // The Gradient tool's preset, shape, opacity and direction, under it.
    var grad = el('div', { 'class': 'st-brush' }, [el('div', { 'class': 'st-layers-head' }, [el('span', { text: 'Gradient' })])]);
    var preset = el('select');
    Object.keys(GRADIENT_PRESETS).forEach(function (k) { preset.appendChild(el('option', { value: k, text: k })); });
    preset.addEventListener('change', function () { S.gradient.preset = preset.value; });
    var shape = el('select');
    GRADIENT_SHAPES.forEach(function (k) { shape.appendChild(el('option', { value: k, text: k })); });
    shape.addEventListener('change', function () { S.gradient.shape = shape.value; });
    grad.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'colors' }), preset]));
    grad.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'shape' }), shape]));
    grad.appendChild(settingSlider(S.gradient, 'opacity', function () {}));
    var reverse = el('input', { type: 'checkbox' });
    reverse.addEventListener('change', function () { S.gradient.reverse = reverse.checked; });
    grad.appendChild(el('div', { 'class': 'st-row' }, [reverse, el('span', { text: 'Reverse' })]));
    // The Clone Stamp's alignment, under that.
    var stamp = el('div', { 'class': 'st-brush' }, [el('div', { 'class': 'st-layers-head' }, [el('span', { text: 'Clone Stamp' })])]);
    var aligned = el('input', { type: 'checkbox' });
    aligned.checked = S.clone.aligned;
    aligned.addEventListener('change', function () { S.clone.aligned = aligned.checked; S.cloneOffset = null; });
    stamp.appendChild(el('div', { 'class': 'st-row' }, [aligned, el('span', { text: 'Aligned' })]));
    stamp.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'Alt-click sets the source' })]));
    statusEl.parentNode.insertBefore(box, statusEl.nextSibling);
    statusEl.parentNode.insertBefore(wand, box.nextSibling);
    statusEl.parentNode.insertBefore(grad, wand.nextSibling);
    statusEl.parentNode.insertBefore(stamp, grad.nextSibling);
  }

  function hexColor(c) {
    return '#' + c.map(function (v) { return ('0' + Math.round(v).toString(16)).slice(-2); }).join('');
  }

  // ---- the selection ------------------------------------------------------
  //
  // An 8-bit coverage over the document with its bounding rectangle,
  // made on the server (selection.lisp) from a marquee rectangle, a
  // lasso polygon, All or Inverse, and fetched here whole: the shader
  // draws the marching ants from it, and a stroke's coverage is
  // multiplied by it on the way to the stroke texture, as the server
  // multiplies its own, so painting under a selection stays bit-exact
  // between the two.  No selection paints everywhere.

  S.selection = null;
  var selectionMask = null, texSelection = null, antsPhase = 0;

  function selectionTexture(w, h) {
    if (texSelection) gl.deleteTexture(texSelection);
    texSelection = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texSelection);
    gl.texStorage2D(gl.TEXTURE_2D, 1, gl.R8, w, h);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  }

  // COVERAGE (W x H at X, Y of the document) times the selection, byte
  // over 255; nothing without one.  The twin of clip-coverage!.
  function clipCoverage(coverage, w, h, x, y) {
    if (!selectionMask) return coverage;
    for (var j = 0; j < h; j++) {
      var src = (y + j) * doc.width + x, dst = j * w;
      for (var i = 0; i < w; i++) coverage[dst + i] = coverage[dst + i] * (selectionMask[src + i] / 255);
    }
    return coverage;
  }

  function loadSelection(rect) {
    S.selection = rect || null;
    if (!rect) { selectionMask = null; requestDraw(); return Promise.resolve(null); }
    return fetchBytes(api('selection')).then(function (bytes) {
      if (bytes.length !== doc.width * doc.height) throw new Error('selection: ' + bytes.length + ' bytes');
      selectionMask = bytes;
      if (!texSelection || texSelection.docWidth !== doc.width || texSelection.docHeight !== doc.height) {
        selectionTexture(doc.width, doc.height);
        texSelection.docWidth = doc.width;
        texSelection.docHeight = doc.height;
      }
      gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
      gl.bindTexture(gl.TEXTURE_2D, texSelection);
      gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, doc.width, doc.height, gl.RED, gl.UNSIGNED_BYTE, bytes);
      requestDraw();
      return rect;
    });
  }

  // {op: 'rect', x, y, w, h}, {op: 'polygon', points}, {op: 'all'},
  // {op: 'none'} or {op: 'inverse'}; resolves to the selection's
  // rectangle, or null.
  S.select = function (body) {
    return S.strokesSettled().then(function () {
      return postJSON('select', JSON.stringify(body));
    }).then(function (res) { return loadSelection(res.selection); })
      .then(function (rect) { status(); return rect; },
            function (e) { status('selection failed: ' + e.message); return null; });
  };

  // Edit > Fill and Clear: the selection (or the whole layer) filled
  // with a color at an opacity, or emptied.
  S.fill = function (body) {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer to fill'); return Promise.resolve(null); }
    body.layer = L.index;
    strokeChain = strokeChain.then(function () {
      return postJSON('fill', JSON.stringify(body)).then(tookRect).then(function () { status(); })
        .catch(function (e) { status('fill failed: ' + e.message); });
    });
    return strokeChain;
  };

  S.openFill = function () {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer to fill'); return null; }
    var choices = { Foreground: function () { return S.brush.color; }, Background: function () { return S.background; },
                    Black: function () { return [0, 0, 0]; }, White: function () { return [255, 255, 255]; },
                    '50% Gray': function () { return [128, 128, 128]; } };
    var sel = el('select');
    Object.keys(choices).forEach(function (k) { sel.appendChild(el('option', { value: k, text: k })); });
    var settings = { opacity: 1 };
    var box = el('div', { 'class': 'st-dialog' }, [
      el('h2', { text: 'Fill: ' + L.name }),
      el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'contents' }), sel]),
      settingSlider(settings, 'opacity', function () {})]);
    var cancel = el('button', { type: 'button', text: 'Cancel' });
    var ok = el('button', { type: 'button', text: 'OK' });
    box.appendChild(el('div', { 'class': 'st-row st-dialog-buttons' }, [cancel, ok]));
    document.getElementById('stratum-app').appendChild(box);
    var dialog = S.fillDialog = {
      cancel: function () { box.remove(); S.fillDialog = null; },
      ok: function () {
        box.remove();
        S.fillDialog = null;
        return S.fill({ color: choices[sel.value](), opacity: settings.opacity, mode: 'paint' });
      }
    };
    cancel.addEventListener('click', dialog.cancel);
    ok.addEventListener('click', dialog.ok);
    return dialog;
  };

  S.clear = function () { return S.fill({ color: [0, 0, 0], opacity: 1, mode: 'erase' }); };

  // Select > Feather: the selection's edge softened by a radius.
  S.openFeather = function () {
    if (!S.selection) { status('nothing is selected to feather'); return null; }
    if (S.featherDialog) S.featherDialog.cancel();
    var state = { radius: 5 };
    var box = el('div', { 'class': 'st-dialog' }, [el('h2', { text: 'Feather Selection' }),
                                                   settingSlider(state, 'radius', function () {})]);
    var cancel = el('button', { type: 'button', text: 'Cancel' });
    var ok = el('button', { type: 'button', text: 'OK' });
    box.appendChild(el('div', { 'class': 'st-row st-dialog-buttons' }, [cancel, ok]));
    document.getElementById('stratum-app').appendChild(box);
    var dialog = S.featherDialog = {
      state: state,
      set: function (changes) { Object.assign(state, changes); },
      cancel: function () { box.remove(); S.featherDialog = null; },
      ok: function () { box.remove(); S.featherDialog = null; return S.select({ op: 'feather', radius: state.radius }); }
    };
    cancel.addEventListener('click', dialog.cancel);
    ok.addEventListener('click', dialog.ok);
    return dialog;
  };

  // ---- the floating layer: Move, Cut, Copy, Paste -------------------------
  //
  // While the Move tool drags, the shader's floating stage shows the
  // active layer's selected pixels (all of them without a selection)
  // cut from where they were and composited at the offset, from the
  // textures already on the GPU, the ants moving along; on release the
  // server does the same arithmetic once (move-pixels!), the selection
  // moves with the pixels, and the layer comes back whole.  Copy and
  // Cut keep the selection's pixels on the server's clipboard; Paste
  // makes a new layer of them above the active one.

  // u_float_inv: the inverse of the forward affine, column-major for GLSL.
  var floating = { layer: -1, inv: new Float32Array([1, 0, 0, 0, 1, 0, 0, 0, 1]) };

  function setFloatTranslation(dx, dy) {
    floating.inv = new Float32Array([1, 0, 0, 0, 1, 0, -dx, -dy, 1]);
  }

  // The inverse of [a, b, c, d, e, f] (x' = ax + by + c, y' = dx + ey + f)
  // as the column-major mat3 the shader multiplies by.
  function inverseAffine(m) {
    var a = m[0], b = m[1], c = m[2], d = m[3], e = m[4], f = m[5];
    var det = a * e - b * d;
    if (Math.abs(det) < 1e-12) det = 1e-12;
    return new Float32Array([e / det, -d / det, 0, -b / det, a / det, 0,
                             (b * f - c * e) / det, (c * d - a * f) / det, 1]);
  }

  S.move = function (layer, dx, dy) {
    strokeChain = strokeChain.then(function () {
      return postJSON('move', JSON.stringify({ layer: layer, dx: dx, dy: dy })).then(function (res) {
        return tookRect(res).then(function () { return loadSelection(res.selection); });
      }).then(function () { floating.layer = -1; requestDraw(); status(); },
              function (e) { floating.layer = -1; requestDraw(); status('move failed: ' + e.message); });
    });
    return strokeChain;
  };

  S.clipboard = function (op) {
    var L = activeL();
    if (op !== 'paste' && (!L || L.kind !== 'raster')) { status('pick a raster layer to ' + op); return Promise.resolve(null); }
    var body = { op: op, layer: L ? L.index : null, above: S.activeLayer };
    strokeChain = strokeChain.then(function () {
      return postJSON('clipboard', JSON.stringify(body)).then(function (res) {
        if (op === 'copy') { status('copied ' + res.clipboard.w + ' x ' + res.clipboard.h); return null; }
        if (op === 'cut') return tookRect(res).then(function () { status(); });
        S.activeLayer = res.active;
        S.undoDepth = 0;
        return reloadStructure(true);
      }).catch(function (e) { status(op + ' failed: ' + e.message); });
    });
    return strokeChain;
  };

  // ---- Image > Crop, Image Size, Canvas Size ------------------------------
  //
  // Each remakes the document at a new size on the server (resize.lisp)
  // and reloads the page, since every texture here is the document's size.

  S.resize = function (body) {
    return S.strokesSettled().then(function () {
      return postJSON('resize', JSON.stringify(body));
    }).then(function (res) {
      if (S.noReload) return res;
      location.reload();
      return res;
    }, function (e) { status('resize failed: ' + e.message); return null; });
  };

  function numberField(label, value, onChange) {
    var input = el('input', { type: 'number', min: '1', max: '16384', value: String(value) });
    input.style.width = '80px';
    input.addEventListener('input', function () { onChange(Number(input.value)); });
    return { row: el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: label }), input, el('span', { text: 'px' })]),
             input: input };
  }

  function sizeDialog(title, body) {
    var box = el('div', { 'class': 'st-dialog' }, [el('h2', { text: title })]);
    body.forEach(function (e) { box.appendChild(e); });
    var cancel = el('button', { type: 'button', text: 'Cancel' });
    var ok = el('button', { type: 'button', text: 'OK' });
    box.appendChild(el('div', { 'class': 'st-row st-dialog-buttons' }, [cancel, ok]));
    document.getElementById('stratum-app').appendChild(box);
    return { box: box, cancel: cancel, ok: ok };
  }

  // File > New: name, size and what the Background holds.
  S.openNew = function () {
    if (S.newDialog) S.newDialog.cancel();
    var state = { name: 'Untitled', width: 1920, height: 1080, background: 'white' };
    var name = el('input', { type: 'text', value: state.name });
    name.style.width = '140px';
    name.addEventListener('input', function () { state.name = name.value; });
    var w = numberField('width', state.width, function (v) { state.width = v; });
    var h = numberField('height', state.height, function (v) { state.height = v; });
    var bg = el('select');
    [['white', 'White'], ['background', 'Background Color'], ['transparent', 'Transparent']].forEach(function (pair) {
      bg.appendChild(el('option', { value: pair[0], text: pair[1] }));
    });
    bg.addEventListener('change', function () { state.background = bg.value; });
    var d = sizeDialog('New', [
      el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'name' }), name]),
      w.row, h.row,
      el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'contents' }), bg])]);
    var dialog = S.newDialog = {
      state: state,
      set: function (changes) { Object.assign(state, changes); name.value = state.name; w.input.value = String(state.width); h.input.value = String(state.height); bg.value = state.background; },
      cancel: function () { d.box.remove(); S.newDialog = null; },
      ok: function () {
        d.box.remove();
        S.newDialog = null;
        var body = { name: state.name, width: state.width, height: state.height,
                     background: state.background === 'background' ? S.background : state.background };
        return postJSON('new', JSON.stringify(body)).then(function (res) {
          if (S.noReload) return res;
          location.search = '?doc=' + encodeURIComponent(res.doc);
          return res;
        }, function (e) { status('new failed: ' + e.message); return null; });
      }
    };
    d.cancel.addEventListener('click', dialog.cancel);
    d.ok.addEventListener('click', dialog.ok);
    name.focus();
    return dialog;
  };

  // Image Size: width and height, proportions constrained by default.
  S.openImageSize = function () {
    if (S.sizeDialog) S.sizeDialog.cancel();
    var state = { width: doc.width, height: doc.height, constrain: true };
    var w = numberField('width', state.width, function (v) {
      state.width = v;
      if (state.constrain) { state.height = Math.max(1, Math.round(v * doc.height / doc.width)); h.input.value = String(state.height); }
    });
    var h = numberField('height', state.height, function (v) {
      state.height = v;
      if (state.constrain) { state.width = Math.max(1, Math.round(v * doc.width / doc.height)); w.input.value = String(state.width); }
    });
    var constrain = el('input', { type: 'checkbox' });
    constrain.checked = true;
    constrain.addEventListener('change', function () { state.constrain = constrain.checked; });
    var d = sizeDialog('Image Size', [w.row, h.row,
      el('div', { 'class': 'st-row' }, [constrain, el('span', { text: 'Constrain Proportions' })])]);
    var dialog = S.sizeDialog = {
      state: state,
      set: function (changes) { Object.assign(state, changes); w.input.value = String(state.width); h.input.value = String(state.height); },
      cancel: function () { d.box.remove(); S.sizeDialog = null; },
      ok: function () { d.box.remove(); S.sizeDialog = null; return S.resize({ op: 'image-size', width: state.width, height: state.height }); }
    };
    d.cancel.addEventListener('click', dialog.cancel);
    d.ok.addEventListener('click', dialog.ok);
    return dialog;
  };

  // Canvas Size: width, height and a nine-way anchor.
  S.openCanvasSize = function () {
    if (S.sizeDialog) S.sizeDialog.cancel();
    var state = { width: doc.width, height: doc.height, anchor: [0.5, 0.5] };
    var w = numberField('width', state.width, function (v) { state.width = v; });
    var h = numberField('height', state.height, function (v) { state.height = v; });
    var grid = el('div', { 'class': 'st-anchor' });
    var cells = [];
    [0, 0.5, 1].forEach(function (ay) {
      [0, 0.5, 1].forEach(function (ax) {
        var c = el('div', { 'class': 'st-anchor-cell' + (ax === 0.5 && ay === 0.5 ? ' st-on' : '') });
        c.addEventListener('click', function () {
          state.anchor = [ax, ay];
          cells.forEach(function (k) { k.classList.toggle('st-on', k === c); });
        });
        cells.push(c);
        grid.appendChild(c);
      });
    });
    var d = sizeDialog('Canvas Size', [w.row, h.row,
      el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'anchor' }), grid])]);
    var dialog = S.sizeDialog = {
      state: state,
      set: function (changes) { Object.assign(state, changes); w.input.value = String(state.width); h.input.value = String(state.height); },
      cancel: function () { d.box.remove(); S.sizeDialog = null; },
      ok: function () {
        d.box.remove();
        S.sizeDialog = null;
        return S.resize({ op: 'canvas-size', width: state.width, height: state.height, anchor: state.anchor });
      }
    };
    d.cancel.addEventListener('click', dialog.cancel);
    d.ok.addEventListener('click', dialog.ok);
    return dialog;
  };

  // ---- Free Transform -----------------------------------------------------
  //
  // Ctrl+T: a box with eight handles around the selection (or the
  // layer), drawn on the overlay.  Dragging inside moves it, a handle
  // scales it about its center (Shift keeps the proportions), outside
  // turns it; the floating stage previews the result through the
  // inverse matrix with bilinear sampling, and Enter or a double-click
  // sends the forward matrix to the server (transform-pixels!), which
  // resamples the same way; Escape drops it.

  S.transforming = null;

  function applyAffine(m, p) { return [m[0] * p[0] + m[1] * p[1] + m[2], m[3] * p[0] + m[4] * p[1] + m[5]]; }

  S.beginTransform = function () {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer to transform'); return null; }
    if (S.transforming) S.transforming.cancel();
    var box = S.selection ? { x: S.selection.x, y: S.selection.y, w: S.selection.w, h: S.selection.h }
                          : { x: 0, y: 0, w: doc.width, h: doc.height };
    var cx = box.x + box.w / 2, cy = box.y + box.h / 2;
    var params = { tx: 0, ty: 0, sx: 1, sy: 1, angle: 0 };
    var drag = null;
    function matrix() {
      var cs = Math.cos(params.angle), sn = Math.sin(params.angle);
      var a = params.sx * cs, b = -params.sy * sn, d = params.sx * sn, e = params.sy * cs;
      return [a, b, cx + params.tx - (a * cx + b * cy), d, e, cy + params.ty - (d * cx + e * cy)];
    }
    function corners() {
      var m = matrix();
      return [[box.x, box.y], [box.x + box.w, box.y], [box.x + box.w, box.y + box.h], [box.x, box.y + box.h]]
        .map(function (p) { return applyAffine(m, p); });
    }
    function handles() {
      var c = corners(), out = c.slice();
      for (var k = 0; k < 4; k++) {
        var n = c[(k + 1) % 4];
        out.push([(c[k][0] + n[0]) / 2, (c[k][1] + n[1]) / 2]);
      }
      return out;
    }
    function draw() {
      if (!overlay) return;
      var ctx = overlay.getContext('2d');
      ctx.clearRect(0, 0, overlay.width, overlay.height);
      var c = corners().map(toCanvas);
      ctx.lineWidth = 1;
      ctx.setLineDash([]);
      ctx.strokeStyle = '#000';
      ctx.beginPath();
      c.forEach(function (p, k) { if (k === 0) ctx.moveTo(p[0], p[1]); else ctx.lineTo(p[0], p[1]); });
      ctx.closePath();
      ctx.stroke();
      ctx.strokeStyle = '#fff';
      ctx.setLineDash([4, 4]);
      ctx.stroke();
      ctx.setLineDash([]);
      handles().map(toCanvas).forEach(function (p) {
        ctx.fillStyle = '#fff';
        ctx.fillRect(p[0] - 4, p[1] - 4, 8, 8);
        ctx.strokeStyle = '#000';
        ctx.strokeRect(p[0] - 4.5, p[1] - 4.5, 9, 9);
      });
    }
    function apply() {
      floating.layer = L.texIndex;
      floating.inv = inverseAffine(matrix());
      draw();
      requestDraw();
    }
    function local(p) {
      // p relative to the moved center, turned back by the angle
      var dx = p[0] - (cx + params.tx), dy = p[1] - (cy + params.ty);
      var cs = Math.cos(-params.angle), sn = Math.sin(-params.angle);
      return [dx * cs - dy * sn, dx * sn + dy * cs];
    }
    function finish() {
      S.transforming = null;
      floating.layer = -1;
      floating.inv = new Float32Array([1, 0, 0, 0, 1, 0, 0, 0, 1]);
      drawGesture(null);
      requestDraw();
    }
    var t = S.transforming = {
      layer: L.index, params: params, box: box, matrix: matrix,
      set: function (changes) { Object.assign(params, changes); apply(); },
      down: function (e) {
        var p = docPoint(e), pc = toCanvas(p), hs = handles().map(toCanvas), hit = -1;
        for (var k = 0; k < hs.length; k++) {
          if (Math.abs(hs[k][0] - pc[0]) <= 10 && Math.abs(hs[k][1] - pc[1]) <= 10) { hit = k; break; }
        }
        var l = local(p), inside = Math.abs(l[0]) <= Math.abs(params.sx) * box.w / 2 && Math.abs(l[1]) <= Math.abs(params.sy) * box.h / 2;
        drag = { mode: hit >= 0 ? 'scale' : (inside ? 'move' : 'rotate'), handle: hit, start: p,
                 from: Object.assign({}, params), shift: e.shiftKey,
                 angle0: Math.atan2(p[1] - (cy + params.ty), p[0] - (cx + params.tx)) };
      },
      move: function (e) {
        if (!drag) return;
        var p = docPoint(e);
        if (drag.mode === 'move') {
          params.tx = drag.from.tx + (p[0] - drag.start[0]);
          params.ty = drag.from.ty + (p[1] - drag.start[1]);
        } else if (drag.mode === 'rotate') {
          params.angle = drag.from.angle + Math.atan2(p[1] - (cy + params.ty), p[0] - (cx + params.tx)) - drag.angle0;
        } else {
          var l = local(p), k = drag.handle;
          var signX = [-1, 1, 1, -1, 0, 1, 0, -1][k], signY = [-1, -1, 1, 1, -1, 0, 1, 0][k];
          var sx = signX ? l[0] / (signX * box.w / 2) : params.sx;
          var sy = signY ? l[1] / (signY * box.h / 2) : params.sy;
          if (e.shiftKey || drag.shift) { var u = (Math.abs(sx) + Math.abs(sy)) / 2; sx = u * (sx < 0 ? -1 : 1); sy = u * (sy < 0 ? -1 : 1); }
          params.sx = Math.abs(sx) < 0.01 ? 0.01 : sx;
          params.sy = Math.abs(sy) < 0.01 ? 0.01 : sy;
        }
        apply();
      },
      up: function () { drag = null; },
      commit: function () {
        var m = matrix();
        finish();
        strokeChain = strokeChain.then(function () {
          return postJSON('transform', JSON.stringify({ layer: L.index, matrix: m })).then(function (res) {
            return tookRect(res).then(function () { return loadSelection(res.selection); });
          }).then(function () { status(); }, function (e) { status('transform failed: ' + e.message); });
        });
        return strokeChain;
      },
      cancel: finish
    };
    apply();
    status('Free Transform: drag to move, handles to scale, outside to turn; Enter commits, Escape cancels');
    return t;
  };

  // The ants crawl while something is selected.
  setInterval(function () {
    if (selectionMask) { antsPhase = (antsPhase + 1) % 8; requestDraw(); }
  }, 120);

  // ---- files --------------------------------------------------------------
  //
  // The browser decodes an image file, in every format it knows, and
  // exactly: an ImageBitmap with no premultiplication and no color
  // conversion, through a texture and readPixels, so the bytes the
  // server gets are the file's.  Open makes a new document of them;
  // Place adds them as a layer of this one; Save As is the flattened
  // PNG the server writes.

  S.decodeImage = function (file) {
    return createImageBitmap(file, { premultiplyAlpha: 'none', colorSpaceConversion: 'none' })
      .then(function (bmp) {
        var w = bmp.width, h = bmp.height;
        var tex = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, tex);
        gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, false);
        gl.pixelStorei(gl.UNPACK_COLORSPACE_CONVERSION_WEBGL, gl.NONE);
        gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, w, h, 0, gl.RGBA, gl.UNSIGNED_BYTE, bmp);
        var fbo = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0);
        var bytes = new Uint8Array(w * h * 4);
        gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        gl.deleteFramebuffer(fbo);
        gl.deleteTexture(tex);
        bmp.close();
        return { width: w, height: h, bytes: bytes };
      });
  };

  function postBytes(path, bytes) {
    return fetchJSON(api(path), { method: 'POST', headers: { 'Content-Type': 'application/octet-stream' }, body: bytes });
  }

  function baseName(file) { return (file.name || 'untitled').replace(/\.[^.]*$/, ''); }

  S.openImage = function (img, name) {
    return postBytes('open?name=' + encodeURIComponent(name) + '&w=' + img.width + '&h=' + img.height, img.bytes);
  };

  // A .stratum file goes to the server whole (its own format); anything
  // else is an image the browser decodes.
  S.openFile = function (file) {
    status('opening ' + (file.name || 'image') + '...');
    var opened = /\.stratum$/i.test(file.name || '')
      ? file.arrayBuffer().then(function (buf) {
          return postBytes('load?name=' + encodeURIComponent(baseName(file)), new Uint8Array(buf));
        })
      : S.decodeImage(file).then(function (img) { return S.openImage(img, baseName(file)); });
    return opened.then(function (res) { location.search = '?doc=' + encodeURIComponent(res.doc); })
      .catch(function (e) { status('open failed: ' + e.message); });
  };

  // File > Save: the document as a .stratum file, downloaded.
  S.saveDocument = function () {
    var a = el('a', { href: api('save'), download: doc.name + '.stratum' });
    document.body.appendChild(a);
    a.click();
    a.remove();
  };

  S.placeImage = function (img, name) {
    return S.strokesSettled().then(function () {
      return postBytes('place?name=' + encodeURIComponent(name) + '&w=' + img.width + '&h=' + img.height +
                       (S.activeLayer !== null ? '&above=' + S.activeLayer : ''), img.bytes);
    }).then(function (res) {
      S.activeLayer = res.active;
      S.undoDepth = 0;
      return reloadStructure(true);
    });
  };

  S.placeFile = function (file) {
    status('placing ' + (file.name || 'image') + '...');
    return S.decodeImage(file).then(function (img) { return S.placeImage(img, baseName(file)); })
      .catch(function (e) { status('place failed: ' + e.message); });
  };

  S.saveAs = function () {
    var a = el('a', { href: api('export.png'), download: doc.name + '.png' });
    document.body.appendChild(a);
    a.click();
    a.remove();
  };

  S.closeDocument = function () {
    return postJSON('close').then(function () { location.search = '?doc=demo'; });
  };

  function chooseFile(then) {
    var input = el('input', { type: 'file', accept: 'image/*,.stratum' });
    input.style.display = 'none';
    input.addEventListener('change', function () {
      if (input.files[0]) then(input.files[0]);
      input.remove();
    });
    document.body.appendChild(input);
    input.click();
  }

  // ---- the menu bar -------------------------------------------------------
  //
  // The menus a classic image editor has, in their usual order with
  // their usual items and keys, so that someone who knows one finds
  // things where they expect them.  An item without a `run' is not
  // built yet and shows disabled, as unavailable commands do; the list
  // is rebuilt each time its menu opens, so checks and enabling are
  // current.

  function activeL() { return doc && S.activeLayer !== null ? doc.layers[S.activeLayer] : null; }
  function togglePanel(sel) {
    var e = document.querySelector(sel);
    e.style.display = e.style.display === 'none' ? '' : 'none';
    resize();
  }
  function panelShown(sel) { var e = document.querySelector(sel); return e && e.style.display !== 'none'; }
  function addLayer(kind) { return function () { S.layerOp({ op: 'add', kind: kind }); }; }

  function menuSpec() {
    return [
      { title: 'File', items: [
        { label: 'New...', key: 'Ctrl+N', run: function () { S.openNew(); } },
        { label: 'Open...', key: 'Ctrl+O', run: function () { chooseFile(S.openFile); } },
        { label: 'Open Sample', sub: ['demo', 'parity', 'stress'].map(function (n) {
          return { label: n, check: n === docName, run: function () { location.search = '?doc=' + n; } }; }) },
        '-',
        { label: 'Place...', run: function () { chooseFile(S.placeFile); } },
        '-',
        { label: 'Save', key: 'Ctrl+S', run: function () { S.saveDocument(); } },
        { label: 'Save As PNG...', key: 'Ctrl+Shift+S', run: S.saveAs },
        { label: 'Export', sub: [{ label: 'Export As PNG...', run: function () { window.open(api('export.png')); } }] },
        '-',
        { label: 'Close', run: S.closeDocument }] },
      { title: 'Edit', items: [
        { label: 'Undo', key: 'Ctrl+Z', run: function () { S.undo(); },
          enabled: function () { return S.undoDepth > 0; } },
        { label: 'Redo', key: 'Ctrl+Shift+Z', run: function () { S.redo(); },
          enabled: function () { return S.redoDepth > 0; } },
        '-',
        { label: 'Cut', key: 'Ctrl+X', enabled: rasterActive, run: function () { S.clipboard('cut'); } },
        { label: 'Copy', key: 'Ctrl+C', enabled: rasterActive, run: function () { S.clipboard('copy'); } },
        { label: 'Paste', key: 'Ctrl+V', run: function () { S.clipboard('paste'); } },
        '-',
        { label: 'Fill...', key: 'Shift+F5', enabled: rasterActive, run: function () { S.openFill(); } },
        { label: 'Clear', key: 'Delete', enabled: rasterActive, run: function () { S.clear(); } },
        '-',
        { label: 'Free Transform', key: 'Ctrl+T', enabled: rasterActive, run: function () { S.beginTransform(); } }] },
      { title: 'Image', items: [
        { label: 'Mode', sub: [{ label: 'RGB Color', check: true }, { label: 'Grayscale' }, { label: 'CMYK Color' }] },
        '-',
        { label: 'Adjustments', sub: [
          { label: 'Levels...', key: 'Ctrl+L', enabled: rasterActive, run: function () { S.openAdjust('levels'); } },
          { label: 'Curves...', key: 'Ctrl+M', enabled: rasterActive, run: function () { S.openAdjust('curves'); } },
          { label: 'Hue/Saturation...', key: 'Ctrl+U', enabled: rasterActive,
            run: function () { S.openAdjust('hue-saturation'); } }] },
        '-',
        { label: 'Image Size...', key: 'Ctrl+Alt+I', run: function () { S.openImageSize(); } },
        { label: 'Canvas Size...', key: 'Ctrl+Alt+C', run: function () { S.openCanvasSize(); } },
        '-',
        { label: 'Crop', enabled: function () { return !!S.selection; }, run: function () { S.resize({ op: 'crop' }); } }] },
      { title: 'Layer', items: [
        { label: 'New', sub: [{ label: 'Layer...', key: 'Ctrl+Shift+N', run: addLayer('raster') }] },
        { label: 'Duplicate Layer...', key: 'Ctrl+J', enabled: activeL,
          run: function () { S.layerOp({ op: 'duplicate' }); } },
        { label: 'Delete Layer', enabled: activeL, run: function () { S.layerOp({ op: 'delete' }); } },
        '-',
        { label: 'New Adjustment Layer', sub: [
          { label: 'Levels...', run: addLayer('levels') },
          { label: 'Curves...', run: addLayer('curves') },
          { label: 'Hue/Saturation...', run: addLayer('hue-saturation') }] },
        '-',
        { label: 'Layer Mask', sub: [
          { label: 'Reveal All', enabled: activeL, run: function () { S.layerOp({ op: 'mask', value: 'reveal' }); } },
          { label: 'Hide All', enabled: activeL, run: function () { S.layerOp({ op: 'mask', value: 'hide' }); } },
          '-',
          { label: 'Delete', enabled: function () { var L = activeL(); return L && L.mask; },
            run: function () { S.layerOp({ op: 'mask', value: 'delete' }); } }] },
        '-',
        { label: 'Arrange', sub: [
          { label: 'Bring Forward', key: 'Ctrl+]', enabled: activeL,
            run: function () { S.layerOp({ op: 'move', delta: 1 }); } },
          { label: 'Send Backward', key: 'Ctrl+[', enabled: activeL,
            run: function () { S.layerOp({ op: 'move', delta: -1 }); } }] },
        '-',
        { label: 'Merge Down', key: 'Ctrl+E', enabled: canMergeDown, run: function () { S.layerOp({ op: 'merge' }); } },
        { label: 'Flatten Image', enabled: function () { return doc && doc.layers.length > 0; },
          run: function () { S.layerOp({ op: 'flatten' }); } }] },
      { title: 'Select', items: [
        { label: 'All', key: 'Ctrl+A', run: function () { S.select({ op: 'all' }); } },
        { label: 'Deselect', key: 'Ctrl+D', enabled: function () { return !!S.selection; },
          run: function () { S.select({ op: 'none' }); } },
        { label: 'Inverse', key: 'Ctrl+Shift+I', run: function () { S.select({ op: 'inverse' }); } },
        '-',
        { label: 'Color Range...' },
        { label: 'Feather...', key: 'Shift+F6', enabled: function () { return !!S.selection; },
          run: function () { S.openFeather(); } }] },
      { title: 'Filter', items: [
        { label: 'Blur', sub: [{ label: 'Gaussian Blur...', enabled: rasterActive,
                                 run: function () { S.openFilter('gaussian-blur'); } }] },
        { label: 'Sharpen', sub: [{ label: 'Unsharp Mask...', enabled: rasterActive,
                                    run: function () { S.openFilter('unsharp-mask'); } }] },
        { label: 'Noise', sub: [{ label: 'Add Noise...', enabled: rasterActive,
                                  run: function () { S.openFilter('add-noise'); } }] }] },
      { title: 'View', items: [
        { label: 'Zoom In', key: 'Ctrl++', run: S.zoomIn },
        { label: 'Zoom Out', key: 'Ctrl+-', run: S.zoomOut },
        { label: 'Fit on Screen', key: 'Ctrl+0', run: S.fitOnScreen },
        { label: 'Actual Pixels', key: 'Ctrl+1', run: S.actualPixels },
        '-',
        { label: 'Benchmark', run: function () { document.querySelector('#stratum-panel .st-bench-btn').click(); } }] },
      { title: 'Window', items: [
        { label: 'Layers', check: panelShown('#stratum-panel'), run: function () { togglePanel('#stratum-panel'); } },
        { label: 'Brush Settings', check: panelShown('#stratum-panel .st-brush'),
          run: function () { togglePanel('#stratum-panel .st-brush'); } }] },
      { title: 'Help', items: [
        { label: 'About Stratum', run: function () {
          status('Stratum: a layered image editor in Common Lisp, on Gendl.  AGPL-3.0-or-later.'); } },
        { label: 'Source Code', run: function () { window.open('https://github.com/genworks/stratum'); } }] }
    ];
  }

  function closeMenus() {
    document.querySelectorAll('#stratum-menubar .st-menu.st-open').forEach(function (m) {
      m.classList.remove('st-open');
    });
  }

  function menuList(items) {
    var list = el('div', { 'class': 'st-menu-list' });
    items.forEach(function (it) {
      if (it === '-') { list.appendChild(el('div', { 'class': 'st-sep' })); return; }
      var on = it.sub ? true : !!(it.run && (!it.enabled || it.enabled()));
      var row = el('div', { 'class': 'st-item' + (on ? '' : ' st-disabled') + (it.sub ? ' st-sub' : '') });
      if (it.check) row.appendChild(el('span', { 'class': 'st-check', text: '✓' }));
      row.appendChild(el('span', { text: it.label }));
      row.appendChild(el('span', { 'class': 'st-key', text: it.sub ? '▸' : (it.key || '') }));
      if (it.sub) row.appendChild(menuList(it.sub));
      else if (on) row.addEventListener('click', function (e) { e.stopPropagation(); closeMenus(); it.run(); });
      else row.addEventListener('click', function (e) { e.stopPropagation(); });
      list.appendChild(row);
    });
    return list;
  }

  function installMenubar() {
    var bar = document.getElementById('stratum-menubar');
    if (!bar) return;
    bar.textContent = '';
    menuSpec().forEach(function (m, k) {
      var menu = el('div', { 'class': 'st-menu', 'data-menu': m.title },
                    [el('div', { 'class': 'st-menu-title', text: m.title })]);
      function open() {
        closeMenus();
        var old = menu.querySelector(':scope > .st-menu-list');
        if (old) menu.removeChild(old);
        menu.appendChild(menuList(menuSpec()[k].items));
        menu.classList.add('st-open');
      }
      menu.firstChild.addEventListener('click', function (e) {
        e.stopPropagation();
        if (menu.classList.contains('st-open')) closeMenus(); else open();
      });
      menu.firstChild.addEventListener('mouseenter', function () {
        if (document.querySelector('#stratum-menubar .st-menu.st-open')) open();
      });
      bar.appendChild(menu);
    });
    document.addEventListener('click', closeMenus);
  }

  // Open menu TITLE and run the item named LABEL (through a submenu
  // with SUB), as a click would: the test hook for the menus.
  S.menuRun = function (title, label, sub) {
    var menu = document.querySelector('#stratum-menubar .st-menu[data-menu="' + title + '"]');
    menu.firstChild.click();
    var scope = menu;
    if (sub) {
      scope = Array.prototype.find.call(menu.querySelectorAll('.st-item.st-sub'), function (r) {
        return r.children[0].textContent === sub || r.children[1].textContent === sub;
      });
    }
    var item = Array.prototype.find.call(scope.querySelectorAll('.st-item'), function (r) {
      return Array.prototype.some.call(r.children, function (c) { return c.textContent === label; });
    });
    var on = item && !item.classList.contains('st-disabled');
    if (on) item.click(); else closeMenus();
    return on;
  };

  // The active layer's controls: its name and kind, opacity, and then
  // its blend mode, its curves, or its settings.
  function layerProperties(L) {
    var i = L.index;
    var head = el('div', { 'class': 'st-head' }, [
      el('span', { 'class': 'st-name', text: L.name }),
      el('span', { 'class': 'st-kind', text: kindLabel(L) })]);
    var range = el('input', { type: 'range', min: '0', max: '100', value: String(Math.round(L.opacity * 100)) });
    var val = el('span', { text: Math.round(L.opacity * 100) + '%' });
    range.addEventListener('input', function () {
      L.opacity = range.value / 100;
      val.textContent = range.value + '%';
      syncOpacity();
      requestDraw();
      postSoon('opacity' + i, { layer: i, slot: 'opacity', value: L.opacity });
    });
    var row = el('div', { 'class': 'st-layer', 'data-layer': String(i) },
                 [head, el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'opacity' }), range, val])]);
    if (L.kind === 'raster') {
      var sel = el('select', { 'class': 'st-blend' });
      ['normal', 'multiply', 'screen', 'add'].forEach(function (m) {
        var o = el('option', { value: m, text: m });
        if (m === L.blend) o.selected = true;
        sel.appendChild(o);
      });
      sel.addEventListener('change', function () {
        L.blend = sel.value;
        post({ layer: i, slot: 'blend', value: sel.value });
      });
      row.appendChild(el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: 'blend' }), sel]));
    } else if (L.kind === 'curves') {
      row.appendChild(curveEditor(L.curves, function (channel, points) {
        uploadLuts();
        requestDraw();
        postSoon('curve' + L.index + channel, { layer: L.index, slot: 'curve', channel: channel, points: points });
      }));
    } else if (L.settings) {
      Object.keys(SETTING_RANGES).forEach(function (name) {
        if (name in L.settings) {
          row.appendChild(settingSlider(L.settings, name, function (n, v) {
            if (L.lutRow !== null && L.lutRow !== undefined) uploadLuts();
            syncOpacity();
            requestDraw();
            postSoon('setting' + L.index + n, { layer: L.index, slot: 'setting', name: n, value: v });
          }));
        }
      });
    }
    return row;
  }

  // Ranges for the settings sliders: [min, max, step].
  var SETTING_RANGES = {
    'in-black': [0, 1, 0.005], 'in-white': [0, 1, 0.005], gamma: [0.2, 4, 0.01],
    'out-black': [0, 1, 0.005], 'out-white': [0, 1, 0.005],
    hue: [-180, 180, 1], saturation: [-1, 1, 0.01], lightness: [-1, 1, 0.01],
    radius: [0.1, 100, 0.1], amount: [0, 500, 1], threshold: [0, 255, 1], tolerance: [0, 255, 1]
  };

  // One slider for a levels or hue/saturation setting.  A move changes
  // the tables or the params uniform and redraws; the server hears
  // when the slider rests.
  function settingSlider(settings, name, onChange) {
    var r = SETTING_RANGES[name] || [0, 1, 0.01];
    var input = el('input', { type: 'range', min: String(r[0]), max: String(r[1]), step: String(r[2]),
                              value: String(settings[name]) });
    var val = el('span', { text: String(Math.round(settings[name] * 1000) / 1000) });
    input.addEventListener('input', function () {
      var v = Number(input.value);
      settings[name] = v;
      val.textContent = String(v);
      onChange(name, v);
    });
    return el('div', { 'class': 'st-row' }, [el('span', { 'class': 'st-label', text: name }), input, val]);
  }

  // A small curves editor over CURVES ({master, red, green, blue}, each
  // a list of points, edited in place): drag a point; click empty space
  // to add one; double-click a point to remove it.  ONCHANGE(channel,
  // points) hears every move.
  function curveEditor(curves, onChange) {
    var size = 180, pad = 8;
    var wrap = el('div');
    var chan = el('select', { 'class': 'st-chan' });
    ['master', 'red', 'green', 'blue'].forEach(function (c) { chan.appendChild(el('option', { value: c, text: c })); });
    var cv = el('canvas', { 'class': 'st-curve', width: String(size), height: String(size) });
    var ctx = cv.getContext('2d');
    var active = -1;
    function pts() { return curves[chan.value]; }
    function toPx(p) { return [pad + p[0] * (size - 2 * pad), size - pad - p[1] * (size - 2 * pad)]; }
    function fromPx(x, y) {
      return [Math.min(1, Math.max(0, (x - pad) / (size - 2 * pad))),
              Math.min(1, Math.max(0, (size - pad - y) / (size - 2 * pad)))];
    }
    function paint() {
      ctx.clearRect(0, 0, size, size);
      ctx.strokeStyle = '#2c2f35';
      for (var g = 1; g < 4; g++) {
        var q = pad + g * (size - 2 * pad) / 4;
        ctx.beginPath(); ctx.moveTo(q, pad); ctx.lineTo(q, size - pad); ctx.stroke();
        ctx.beginPath(); ctx.moveTo(pad, q); ctx.lineTo(size - pad, q); ctx.stroke();
      }
      var p = pts(), xs = column(p, 0), ys = column(p, 1), ms = curveTangents(xs, ys);
      ctx.strokeStyle = { master: '#d8dbe0', red: '#e06c6c', green: '#6cc070', blue: '#6c9ce0' }[chan.value];
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      for (var k = 0; k <= 64; k++) {
        var x = k / 64, y = Math.min(1, Math.max(0, curveEval(xs, ys, ms, x)));
        var xy = toPx([x, y]);
        if (k === 0) ctx.moveTo(xy[0], xy[1]); else ctx.lineTo(xy[0], xy[1]);
      }
      ctx.stroke();
      ctx.lineWidth = 1;
      p.forEach(function (pt, k) {
        var xy = toPx(pt);
        ctx.fillStyle = k === active ? '#ffffff' : '#9aa0a8';
        ctx.fillRect(xy[0] - 3, xy[1] - 3, 6, 6);
      });
    }
    function changed() {
      paint();
      onChange(chan.value, pts().slice());
    }
    function nearest(x, y) {
      var best = -1, bd = 64;
      pts().forEach(function (pt, k) {
        var xy = toPx(pt), d = (xy[0] - x) * (xy[0] - x) + (xy[1] - y) * (xy[1] - y);
        if (d < bd) { bd = d; best = k; }
      });
      return best;
    }
    function local(e) { var r = cv.getBoundingClientRect(); return [e.clientX - r.left, e.clientY - r.top]; }
    cv.addEventListener('pointerdown', function (e) {
      var xy = local(e);
      active = nearest(xy[0], xy[1]);
      if (active < 0) {
        var np = fromPx(xy[0], xy[1]), p = pts(), k = 0;
        while (k < p.length && p[k][0] < np[0]) k++;
        if (k > 0 && k < p.length && np[0] - p[k - 1][0] > 0.01 && p[k][0] - np[0] > 0.01) {
          p.splice(k, 0, np);
          active = k;
          changed();
        }
      }
      if (active >= 0) cv.setPointerCapture(e.pointerId);
      paint();
    });
    cv.addEventListener('pointermove', function (e) {
      if (active < 0) return;
      var xy = local(e), np = fromPx(xy[0], xy[1]), p = pts();
      var lo = active > 0 ? p[active - 1][0] + 0.01 : 0;
      var hi = active < p.length - 1 ? p[active + 1][0] - 0.01 : 1;
      p[active] = [Math.min(hi, Math.max(lo, np[0])), np[1]];
      changed();
    });
    cv.addEventListener('pointerup', function () { active = -1; paint(); });
    cv.addEventListener('dblclick', function (e) {
      var xy = local(e), k = nearest(xy[0], xy[1]), p = pts();
      if (k > 0 && k < p.length - 1) { p.splice(k, 1); active = -1; changed(); }
    });
    chan.addEventListener('change', paint);
    wrap.appendChild(el('div', { 'class': 'st-row' }, [el('span', { text: 'curve' }), chan]));
    wrap.appendChild(cv);
    paint();
    return wrap;
  }

  // ---- Filters ------------------------------------------------------------
  //
  // A filter's preview is a texture the viewer computes itself and the
  // main shader swaps in for the layer's pixels (the replacement
  // stage, u_replace_layer), blended by the selection.  Gaussian Blur
  // runs the server's shader (doc.blurShader) in two passes into float
  // textures, rows then columns, the same kernel as
  // gaussian-blur-rgba; OK sends the radius and the server bakes it.
  // Float render targets need EXT_color_buffer_float; without it the
  // preview is off and OK still applies the filter.

  var replace = { layer: -1, texture: null }, texIdle = null;
  var blurProg = null, blurUni = {}, blurTex = [null, null], blurFbo = [null, null], floatRenderable = null;
  S.filterPreviewSupported = null;

  function ensureBlur() {
    if (floatRenderable === null) {
      floatRenderable = !!gl.getExtension('EXT_color_buffer_float');
      S.filterPreviewSupported = floatRenderable;
    }
    if (!floatRenderable) return false;
    if (!blurProg) {
      var p = gl.createProgram();
      var vs = compileShader(gl.VERTEX_SHADER, VERT), fs = compileShader(gl.FRAGMENT_SHADER, doc.filterShader);
      gl.attachShader(p, vs);
      gl.attachShader(p, fs);
      gl.linkProgram(p);
      gl.deleteShader(vs);
      gl.deleteShader(fs);
      if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error('filter link: ' + gl.getProgramInfoLog(p));
      blurProg = p;
      ['u_layers', 'u_src', 'u_layer', 'u_pass', 'u_size', 'u_sigma', 'u_extent',
       'u_amount', 'u_threshold', 'u_mono', 'u_seed',
       'u_g0', 'u_g1', 'u_c0', 'u_c1', 'u_shape', 'u_opacity'].forEach(function (n) {
        blurUni[n] = gl.getUniformLocation(p, n);
      });
    }
    if (!blurTex[0] || blurTex[0].w !== doc.width || blurTex[0].h !== doc.height) {
      for (var k = 0; k < 2; k++) {
        if (blurTex[k]) { gl.deleteTexture(blurTex[k]); gl.deleteFramebuffer(blurFbo[k]); }
        var t = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, t);
        gl.texStorage2D(gl.TEXTURE_2D, 1, gl.RGBA32F, doc.width, doc.height);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        t.w = doc.width;
        t.h = doc.height;
        var f = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, f);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, t, 0);
        blurTex[k] = t;
        blurFbo[k] = f;
      }
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    }
    return true;
  }

  // One filter pass of the filter program into target texture T,
  // reading layer TEXINDEX and SRC.
  function filterPass(pass, target, src, texIndex, s) {
    gl.bindFramebuffer(gl.FRAMEBUFFER, blurFbo[target]);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, texLayers);
    gl.uniform1i(blurUni.u_layers, 0);
    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, src);
    gl.uniform1i(blurUni.u_src, 1);
    gl.uniform1i(blurUni.u_layer, texIndex);
    gl.uniform1i(blurUni.u_pass, pass);
    gl.uniform2f(blurUni.u_size, doc.width, doc.height);
    gl.uniform1f(blurUni.u_sigma, s.sigma || 1);
    gl.uniform1i(blurUni.u_extent, Math.max(1, Math.ceil(3 * (s.sigma || 1))));
    gl.uniform1f(blurUni.u_amount, s.amount || 0);
    gl.uniform1f(blurUni.u_threshold, s.threshold || 0);
    gl.uniform1i(blurUni.u_mono, s.mono ? 1 : 0);
    gl.uniform1i(blurUni.u_seed, s.seed || 1);
    if (s.gradient) {
      var g = s.gradient;
      gl.uniform2f(blurUni.u_g0, g.x0, g.y0);
      gl.uniform2f(blurUni.u_g1, g.x1, g.y1);
      gl.uniform4f(blurUni.u_c0, g.start[0] / 255, g.start[1] / 255, g.start[2] / 255, g.start[3] / 255);
      gl.uniform4f(blurUni.u_c1, g.end[0] / 255, g.end[1] / 255, g.end[2] / 255, g.end[3] / 255);
      gl.uniform1i(blurUni.u_shape, Math.max(0, GRADIENT_SHAPES.indexOf(g.shape)));
      gl.uniform1f(blurUni.u_opacity, g.opacity);
    }
    gl.drawArrays(gl.TRIANGLES, 0, 3);
  }

  // The Gradient tool's preview: pass 4 lays the ramp over layer
  // TEXINDEX into the replacement texture, the twin of gradient-rgba;
  // the drag's commit (S.gradientFill) bakes the same ramp on the
  // server.
  function runGradient(texIndex, body) {
    if (!ensureBlur()) return false;
    gl.useProgram(blurProg);
    gl.bindVertexArray(vao);
    gl.viewport(0, 0, doc.width, doc.height);
    filterPass(4, 1, texIdle, texIndex, { gradient: body });
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    replace.texture = blurTex[1];
    replace.layer = texIndex;
    requestDraw();
    return true;
  }

  var gradientWarned = false;
  function previewGradient(g) {
    if (!runGradient(g.texIndex, gradientBody(g)) && !gradientWarned) {
      gradientWarned = true;
      status('no float render targets here: the gradient shows when the pointer lifts');
    }
  }

  S.gradientFill = function (body) {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer for a gradient'); return Promise.resolve(null); }
    body.layer = L.index;
    strokeChain = strokeChain.then(function () {
      return postJSON('gradient', JSON.stringify(body)).then(tookRect)
        .then(function () { clearReplace(); status(); },
              function (e) { clearReplace(); status('gradient failed: ' + e.message); });
    });
    return strokeChain;
  };

  // Filter layer TEXINDEX into the replacement texture: a blur is
  // passes 0 and 1; a sharpening adds pass 2 over the blur; noise is
  // pass 3 alone.
  function runFilter(kind, texIndex, state) {
    if (!ensureBlur()) return false;
    gl.useProgram(blurProg);
    gl.bindVertexArray(vao);
    gl.viewport(0, 0, doc.width, doc.height);
    var out;
    if (kind === 'add-noise') {
      filterPass(3, 1, texIdle, texIndex, { amount: state.amount / 100, mono: state.monochrome, seed: state.seed });
      out = 1;
    } else {
      var s = { sigma: Math.max(0.1, state.radius) };
      filterPass(0, 0, texIdle, texIndex, s);
      filterPass(1, 1, blurTex[0], texIndex, s);
      out = 1;
      if (kind === 'unsharp-mask') {
        filterPass(2, 0, blurTex[1], texIndex, { amount: state.amount / 100, threshold: state.threshold });
        out = 0;
      }
    }
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    replace.texture = blurTex[out];
    replace.layer = texIndex;
    requestDraw();
    return true;
  }

  function clearReplace() {
    replace.layer = -1;
    requestDraw();
  }

  var FILTERS = {
    'gaussian-blur': { title: 'Gaussian Blur', defaults: { radius: 4 } },
    'unsharp-mask': { title: 'Unsharp Mask', defaults: { amount: 100, radius: 2, threshold: 0 } },
    'add-noise': { title: 'Add Noise', defaults: { amount: 10, monochrome: false, seed: 1 } }
  };

  S.openFilter = function (kind) {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer to filter'); return null; }
    if (S.filterDialog) S.filterDialog.cancel();
    var spec = FILTERS[kind], state = Object.assign({}, spec.defaults);
    var controls = el('div');
    var box = el('div', { 'class': 'st-dialog' }, [el('h2', { text: spec.title + ': ' + L.name }), controls]);
    function refresh() {
      if (!runFilter(kind, L.texIndex, state)) {
        status('no float render targets here: the preview is off, OK still applies the filter');
      }
    }
    function buildControls() {
      controls.textContent = '';
      Object.keys(state).forEach(function (name) {
        if (name === 'seed') return;
        if (typeof state[name] === 'boolean') {
          var box2 = el('input', { type: 'checkbox' });
          box2.checked = state[name];
          box2.addEventListener('change', function () { state[name] = box2.checked; refresh(); });
          controls.appendChild(el('div', { 'class': 'st-row' }, [box2, el('span', { text: name })]));
        } else {
          controls.appendChild(settingSlider(state, name, refresh));
        }
      });
    }
    buildControls();
    var cancel = el('button', { type: 'button', text: 'Cancel' });
    var ok = el('button', { type: 'button', text: 'OK' });
    box.appendChild(el('div', { 'class': 'st-row st-dialog-buttons' }, [cancel, ok]));
    document.getElementById('stratum-app').appendChild(box);
    var dialog = S.filterDialog = {
      kind: kind, state: state, layer: L.index,
      set: function (changes) { Object.assign(state, changes); buildControls(); refresh(); },
      cancel: function () { box.remove(); S.filterDialog = null; clearReplace(); },
      ok: function () {
        box.remove();
        S.filterDialog = null;
        return S.strokesSettled().then(function () {
          return postJSON('filter', JSON.stringify({ layer: L.index, kind: kind, settings: state }));
        }).then(tookRect)
          .then(function () { clearReplace(); status(); },
                function (e) { clearReplace(); status('filter failed: ' + e.message); });
      }
    };
    cancel.addEventListener('click', dialog.cancel);
    ok.addEventListener('click', dialog.ok);
    refresh();
    return dialog;
  };

  // ---- Image > Adjustments ------------------------------------------------
  //
  // Levels, Curves and Hue/Saturation baked into the active raster
  // layer's pixels, each in a dialog with a live preview: while it is
  // open the shader applies the adjustment to that one layer alone (the
  // adjustment stage: three rows at the end of the table texture, or
  // hue/saturation parameters), and OK sends the settings to the
  // server, which bakes the same arithmetic into the bytes and answers
  // the whole layer to re-upload.  Cancel drops the preview.

  var adjust = { layer: -1, kind: 0, params: [0, 0, 0, 0], luts: null };
  var ADJUST_DEFAULTS = {
    levels: { 'in-black': 0, 'in-white': 1, gamma: 1, 'out-black': 0, 'out-white': 1 },
    'hue-saturation': { hue: 0, saturation: 0, lightness: 0 }
  };
  var ADJUST_TITLES = { levels: 'Levels', curves: 'Curves', 'hue-saturation': 'Hue/Saturation' };

  function identityCurves() {
    return { master: [[0, 0], [1, 1]], red: [[0, 0], [1, 1]], green: [[0, 0], [1, 1]], blue: [[0, 0], [1, 1]] };
  }

  function adjustLuts(kind, state) {
    if (kind === 'levels') {
      var s = state.settings;
      var t = levelsLut(s['in-black'], s['in-white'], s.gamma, s['out-black'], s['out-white'], 256);
      return [t, t, t];
    }
    var m = state.curves.master;
    return ['red', 'green', 'blue'].map(function (ch) {
      var c = state.curves[ch];
      return composedLut(column(m, 0), column(m, 1), column(c, 0), column(c, 1), 256);
    });
  }

  function previewAdjust(kind, state, L) {
    adjust.layer = L.texIndex;
    if (kind === 'hue-saturation') {
      adjust.kind = 2;
      adjust.params = [state.settings.hue / 360, state.settings.saturation, state.settings.lightness, 0];
    } else {
      adjust.kind = 1;
      adjust.luts = adjustLuts(kind, state);
      uploadLuts();
    }
    requestDraw();
  }

  function clearAdjust() {
    adjust.layer = -1;
    adjust.luts = null;
    requestDraw();
  }

  function rasterActive() { var L = activeL(); return !!(L && L.kind === 'raster'); }
  function canMergeDown() {
    var L = activeL();
    return !!(L && L.index > 0 && doc.layers[L.index - 1].kind === 'raster');
  }

  // Open the KIND dialog on the active raster layer; the dialog object
  // (set, ok, cancel) is also the test hook.
  S.openAdjust = function (kind) {
    var L = activeL();
    if (!L || L.kind !== 'raster') { status('pick a raster layer to adjust'); return null; }
    if (S.adjustDialog) S.adjustDialog.cancel();
    var state = { settings: Object.assign({}, ADJUST_DEFAULTS[kind] || {}), curves: identityCurves() };
    var controls = el('div');
    var box = el('div', { 'class': 'st-dialog' }, [el('h2', { text: ADJUST_TITLES[kind] + ': ' + L.name }), controls]);
    function refresh() { previewAdjust(kind, state, L); }
    function buildControls() {
      controls.textContent = '';
      if (kind === 'curves') controls.appendChild(curveEditor(state.curves, refresh));
      else Object.keys(state.settings).forEach(function (name) {
        controls.appendChild(settingSlider(state.settings, name, refresh));
      });
    }
    buildControls();
    var cancel = el('button', { type: 'button', text: 'Cancel' });
    var ok = el('button', { type: 'button', text: 'OK' });
    box.appendChild(el('div', { 'class': 'st-row st-dialog-buttons' }, [cancel, ok]));
    document.getElementById('stratum-app').appendChild(box);
    var dialog = S.adjustDialog = {
      kind: kind, state: state, layer: L.index,
      set: function (changes) {
        Object.assign(kind === 'curves' ? state.curves : state.settings, changes);
        buildControls();
        refresh();
      },
      cancel: function () { box.remove(); S.adjustDialog = null; clearAdjust(); },
      ok: function () {
        box.remove();
        S.adjustDialog = null;
        var body = { layer: L.index, kind: kind };
        if (kind === 'curves') body.curves = state.curves; else body.settings = state.settings;
        return S.strokesSettled().then(function () { return postJSON('adjust', JSON.stringify(body)); })
          .then(tookRect)
          .then(function () { clearAdjust(); status(); },
                function (e) { clearAdjust(); status('adjustment failed: ' + e.message); });
      }
    };
    cancel.addEventListener('click', dialog.cancel);
    ok.addEventListener('click', dialog.ok);
    refresh();
    return dialog;
  };

  // ---- test hooks ---------------------------------------------------------

  // The whole document rendered at 1:1 with texelFetch into an offscreen
  // target and read back (rows from the bottom, as readPixels gives
  // them), a live stroke preview included.
  S.renderExact = function () {
    var w = doc.width, h = doc.height;
    var target = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, target);
    gl.texStorage2D(gl.TEXTURE_2D, 1, gl.RGBA8, w, h);
    var fbo = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, target, 0);
    gl.viewport(0, 0, w, h);
    bindAndSet(h, { scale: 1, ox: 0, oy: 0 }, true);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    var gpu = new Uint8Array(w * h * 4);
    gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, gpu);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.deleteFramebuffer(fbo);
    gl.deleteTexture(target);
    return gpu;
  };

  // Two renders compared byte for byte.
  function renderDiff(a, b) {
    var max = 0, over = 0, changed = 0;
    for (var i = 0; i < a.length; i++) {
      var d = Math.abs(a[i] - b[i]);
      if (d > max) max = d;
      if (d > 1) over++;
      if (d > 0) changed++;
    }
    return { maxDiff: max, overOne: over, changed: changed };
  }

  // The exact render compared with the server's CPU render, pixel by pixel.
  S.parity = function () {
    var w = doc.width, h = doc.height;
    var gpu = S.renderExact();
    return fetchBytes(api('cpu')).then(function (cpu) {
      var max = 0, over = 0, sum = 0, worst = null;
      for (var y = 0; y < h; y++) {
        for (var x = 0; x < w; x++) {
          for (var c = 0; c < 4; c++) {
            var g = gpu[((h - 1 - y) * w + x) * 4 + c], k = cpu[(y * w + x) * 4 + c];
            var d = Math.abs(g - k);
            sum += d;
            if (d > 1) over++;
            if (d > max) { max = d; worst = { x: x, y: y, c: c, gpu: g, cpu: k }; }
          }
        }
      }
      return { width: w, height: h, maxDiff: max, overOne: over, meanDiff: sum / (w * h * 4), worst: worst };
    });
  };

  // Draw N frames back to back, each with a pan step and (with curves)
  // a curve point moved and the table re-uploaded.  A one-pixel
  // readPixels ends each frame, because Chrome's gl.finish() returns
  // before the GPU process has drawn.  The view and the curve are put
  // back afterwards, so a parity check after a bench still compares
  // like with like.  Headless SwiftShader numbers only prove the path;
  // real frame rates come from a real GPU.
  S.bench = function (n, withCurves) {
    n = n || 60;
    var point = null, saved = null, ox = view.ox;
    if (withCurves) {
      doc.layers.forEach(function (L) {
        if (!point && L.kind === 'curves' && L.curves.master.length > 2) point = L.curves.master[1];
      });
      if (point) saved = point[1];
    }
    var px = new Uint8Array(4);
    var t0 = performance.now(), worst = 0;
    for (var f = 0; f < n; f++) {
      var t = performance.now();
      view.ox += 3 / view.scale;
      if (point) {
        point[1] = 0.15 + 0.1 * Math.sin(f / 5);
        uploadLuts();
      }
      draw();
      gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, px);
      worst = Math.max(worst, performance.now() - t);
    }
    var total = performance.now() - t0;
    view.ox = ox;
    if (point) { point[1] = saved; uploadLuts(); }
    draw();
    return { frames: n, msPerFrame: total / n, worstMs: worst, fps: 1000 * n / total,
             curves: !!point, canvas: [canvas.width, canvas.height], scale: view.scale };
  };

  // The curve tables as the browser computes them (composedLut from
  // dual.js, in float64) against the server's (the same Lisp source),
  // row for row.
  S.lutCheck = function () {
    return fetchJSON(api('luts')).then(function (server) {
      var max = 0, count = 0;
      doc.layers.forEach(function (L) {
        if (L.lutRow === null || L.lutRow === undefined) return;
        layerLuts(L).forEach(function (lut, k) {
          for (var i = 0; i < 256; i++) {
            max = Math.max(max, Math.abs(lut[i] - server[(L.lutRow + k) * 256 + i]));
            count++;
          }
        });
      });
      return { entries: count, maxDiff: max };
    });
  };

  // The browser half of the regression suite (tests/browser.sh): GPU
  // against CPU, browser curve tables against the server's, and a
  // blend-mode round trip that must rebuild the program exactly once
  // each way and leave parity intact.  The document is left as found.
  S.selfTest = function () {
    var steps = [], ok = true;
    function check(name, pass, detail) {
      steps.push({ name: name, pass: !!pass, detail: detail });
      if (!pass) ok = false;
    }
    var L = null, original = null, builds = 0;
    return S.parity().then(function (p) {
      check('parity', p.maxDiff <= 1, p);
      return S.lutCheck();
    }).then(function (l) {
      check('curve tables', l.entries === 0 || l.maxDiff < 1e-9, l);
      doc.layers.forEach(function (x) { if (!L && x.kind === 'raster' && x.index > 0) L = x; });
      if (!L) return null;
      original = L.blend;
      builds = S.stats.programBuilds;
      var other = original === 'screen' ? 'multiply' : 'screen';
      L.blend = other;
      return post({ layer: L.index, slot: 'blend', value: other }).then(function () {
        check('blend edit rebuilds once', S.stats.programBuilds === builds + 1,
              { before: builds, after: S.stats.programBuilds });
        return S.parity();
      }).then(function (p) {
        check('parity after blend edit', p.maxDiff <= 1, p);
        L.blend = original;
        return post({ layer: L.index, slot: 'blend', value: original });
      }).then(function () {
        check('blend restored', S.stats.programBuilds === builds + 2,
              { before: builds, after: S.stats.programBuilds });
        return S.parity();
      }).then(function (p) {
        check('parity after restore', p.maxDiff <= 1, p);
      });
    }).then(function () {
      // A rectangle of the second raster layer back from the server into
      // the same place: a wrong offset or a transposed copy would show
      // as a parity failure.
      var tex = Math.min(1, doc.rasterCount - 1), x = 37, y = 23, w = 101, h = 59;
      return S.uploadRect(tex, x, y, w, h).then(function (n) {
        check('uploadRect brings w*h*4 bytes', n === w * h * 4, { bytes: n });
        return S.parity();
      }).then(function (p) {
        check('parity after uploadRect', p.maxDiff <= 1, p);
      });
    }).then(function () {
      // The stroke round trip (README "Open pieces", acceptance test 1).
      // A synthetic pen stroke through the brush tool is previewed here,
      // committed by the server and its rectangle re-uploaded.  Every
      // sample must be recorded in document pixels; the browser's
      // coverage must equal the server's to the last bit; the preview
      // must match what came back; parity must hold on the committed
      // pixels; and undo must give the picture back exactly.
      var saved = { tool: S.tool, brush: S.brush };
      var before = S.renderExact(), previewed = null, got = null;
      S.brush = { radius: 18, hardness: 0.7, spacing: 0.25, flow: 0.8, opacity: 0.9,
                  color: [230, 80, 70], mode: 'paint' };
      S.setTool('brush');
      var r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
      var points = [[0.3, 0.4, 0.2], [0.35, 0.42, 0.4], [0.4, 0.45, 0.6], [0.45, 0.5, 0.8],
                    [0.5, 0.52, 0.9], [0.55, 0.55, 1.0]];
      function fire(type, pt) {
        canvas.dispatchEvent(new PointerEvent(type, {
          clientX: r.left + pt[0] * r.width, clientY: r.top + pt[1] * r.height,
          pointerId: 41, pointerType: 'pen', pressure: pt[2], bubbles: true }));
      }
      fire('pointerdown', points[0]);
      points.slice(1).forEach(function (pt) { fire('pointermove', pt); });
      fire('pointerup', points[points.length - 1]);
      S.setTool(saved.tool);
      S.brush = saved.brush;
      got = S.lastStroke;
      var want = points.map(function (pt) {
        return [view.ox + pt[0] * r.width * dpr / view.scale, view.oy + pt[1] * r.height * dpr / view.scale, pt[2]];
      });
      var worst = got ? Math.max.apply(null, got.samples.map(function (s, k) {
        return Math.max(Math.abs(s.x - want[k][0]), Math.abs(s.y - want[k][1]), Math.abs(s.pressure - want[k][2]));
      })) : null;
      check('a brush stroke records every sample in document pixels',
            got && got.samples.length === points.length && got.layer === S.activeLayer &&
            got.pointerType === 'pen' && worst < 1e-3,
            { samples: got && got.samples.length, layer: got && got.layer, worst: worst });
      if (!got || !got.previewed) return null;
      return got.previewed.then(function () {
        // The commit's request is in flight; the preview is still up.
        previewed = S.renderExact();
        return S.strokeCheck(got);
      }).then(function (c) {
        check('the browser paints the coverage the server paints, to the last bit',
              !c.empty && c.maxDiff === 0, c);
        return S.strokesSettled();
      }).then(function () {
        var after = S.renderExact();
        var moved = renderDiff(before, after), kept = renderDiff(previewed, after);
        check('the committed stroke changed the picture', moved.maxDiff > 0, moved);
        // The commit rounds the painted layer to 8 bits once, which the
        // preview did not; through the stack above it that is up to one
        // more level than the usual float32-against-double one.
        check('the preview matches the committed stroke', kept.maxDiff <= 2, kept);
        return S.parity();
      }).then(function (p) {
        check('parity after a committed stroke', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        var back = renderDiff(before, S.renderExact());
        check('undo gives the picture back exactly', back.maxDiff === 0, back);
        return S.parity();
      }).then(function (p) {
        check('parity after undo', p.maxDiff <= 1, p);
        // Redo brings the stroke back exactly as committed; undo again.
        var committed = null;
        return S.redo().then(function () {
          committed = S.renderExact();
          check('redo brings the stroke back', renderDiff(before, committed).maxDiff > 0 && S.redoDepth === 0 && S.undoDepth > 0,
                { undo: S.undoDepth, redo: S.redoDepth });
          return S.undo();
        }).then(function () {
          check('undo after redo gives the picture back exactly', renderDiff(before, S.renderExact()).maxDiff === 0);
          return S.redo();
        }).then(function () {
          check('redo is exact', renderDiff(committed, S.renderExact()).maxDiff === 0);
          return S.undo();
        });
      });
    }).then(function () {
      // The stack through the menus: Layer > New Adjustment Layer >
      // Curves above the active layer, then Layer > Delete Layer; each
      // brings a new plan and keeps parity, and the stack ends as found.
      var n = doc.layers.length, active = S.activeLayer, builds = S.stats.programBuilds;
      var ran = S.menuRun('Layer', 'Curves...', 'New Adjustment Layer');
      return S.strokesSettled().then(function () {
        return new Promise(function (r) {
          var tries = 0;
          (function w() { if (doc.layers.length === n + 1 || !ran || tries++ > 500) r(); else setTimeout(w, 20); })();
        });
      }).then(function () {
        check('a new adjustment layer joins the stack above the active one',
              ran && doc.layers.length === n + 1 && S.activeLayer === active + 1 &&
              doc.layers[S.activeLayer].kind === 'curves' && S.stats.programBuilds === builds + 1,
              { ran: ran, layers: doc.layers.length, active: S.activeLayer, builds: S.stats.programBuilds - builds });
        return S.parity();
      }).then(function (p) {
        check('parity with the new layer', p.maxDiff <= 1, p);
        return S.layerOp({ op: 'delete' });
      }).then(function () {
        check('deleting it gives the stack back', doc.layers.length === n, { layers: doc.layers.length });
        S.setActiveLayer(active);
        return S.parity();
      }).then(function (p) {
        check('parity after the delete', p.maxDiff <= 1, p);
      });
    }).then(function () {
      // The palette: a tool not built is refused; the eyedropper puts
      // the color under the pointer into the foreground; X swaps the
      // swatches; a raster layer's thumbnail comes w*h*4 bytes.
      var saved = { tool: S.tool, color: S.brush.color, background: S.background };
      var refused = !S.setTool('healing') && S.tool === saved.tool;
      S.setTool('eyedropper');
      var r = canvas.getBoundingClientRect();
      canvas.dispatchEvent(new PointerEvent('pointerdown', {
        clientX: r.left + r.width / 2, clientY: r.top + r.height / 2, pointerId: 43, pointerType: 'mouse', bubbles: true }));
      canvas.dispatchEvent(new PointerEvent('pointerup', {
        clientX: r.left + r.width / 2, clientY: r.top + r.height / 2, pointerId: 43, pointerType: 'mouse', bubbles: true }));
      var picked = S.brush.color.slice();
      S.swapColors();
      var swapped = S.brush.color.join() === saved.background.join() && S.background.join() === picked.join();
      S.background = saved.background;
      S.setBrush({ color: saved.color });
      S.setTool(saved.tool);
      check('the palette refuses a tool not built', refused, { tool: S.tool });
      check('the eyedropper picks a color and X swaps the swatches',
            picked.length === 3 && picked.every(function (v) { return v >= 0 && v <= 255; }) && swapped,
            { picked: picked, swapped: swapped });
      var tex = doc.layers[S.activeLayer] && doc.layers[S.activeLayer].texIndex;
      return S.refreshThumb(tex).then(function (n) {
        check('a layer thumbnail comes from the server', n === 40 * 24 * 4, { bytes: n });
      });
    }).then(function () {
      // A file in and out: a small image drawn here and encoded as a PNG
      // by the browser decodes back byte for byte; placed as a layer it
      // lands centered and reads back from the server byte for byte;
      // opened, it is a new document of its size; then both go.
      var w = 50, h = 30, c2 = el('canvas', { width: String(w), height: String(h) });
      var ctx = c2.getContext('2d'), img = ctx.createImageData(w, h), want = new Uint8Array(w * h * 4);
      for (var i = 0; i < w * h; i++) {
        var a = i % 5 === 0 ? 0 : 255;
        want[4 * i] = a ? (i * 7) & 255 : 0;
        want[4 * i + 1] = a ? (i * 13) & 255 : 0;
        want[4 * i + 2] = a ? (i * 29) & 255 : 0;
        want[4 * i + 3] = a;
      }
      img.data.set(want);
      ctx.putImageData(img, 0, 0);
      var active = S.activeLayer, n = doc.layers.length, decoded = null;
      function same(a, b) { if (a.length !== b.length) return false; for (var k = 0; k < a.length; k++) if (a[k] !== b[k]) return false; return true; }
      return new Promise(function (r) { c2.toBlob(r, 'image/png'); }).then(function (blob) {
        return S.decodeImage(blob);
      }).then(function (d) {
        decoded = d;
        check('an image file decodes back byte for byte',
              d.width === w && d.height === h && same(d.bytes, want), { width: d.width, height: d.height });
        return S.placeImage(d, 'placed');
      }).then(function () {
        var L = doc.layers[S.activeLayer];
        var ox = Math.floor((doc.width - w) / 2), oy = Math.floor((doc.height - h) / 2);
        return fetchBytes(api('raster?i=' + L.texIndex + '&x=' + ox + '&y=' + oy + '&w=' + w + '&h=' + h)).then(function (b) {
          check('a placed image lands centered on its own layer, byte for byte',
                doc.layers.length === n + 1 && L.kind === 'raster' && L.name === 'placed' && same(b, want),
                { layers: doc.layers.length, name: L.name });
          return S.layerOp({ op: 'delete' });
        });
      }).then(function () {
        S.setActiveLayer(active);
        return S.openImage(decoded, 'selftest');
      }).then(function (res) {
        return fetchJSON('/stratum/api/doc?doc=' + encodeURIComponent(res.doc)).then(function (d) {
          check('an opened image is a new document of its size with one Background layer',
                d.width === w && d.height === h && d.layers.length === 1 && d.layers[0].name === 'Background',
                { doc: res.doc, width: d.width, height: d.height, layers: d.layers.length });
          return fetchJSON('/stratum/api/close?doc=' + encodeURIComponent(res.doc), { method: 'POST' });
        });
      });
    }).then(function () {
      // Masks: Layer > Layer Mask > Hide All on the active layer hides it
      // (parity intact); a white stroke on the mask reveals some of it,
      // previewed and committed through the mask's own live-stroke
      // stage; undo; Delete gives the stack back.
      // On the bottom layer, which has pixels everywhere, so a revealed
      // patch is sure to show.
      var active = S.activeLayer;
      S.setActiveLayer(0, false);
      var L0 = activeL(), n = doc.layers.length, before = S.renderExact(), hidden = null, previewed = null;
      if (!L0 || L0.kind !== 'raster' || L0.mask) { S.setActiveLayer(active, false); return null; }
      return S.layerOp({ op: 'mask', value: 'hide' }).then(function () {
        var L = activeL();
        check('Hide All gives the layer a mask and makes it the target',
              L && L.mask && S.maskTarget && doc.layers.length === n, { mask: L && L.mask, target: S.maskTarget });
        hidden = S.renderExact();
        check('a hidden layer changes the picture', renderDiff(before, hidden).maxDiff > 0);
        return S.parity();
      }).then(function (p) {
        check('parity with the new mask', p.maxDiff <= 1, p);
        var saved = { tool: S.tool, brush: S.brush };
        S.brush = { radius: 20, hardness: 0.8, spacing: 0.25, flow: 1, opacity: 1, color: [255, 255, 255], mode: 'paint' };
        S.setTool('brush');
        var r = canvas.getBoundingClientRect();
        function fire(type, fx, fy) {
          canvas.dispatchEvent(new PointerEvent(type, { clientX: r.left + fx * r.width, clientY: r.top + fy * r.height,
                                                        pointerId: 44, pointerType: 'mouse', bubbles: true }));
        }
        fire('pointerdown', 0.4, 0.5); fire('pointermove', 0.5, 0.5); fire('pointermove', 0.6, 0.5); fire('pointerup', 0.6, 0.5);
        S.setTool(saved.tool);
        S.brush = saved.brush;
        var got = S.lastStroke;
        check('the stroke went to the mask', got && got.target === 'mask', { target: got && got.target });
        return got.previewed.then(function () {
          previewed = S.renderExact();
          return S.strokesSettled();
        });
      }).then(function () {
        var after = S.renderExact();
        check('a white stroke on the mask reveals some of the layer', renderDiff(hidden, after).maxDiff > 0);
        check('the mask preview matches the committed stroke', renderDiff(previewed, after).maxDiff <= 2,
              renderDiff(previewed, after));
        return S.parity();
      }).then(function (p) {
        check('parity after a mask stroke', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the mask back exactly', renderDiff(hidden, S.renderExact()).maxDiff === 0);
        return S.layerOp({ op: 'mask', value: 'delete' });
      }).then(function () {
        var L = activeL();
        check('Delete takes the mask away and gives the picture back',
              L && !L.mask && !S.maskTarget && renderDiff(before, S.renderExact()).maxDiff === 0,
              { mask: L && L.mask });
        S.setActiveLayer(active, false);
      });
    }).then(function () {
      // Image > Adjustments > Levels on the bottom layer: previewed on
      // that layer alone, then baked by the server; the baked picture is
      // what the preview showed, parity holds, undo is exact.
      var active = S.activeLayer;
      S.setActiveLayer(0, false);
      var before = S.renderExact();
      var dialog = S.openAdjust('levels');
      if (!dialog) { S.setActiveLayer(active, false); return null; }
      dialog.set({ gamma: 1.6, 'in-black': 0.05 });
      var previewed = S.renderExact();
      check('an adjustment previews on its layer', renderDiff(before, previewed).maxDiff > 0);
      return dialog.ok().then(function () {
        var after = S.renderExact(), d = renderDiff(previewed, after);
        check('OK bakes what the preview showed', d.maxDiff <= 2, d);
        return S.parity();
      }).then(function (p) {
        check('parity after a baked adjustment', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the layer back exactly after an adjustment',
              renderDiff(before, S.renderExact()).maxDiff === 0);
        S.setActiveLayer(active, false);
      });
    }).then(function () {
      // Filter > Blur > Gaussian Blur on the Discs layer, whose edges
      // show a blur: previewed through the blur passes and the
      // replacement stage, baked by the server with the same kernel;
      // parity; undo.
      var active = S.activeLayer, discs = null;
      doc.layers.forEach(function (L) { if (!discs && L.kind === 'raster' && /disc/i.test(L.name)) discs = L; });
      if (!discs) return null;
      S.setActiveLayer(discs.index, false);
      var before = S.renderExact();
      var dialog = S.openFilter('gaussian-blur');
      dialog.set({ radius: 3 });
      var previewed = S.renderExact(), previews = S.filterPreviewSupported;
      check('Gaussian Blur previews on its layer', !previews || renderDiff(before, previewed).maxDiff > 0, { preview: previews });
      return dialog.ok().then(function () {
        var after = S.renderExact();
        check('the baked blur changed the picture', renderDiff(before, after).maxDiff > 0);
        if (previews) {
          var d = renderDiff(previewed, after);
          check('OK bakes what the blur preview showed', d.maxDiff <= 2, d);
        }
        return S.parity();
      }).then(function (p) {
        check('parity after a blur', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the layer back after a blur', renderDiff(before, S.renderExact()).maxDiff === 0);
        // Unsharp Mask on the same layer, then Add Noise, monochrome,
        // each previewed and baked within two levels of the preview.
        var d2 = S.openFilter('unsharp-mask');
        d2.set({ amount: 150, radius: 2, threshold: 0 });
        var sharpPreview = S.renderExact();
        return d2.ok().then(function () {
          var after = S.renderExact(), d = renderDiff(sharpPreview, after);
          check('Unsharp Mask changes the picture', renderDiff(before, after).maxDiff > 0);
          if (previews) check('OK bakes what the sharpening preview showed', d.maxDiff <= 2, d);
          return S.undo();
        }).then(function () {
          check('undo gives the layer back after a sharpening', renderDiff(before, S.renderExact()).maxDiff === 0);
          var d3 = S.openFilter('add-noise');
          d3.set({ amount: 20, monochrome: true, seed: 7 });
          var noisePreview = S.renderExact();
          return d3.ok().then(function () {
            var after = S.renderExact(), d = renderDiff(noisePreview, after);
            check('Add Noise changes the picture', renderDiff(before, after).maxDiff > 0);
            if (previews) check('OK bakes the noise the preview drew', d.maxDiff <= 2, d);
            return S.parity();
          });
        }).then(function (p) {
          check('parity after noise', p.maxDiff <= 1, p);
          return S.undo();
        }).then(function () {
          check('undo gives the layer back after noise', renderDiff(before, S.renderExact()).maxDiff === 0);
          S.setActiveLayer(active, false);
        });
      });
    }).then(function () {
      // The selection: a marquee rectangle through the tool, a stroke
      // under it that changes nothing outside it and still matches the
      // server's coverage to the last bit, Fill inside it, undo, Inverse
      // and Deselect.  Exact renders carry no ants.
      var active = S.activeLayer;
      S.setActiveLayer(0, false);
      var W = doc.width, H = doc.height, before = S.renderExact();
      var rx = Math.floor(W * 0.3), ry = Math.floor(H * 0.3), rw = Math.floor(W * 0.3), rh = Math.floor(H * 0.3);
      var saved = { tool: S.tool, brush: S.brush };
      S.setTool('marquee');
      var r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
      function at(x, y) {
        return { clientX: r.left + ((x - view.ox) * view.scale) / dpr, clientY: r.top + ((y - view.oy) * view.scale) / dpr,
                 pointerId: 45, pointerType: 'mouse', bubbles: true };
      }
      canvas.dispatchEvent(new PointerEvent('pointerdown', at(rx, ry)));
      canvas.dispatchEvent(new PointerEvent('pointermove', at(rx + rw, ry + rh)));
      canvas.dispatchEvent(new PointerEvent('pointerup', at(rx + rw, ry + rh)));
      S.setTool(saved.tool);
      function outsideChanged(a, b, rect) {
        var n = 0;
        for (var y = 0; y < H; y++) for (var x = 0; x < W; x++) {
          if (x >= rect.x && x < rect.x + rect.w && y >= rect.y && y < rect.y + rect.h) continue;
          var o = ((H - 1 - y) * W + x) * 4;
          if (a[o] !== b[o] || a[o + 1] !== b[o + 1] || a[o + 2] !== b[o + 2] || a[o + 3] !== b[o + 3]) n++;
        }
        return n;
      }
      return S.strokesSettled().then(function () {
        return new Promise(function (res) { var t = 0; (function w() { if (S.selection || t++ > 200) res(); else setTimeout(w, 20); })(); });
      }).then(function () {
        var sel = S.selection;
        check('the marquee selects its rectangle', sel && Math.abs(sel.x - rx) <= 1 && Math.abs(sel.y - ry) <= 1 &&
              Math.abs(sel.w - rw) <= 2 && Math.abs(sel.h - rh) <= 2, sel);
        S.brush = { radius: Math.max(8, Math.floor(W / 20)), hardness: 0.7, spacing: 0.25, flow: 1, opacity: 1, color: [20, 200, 90], mode: 'paint' };
        S.setTool('brush');
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(rx - rw / 2, ry + rh / 2)));
        canvas.dispatchEvent(new PointerEvent('pointermove', at(rx + rw / 2, ry + rh / 2)));
        canvas.dispatchEvent(new PointerEvent('pointerup', at(rx + rw / 2, ry + rh / 2)));
        S.setTool(saved.tool);
        S.brush = saved.brush;
        var got = S.lastStroke;
        return got.previewed.then(function () { return S.strokeCheck(got); });
      }).then(function (c) {
        check('under a selection the browser still paints the coverage the server paints, to the last bit',
              !c.empty && c.maxDiff === 0, c);
        return S.strokesSettled();
      }).then(function () {
        var after = S.renderExact(), sel = S.selection;
        check('a stroke under a selection changes nothing outside it',
              renderDiff(before, after).maxDiff > 0 && outsideChanged(before, after, sel) === 0,
              { outside: outsideChanged(before, after, sel) });
        return S.fill({ color: [255, 255, 255], opacity: 1, mode: 'paint' });
      }).then(function () {
        var filled = S.renderExact(), sel = S.selection;
        check('Fill paints the selection only', outsideChanged(before, filled, sel) === 0 && renderDiff(before, filled).maxDiff > 0);
        return S.undo().then(function () { return S.undo(); });
      }).then(function () {
        check('undo gives the picture back through the fill and the stroke', renderDiff(before, S.renderExact()).maxDiff === 0);
        return S.select({ op: 'inverse' });
      }).then(function (rect) {
        check('Inverse selects everything else', rect && rect.w === W && rect.h === H, rect);
        return S.select({ op: 'none' });
      }).then(function (rect) {
        check('Deselect clears the selection', rect === null && !S.selection);
        // The Magic Wand on the Sky gradient: a click through the tool
        // selects a band of similar color; Shift-click adds a second;
        // Feather softens the edge and widens the bounds; Deselect.
        S.setTool('wand');
        S.setBrush({});
        var first = null;
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(W * 0.5, H * 0.2)));
        canvas.dispatchEvent(new PointerEvent('pointerup', at(W * 0.5, H * 0.2)));
        return S.strokesSettled().then(function () {
          return new Promise(function (res) { var t = 0; (function w() { if (S.selection || t++ > 200) res(); else setTimeout(w, 20); })(); });
        }).then(function () {
          first = S.selection;
          check('the Magic Wand selects a band of similar color', first && first.w === W && first.h < H, first);
          var ev = at(W * 0.5, H * 0.8);
          ev.shiftKey = true;
          canvas.dispatchEvent(new PointerEvent('pointerdown', ev));
          canvas.dispatchEvent(new PointerEvent('pointerup', ev));
          return S.strokesSettled().then(function () {
            return new Promise(function (res) { var t = 0; (function w() { if ((S.selection && S.selection.h > first.h) || t++ > 200) res(); else setTimeout(w, 20); })(); });
          });
        }).then(function () {
          check('Shift-click adds a second band', S.selection && S.selection.h > first.h, S.selection);
          S.setTool(saved.tool);
          var before = S.selection;
          var d = S.openFeather();
          d.set({ radius: 6 });
          return d.ok().then(function (rect) {
            check('Feather widens the selection\'s bounds', rect && rect.h >= before.h, { before: before, after: rect });
            return S.select({ op: 'none' });
          });
        }).then(function () { S.setActiveLayer(active, false); });
      });
    }).then(function () {
      // The floating layer: a rectangle of the bottom layer dragged with
      // the Move tool, previewed through the shader's floating stage and
      // committed by the server with the selection moving along; undo;
      // then Copy and Paste (a new layer above with the same bytes where
      // they came from), Cut (the rectangle cleared) and undo.
      var active = S.activeLayer;
      S.setActiveLayer(0, false);
      var W = doc.width, H = doc.height, before = S.renderExact(), n = doc.layers.length;
      var rx = Math.floor(W * 0.2), ry = Math.floor(H * 0.2), rw = Math.floor(W * 0.2), rh = Math.floor(H * 0.2);
      var dx = Math.floor(W * 0.1), dy = Math.floor(H * 0.05), saved = S.tool, previewed = null;
      var r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
      function at(x, y) {
        return { clientX: r.left + ((x - view.ox) * view.scale) / dpr, clientY: r.top + ((y - view.oy) * view.scale) / dpr,
                 pointerId: 46, pointerType: 'mouse', bubbles: true };
      }
      function same(a, b) { if (a.length !== b.length) return false; for (var k = 0; k < a.length; k++) if (a[k] !== b[k]) return false; return true; }
      function rect(tex) { return fetchBytes(api('raster?i=' + tex + '&x=' + rx + '&y=' + ry + '&w=' + rw + '&h=' + rh)); }
      return S.select({ op: 'rect', x: rx, y: ry, w: rw, h: rh }).then(function () {
        S.setTool('move');
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(rx + 5, ry + 5)));
        canvas.dispatchEvent(new PointerEvent('pointermove', at(rx + 5 + dx, ry + 5 + dy)));
        previewed = S.renderExact();
        check('the Move tool previews the moved pixels', renderDiff(before, previewed).maxDiff > 0);
        canvas.dispatchEvent(new PointerEvent('pointerup', at(rx + 5 + dx, ry + 5 + dy)));
        S.setTool(saved);
        return S.strokesSettled();
      }).then(function () {
        var after = S.renderExact(), d = renderDiff(previewed, after), sel = S.selection;
        check('the committed move is what the preview showed', d.maxDiff <= 2, d);
        check('the selection moved with the pixels', sel && sel.x === rx + dx && sel.y === ry + dy, sel);
        return S.parity();
      }).then(function (p) {
        check('parity after a move', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the picture back after a move', renderDiff(before, S.renderExact()).maxDiff === 0);
        return S.select({ op: 'rect', x: rx, y: ry, w: rw, h: rh });
      }).then(function () { return S.clipboard('copy'); })
        .then(function () { return S.clipboard('paste'); })
        .then(function () {
          var L = doc.layers[S.activeLayer];
          return Promise.all([rect(L.texIndex), rect(doc.layers[0].texIndex)]).then(function (pair) {
            check('Paste makes a layer above with the copied bytes where they came from',
                  doc.layers.length === n + 1 && S.activeLayer === 1 && L.kind === 'raster' && same(pair[0], pair[1]),
                  { layers: doc.layers.length, active: S.activeLayer });
            return S.layerOp({ op: 'delete' });
          });
        }).then(function () {
          S.setActiveLayer(0, false);
          return S.clipboard('cut');
        }).then(function () {
          return rect(doc.layers[0].texIndex);
        }).then(function (b) {
          var clear = true;
          for (var k = 3; k < b.length; k += 4) if (b[k] !== 0) clear = false;
          check('Cut clears the selection on the layer', clear);
          return S.undo();
        }).then(function () {
          check('undo gives the picture back after a cut', renderDiff(before, S.renderExact()).maxDiff === 0);
          return S.select({ op: 'none' });
        }).then(function () { S.setActiveLayer(active, false); });
    }).then(function () {
      // Free Transform: a rectangle of the bottom layer scaled, turned
      // and moved, previewed through the floating stage with bilinear
      // sampling and committed by the server with the same; the
      // selection follows; parity; undo; and Escape leaves the picture.
      var active = S.activeLayer;
      S.setActiveLayer(0, false);
      var W = doc.width, H = doc.height, before = S.renderExact(), previewed = null;
      var rx = Math.floor(W * 0.25), ry = Math.floor(H * 0.25), rw = Math.floor(W * 0.3), rh = Math.floor(H * 0.3);
      return S.select({ op: 'rect', x: rx, y: ry, w: rw, h: rh }).then(function () {
        var t = S.beginTransform();
        t.set({ sx: 1.3, sy: 0.8, angle: 0.35, tx: Math.floor(W * 0.05), ty: Math.floor(H * 0.03) });
        previewed = S.renderExact();
        check('Free Transform previews the transformed pixels', renderDiff(before, previewed).maxDiff > 0);
        return t.commit();
      }).then(function () {
        var after = S.renderExact(), d = renderDiff(previewed, after), sel = S.selection;
        check('the committed transform is what the preview showed', d.maxDiff <= 2, d);
        check('the selection follows the transform', sel && (sel.w !== rw || sel.x !== rx), sel);
        return S.parity();
      }).then(function (p) {
        check('parity after a transform', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the picture back after a transform', renderDiff(before, S.renderExact()).maxDiff === 0);
        return S.select({ op: 'none' });
      }).then(function () {
        var t = S.beginTransform();
        t.set({ angle: 1 });
        t.cancel();
        check('Escape leaves the picture alone', !S.transforming && renderDiff(before, S.renderExact()).maxDiff === 0);
        S.setActiveLayer(active, false);
      });
    }).then(function () {
      // Crop, Image Size and Canvas Size on a throwaway document opened
      // from a small image, through the endpoint the dialogs use (this
      // page cannot reload mid-test): each answers the new size, and
      // the document says so afterwards.  The dialogs open and close.
      var w = 40, h = 24, c2 = el('canvas', { width: String(w), height: String(h) });
      var ctx = c2.getContext('2d');
      ctx.fillStyle = '#4080c0';
      ctx.fillRect(0, 0, w, h);
      var name = null;
      function sized(d) { return fetchJSON('/stratum/api/doc?doc=' + encodeURIComponent(d)).then(function (j) { return [j.width, j.height]; }); }
      function resize(body) {
        return fetchJSON('/stratum/api/resize?doc=' + encodeURIComponent(name),
                         { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
      }
      var dialog = S.openImageSize();
      dialog.set({ width: 100, height: 50 });
      var opened = !!document.querySelector('.st-dialog');
      dialog.cancel();
      check('the Image Size dialog opens and cancels', opened && !document.querySelector('.st-dialog') && !S.sizeDialog);
      return new Promise(function (r) { c2.toBlob(r, 'image/png'); }).then(S.decodeImage).then(function (img) {
        return S.openImage(img, 'sizetest');
      }).then(function (res) {
        name = res.doc;
        return fetchJSON('/stratum/api/select?doc=' + encodeURIComponent(name),
                         { method: 'POST', headers: { 'Content-Type': 'application/json' },
                           body: JSON.stringify({ op: 'rect', x: 10, y: 4, w: 20, h: 16 }) });
      }).then(function () { return resize({ op: 'crop' }); })
        .then(function (r) { return sized(name).then(function (s) { check('Crop remakes the document as the selection', r.width === 20 && r.height === 16 && s[0] === 20 && s[1] === 16, s); }); })
        .then(function () { return resize({ op: 'image-size', width: 60, height: 48 }); })
        .then(function () { return sized(name).then(function (s) { check('Image Size remakes the document at the new size', s[0] === 60 && s[1] === 48, s); }); })
        .then(function () { return resize({ op: 'canvas-size', width: 80, height: 80, anchor: [0, 0] }); })
        .then(function () { return sized(name).then(function (s) { check('Canvas Size remakes the document on the new canvas', s[0] === 80 && s[1] === 80, s); }); })
        .then(function () { return fetchJSON('/stratum/api/close?doc=' + encodeURIComponent(name), { method: 'POST' }); });
    }).then(function () {
      // File > New makes a document of the asked size with a white or a
      // transparent Background; the Crop tool's drag leaves a box that
      // Escape drops; a crop-rect through the endpoint crops the new
      // document; then it is closed.  The New dialog opens and cancels.
      var dlg = S.openNew();
      dlg.set({ name: 'newtest', width: 64, height: 48, background: 'transparent' });
      var opened = !!document.querySelector('.st-dialog');
      dlg.cancel();
      check('the New dialog opens and cancels', opened && !document.querySelector('.st-dialog') && !S.newDialog);
      var made = null;
      return postJSON('new', JSON.stringify({ name: 'newtest', width: 64, height: 48, background: 'transparent' })).then(function (res) {
        made = res.doc;
        return Promise.all([fetchJSON('/stratum/api/doc?doc=' + encodeURIComponent(made)),
                            fetchBytes('/stratum/api/raster?doc=' + encodeURIComponent(made) + '&i=0')]);
      }).then(function (pair) {
        var d = pair[0], px = pair[1], clear = true;
        for (var k = 3; k < px.length; k += 4) if (px[k] !== 0) clear = false;
        check('New makes a document of the asked size with one transparent Background',
              d.width === 64 && d.height === 48 && d.layers.length === 1 && d.layers[0].name === 'Background' && clear,
              { width: d.width, height: d.height, layers: d.layers.length });
        var saved = S.tool, r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
        function at(x, y) {
          return { clientX: r.left + ((x - view.ox) * view.scale) / dpr, clientY: r.top + ((y - view.oy) * view.scale) / dpr,
                   pointerId: 47, pointerType: 'mouse', bubbles: true };
        }
        S.setTool('crop');
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(10, 10)));
        canvas.dispatchEvent(new PointerEvent('pointermove', at(110, 70)));
        canvas.dispatchEvent(new PointerEvent('pointerup', at(110, 70)));
        var box = S.cropBox;
        S.cancelCrop();
        S.setTool(saved);
        check('the Crop tool leaves a box that Escape drops', box && box.w === 100 && box.h === 60 && !S.cropBox, box);
        return fetchJSON('/stratum/api/resize?doc=' + encodeURIComponent(made),
                         { method: 'POST', headers: { 'Content-Type': 'application/json' },
                           body: JSON.stringify({ op: 'crop-rect', x: 10, y: 8, w: 30, h: 20 }) });
      }).then(function (res) {
        check('a crop to a rectangle remakes the document at that size', res.width === 30 && res.height === 20, res);
        return fetchJSON('/stratum/api/close?doc=' + encodeURIComponent(made), { method: 'POST' });
      });
    }).then(function () {
      // The Gradient tool on the lowest raster layer: a drag across it
      // previews a ramp from the foreground to the background through
      // the replacement stage and bakes it when the pointer lifts, the
      // bake within two levels of the preview, the layer's bytes the
      // two colors at the ends and their encoded mean in the middle;
      // parity; undo; a radial ramp under a selection changes nothing
      // outside it.
      var active = S.activeLayer, L = null;
      doc.layers.forEach(function (x) { if (!L && x.kind === 'raster') L = x; });
      if (!L) return null;
      S.setActiveLayer(L.index, false);
      var saved = { tool: S.tool, color: S.brush.color, background: S.background, options: Object.assign({}, S.gradient) };
      S.setBrush({ color: [200, 30, 30] });
      S.background = [20, 40, 220];
      Object.assign(S.gradient, { preset: 'Foreground to Background', shape: 'linear', opacity: 1, reverse: false });
      var W = doc.width, H = doc.height, before = S.renderExact(), previews = S.filterPreviewSupported;
      var r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
      function at(x, y) {
        return { clientX: r.left + ((x - view.ox) * view.scale) / dpr, clientY: r.top + ((y - view.oy) * view.scale) / dpr,
                 pointerId: 48, pointerType: 'mouse', bubbles: true };
      }
      function dragRamp(x0, y0, x1, y1) {
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(x0, y0)));
        canvas.dispatchEvent(new PointerEvent('pointermove', at(x1, y1)));
        var shown = S.renderExact();
        canvas.dispatchEvent(new PointerEvent('pointerup', at(x1, y1)));
        return S.strokesSettled().then(function () { return shown; });
      }
      function outside(a, b, rect) {
        var n = 0;
        for (var y = 0; y < H; y++) for (var x = 0; x < W; x++) {
          if (x >= rect.x && x < rect.x + rect.w && y >= rect.y && y < rect.y + rect.h) continue;
          var o = ((H - 1 - y) * W + x) * 4;
          if (a[o] !== b[o] || a[o + 1] !== b[o + 1] || a[o + 2] !== b[o + 2] || a[o + 3] !== b[o + 3]) n++;
        }
        return n;
      }
      S.setTool('gradient');
      var x0 = Math.round(W * 0.1), x1 = Math.round(W * 0.9), ym = Math.round(H * 0.5);
      return dragRamp(x0, ym, x1, ym).then(function (shown) {
        var after = S.renderExact();
        check('a gradient drag previews on its layer and bakes on release',
              renderDiff(before, after).maxDiff > 0 && (!previews || renderDiff(before, shown).maxDiff > 0), { preview: previews });
        if (previews) {
          var d = renderDiff(shown, after);
          check('release bakes what the gradient preview showed', d.maxDiff <= 2, d);
        }
        return fetchBytes(api('raster?i=' + L.texIndex));
      }).then(function (px) {
        function px4(x, y) { var o = (y * W + x) * 4; return [px[o], px[o + 1], px[o + 2], px[o + 3]]; }
        function near(a, b, tol) { return a.every(function (v, i) { return Math.abs(v - b[i]) <= tol; }); }
        var left = px4(Math.round(W * 0.05), ym), right = px4(Math.round(W * 0.95), ym), mid = px4(Math.round(W * 0.5), ym);
        check('the ramp runs from the foreground to the background, their encoded mean in the middle',
              near(left, [200, 30, 30, 255], 1) && near(right, [20, 40, 220, 255], 1) && near(mid, [110, 35, 125, 255], 2),
              { left: left, mid: mid, right: right });
        return S.parity();
      }).then(function (p) {
        check('parity after a gradient', p.maxDiff <= 1, p);
        return S.undo();
      }).then(function () {
        check('undo gives the layer back after a gradient', renderDiff(before, S.renderExact()).maxDiff === 0);
        var rect = { x: Math.floor(W * 0.3), y: Math.floor(H * 0.3), w: Math.floor(W * 0.4), h: Math.floor(H * 0.4) };
        S.gradient.shape = 'radial';
        return S.select({ op: 'rect', x: rect.x, y: rect.y, w: rect.w, h: rect.h }).then(function () {
          return dragRamp(W * 0.5, H * 0.5, W * 0.5, H * 0.3);
        }).then(function (shown) {
          var after = S.renderExact(), d = renderDiff(shown, after);
          check('a radial gradient under a selection changes nothing outside it',
                renderDiff(before, after).maxDiff > 0 && outside(before, after, rect) === 0 && (!previews || d.maxDiff <= 2),
                { outside: outside(before, after, rect), preview: d });
          return S.undo();
        }).then(function () { return S.select({ op: 'none' }); });
      }).then(function () {
        check('undo gives the layer back after the radial gradient', renderDiff(before, S.renderExact()).maxDiff === 0);
        S.setTool(saved.tool);
        S.setBrush({ color: saved.color });
        S.background = saved.background;
        Object.assign(S.gradient, saved.options);
        S.setActiveLayer(active, false);
      });
    }).then(function () {
      // The Clone Stamp on the lowest raster layer: Alt-click sets the
      // source; a stroke paints the layer's own pixels from there,
      // previewed through the live-stroke stage with the sampled paint
      // and committed within two levels of the preview; the stroke's
      // first dab takes the pixel at its source; parity; Aligned keeps
      // the offset for a second stroke; undo twice gives the layer back.
      var active = S.activeLayer, L = null;
      doc.layers.forEach(function (x) { if (!L && x.kind === 'raster') L = x; });
      if (!L) return null;
      S.setActiveLayer(L.index, false);
      var saved = { tool: S.tool, brush: S.brush, aligned: S.clone.aligned, source: S.cloneSource, offset: S.cloneOffset };
      S.brush = { radius: 12, hardness: 1, spacing: 0.25, flow: 1, opacity: 1, color: [0, 0, 0], mode: 'paint' };
      S.clone.aligned = true;
      S.cloneSource = null;
      S.cloneOffset = null;
      var W = doc.width, H = doc.height, before = S.renderExact(), bytesBefore = null;
      var r = canvas.getBoundingClientRect(), dpr = window.devicePixelRatio || 1;
      function at(x, y, alt) {
        return { clientX: r.left + ((x - view.ox) * view.scale) / dpr, clientY: r.top + ((y - view.oy) * view.scale) / dpr,
                 pointerId: 49, pointerType: 'mouse', bubbles: true, altKey: !!alt };
      }
      function px4(b, x, y) { var o = (y * W + x) * 4; return [b[o], b[o + 1], b[o + 2], b[o + 3]]; }
      var sx = Math.round(W * 0.2), sy = Math.round(H * 0.25), dx = Math.round(W * 0.6), dy = Math.round(H * 0.6);
      S.setTool('stamp');
      function restore() {
        S.setTool(saved.tool);
        S.brush = saved.brush;
        S.clone.aligned = saved.aligned;
        S.cloneSource = saved.source;
        S.cloneOffset = saved.offset;
        S.setActiveLayer(active, false);
      }
      return fetchBytes(api('raster?i=' + L.texIndex)).then(function (px) {
        bytesBefore = px;
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(sx, sy, true)));
        canvas.dispatchEvent(new PointerEvent('pointerup', at(sx, sy, true)));
        check('Alt-click sets the clone source',
              S.cloneSource && Math.round(S.cloneSource.x) === sx && Math.round(S.cloneSource.y) === sy, S.cloneSource);
        canvas.dispatchEvent(new PointerEvent('pointerdown', at(dx, dy)));
        canvas.dispatchEvent(new PointerEvent('pointermove', at(dx + 20, dy + 8)));
        canvas.dispatchEvent(new PointerEvent('pointerup', at(dx + 20, dy + 8)));
        var got = S.lastStroke;
        check('a stamp stroke carries the offset from its start to the source',
              got && got.clone && got.clone[0] === sx - dx && got.clone[1] === sy - dy, got && got.clone);
        if (!got || !got.previewed) { restore(); return null; }
        return got.previewed.then(function () {
          var shown = S.renderExact();
          return S.strokesSettled().then(function () {
            var after = S.renderExact(), kept = renderDiff(shown, after);
            check('the stamp stroke changed the picture and the commit matches its preview',
                  renderDiff(before, after).maxDiff > 0 && kept.maxDiff <= 2, kept);
            return fetchBytes(api('raster?i=' + L.texIndex));
          });
        }).then(function (px) {
          var painted = px4(px, dx, dy), source = px4(bytesBefore, sx, sy);
          check('the first dab takes the pixel at its source',
                painted.every(function (v, i) { return Math.abs(v - source[i]) <= 1; }), { painted: painted, source: source });
          return S.parity();
        }).then(function (p) {
          check('parity after a stamp stroke', p.maxDiff <= 1, p);
          canvas.dispatchEvent(new PointerEvent('pointerdown', at(dx + 40, dy + 30)));
          canvas.dispatchEvent(new PointerEvent('pointerup', at(dx + 40, dy + 30)));
          var second = S.lastStroke;
          check('Aligned keeps the offset for the next stroke',
                second && second.clone && second.clone[0] === sx - dx && second.clone[1] === sy - dy, second && second.clone);
          return S.strokesSettled().then(function () { return S.undo(); }).then(function () { return S.undo(); });
        }).then(function () {
          check('undo gives the layer back after two stamp strokes', renderDiff(before, S.renderExact()).maxDiff === 0);
          restore();
        });
      });
    }).then(function () {
      // Merge Down and Flatten on a throwaway document: an image opened,
      // a second image placed on it, the two merged into one layer that
      // keeps the lower one's name, then a curves layer added and the
      // document flattened into one Background.
      var w = 40, h = 24, c2 = el('canvas', { width: String(w), height: String(h) }), ctx = c2.getContext('2d');
      ctx.fillStyle = '#c04040';
      ctx.fillRect(0, 0, w, h);
      var name = null;
      function op(body) {
        return fetchJSON('/stratum/api/layers?doc=' + encodeURIComponent(name),
                         { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
      }
      function layers() { return fetchJSON('/stratum/api/doc?doc=' + encodeURIComponent(name)).then(function (d) { return d.layers; }); }
      return new Promise(function (r) { c2.toBlob(r, 'image/png'); }).then(S.decodeImage).then(function (img) {
        return S.openImage(img, 'mergetest').then(function (res) {
          name = res.doc;
          return fetch('/stratum/api/place?doc=' + encodeURIComponent(name) + '&name=top&w=' + w + '&h=' + h + '&above=0',
                       { method: 'POST', headers: { 'Content-Type': 'application/octet-stream' }, body: img.bytes });
        });
      }).then(function () { return op({ op: 'merge', layer: 1 }); })
        .then(function (res) { return layers().then(function (L) {
          check('Merge Down leaves one raster layer with the lower name', res.active === 0 && L.length === 1 && L[0].name === 'Background' && L[0].kind === 'raster', L.map(function (x) { return x.name; }));
          return op({ op: 'add', kind: 'curves', above: 0 });
        }); })
        .then(function () { return op({ op: 'flatten' }); })
        .then(function () { return layers().then(function (L) {
          check('Flatten leaves one Background layer', L.length === 1 && L[0].name === 'Background' && L[0].kind === 'raster', L.map(function (x) { return x.name; }));
          return fetchJSON('/stratum/api/close?doc=' + encodeURIComponent(name), { method: 'POST' });
        }); });
    }).then(function () {
      // The native file: Save brings a .stratum file of this document;
      // loaded back as a new document it has the same size and stack
      // and renders byte for byte the same on the server.
      var loaded = null;
      return fetchBytes(api('save')).then(function (bytes) {
        var magic = String.fromCharCode.apply(null, bytes.subarray(0, 8));
        check('Save brings a Stratum file', magic === 'STRATUM1' && bytes.length > 1000, { bytes: bytes.length });
        return postBytes('load?name=roundtrip', bytes);
      }).then(function (res) {
        loaded = res.doc;
        return fetchJSON('/stratum/api/doc?doc=' + encodeURIComponent(loaded));
      }).then(function (d) {
        var same = d.width === doc.width && d.height === doc.height && d.layers.length === doc.layers.length &&
          d.layers.every(function (L, k) { return L.name === doc.layers[k].name && L.kind === doc.layers[k].kind; });
        check('loaded back, it is the same stack', same, { width: d.width, height: d.height, layers: d.layers.length });
        return Promise.all([fetchBytes(api('cpu')), fetchBytes('/stratum/api/cpu?doc=' + encodeURIComponent(loaded))]);
      }).then(function (pair) {
        var same = pair[0].length === pair[1].length;
        for (var k = 0; same && k < pair[0].length; k++) if (pair[0][k] !== pair[1][k]) same = false;
        check('loaded back, it renders byte for byte the same', same, { bytes: pair[0].length });
        return fetchJSON('/stratum/api/close?doc=' + encodeURIComponent(loaded), { method: 'POST' });
      });
    }).then(function () {
      return { ok: ok, doc: doc.name, steps: steps };
    }, function (e) {
      check('no exception', false, String(e && e.message || e));
      return { ok: false, doc: doc && doc.name, steps: steps };
    });
  };

  // The panel's Benchmark button: 120 frames of pan plus curve drag,
  // the result in the status lines, so a real GPU's number needs no
  // console.
  function installBenchButton() {
    var out = el('div', { 'class': 'st-bench' });
    var btn = el('button', { type: 'button', 'class': 'st-bench-btn', text: 'Benchmark (pan + curve drag)' });
    btn.addEventListener('click', function () {
      btn.disabled = true;
      out.textContent = 'running...';
      setTimeout(function () {
        var r = S.bench(120, true);
        out.textContent = r.frames + ' frames at ' + r.canvas[0] + ' x ' + r.canvas[1] + ': ' +
          r.msPerFrame.toFixed(2) + ' ms a frame (' + Math.round(r.fps) + ' fps), worst ' +
          r.worstMs.toFixed(1) + ' ms';
        S.lastBench = r;
        btn.disabled = false;
      }, 50);
    });
    statusEl.parentNode.insertBefore(el('div', { 'class': 'st-row' }, [btn]), statusEl.nextSibling);
    statusEl.parentNode.insertBefore(out, btn.parentNode.nextSibling);
  }

  S.view = view;
  S.redraw = requestDraw;
  S.doc = function () { return doc; };

  // ---- start --------------------------------------------------------------

  function start() {
    canvas = document.getElementById('stratum-canvas');
    overlay = document.getElementById('stratum-overlay');
    statusEl = document.getElementById('stratum-status');
    layersEl = document.getElementById('stratum-layers');
    gl = canvas.getContext('webgl2', { antialias: false, preserveDrawingBuffer: true });
    if (!gl) { status('WebGL 2 is not available in this browser.'); S.error = 'no webgl2'; return; }
    vao = gl.createVertexArray();
    strokeTexture(1, 1);
    selectionTexture(1, 1);
    texIdle = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, texIdle);
    gl.texStorage2D(gl.TEXTURE_2D, 1, gl.RGBA8, 1, 1);
    installNavigation();
    var dpr = window.devicePixelRatio || 1;
    canvas.width = Math.max(1, Math.round(canvas.clientWidth * dpr));
    canvas.height = Math.max(1, Math.round(canvas.clientHeight * dpr));
    if (overlay) { overlay.width = canvas.width; overlay.height = canvas.height; }
    fetchJSON(api('doc')).then(function (d) {
      doc = d;
      fitView();
      buildProgram(doc.shader);
      syncOpacity();
      uploadLuts();
      buildPanel();
      return uploadPixels();
    }).then(function () {
      return loadSelection(doc.selection);
    }).then(function () {
      status();
      draw();
      installBenchButton();
      installToolbar();
      installTools();
      installMenubar();
      S.ready = true;
      if (window.ResizeObserver) new ResizeObserver(resize).observe(canvas);
    }).catch(function (e) {
      S.error = String(e && e.message || e);
      status('error: ' + S.error);
      console.error(e);
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
