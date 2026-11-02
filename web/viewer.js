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
// uses, translated by Parenscript into /stratum/dual.js.
//
// window.STRATUM exposes the test hooks: parity() compares a GPU
// render with the server's CPU render pixel for pixel, and bench()
// measures frame times.

(function () {
  'use strict';

  var S = window.STRATUM = { stats: { draws: 0, programBuilds: 0, lutUploads: 0 } };
  var qs = new URLSearchParams(location.search);
  var docName = qs.get('doc') || 'demo';

  function api(path) {
    return '/stratum/api/' + path + (path.indexOf('?') < 0 ? '?' : '&') +
      'doc=' + encodeURIComponent(docName);
  }

  var VERT = '#version 300 es\n' +
    'const vec2 P[3] = vec2[3](vec2(-1.0, -1.0), vec2(3.0, -1.0), vec2(-1.0, 3.0));\n' +
    'void main() { gl_Position = vec4(P[gl_VertexID], 0.0, 1.0); }\n';

  var canvas, gl, vao, prog = null, uni = {}, doc = null;
  var texLayers = null, texMasks = null, texLuts = null;
  var opacity = null, lutData = null;
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
    ['u_layers', 'u_masks', 'u_luts', 'u_opacity', 'u_background', 'u_doc_size',
     'u_view_offset', 'u_view_scale', 'u_canvas_h', 'u_exact'].forEach(function (n) {
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

  // ---- parameters: opacity uniforms and curve tables ----------------------

  function syncOpacity() {
    if (!opacity || opacity.length !== Math.max(1, doc.layers.length)) {
      opacity = new Float32Array(Math.max(1, doc.layers.length));
    }
    doc.layers.forEach(function (L, i) { opacity[i] = L.visible ? L.opacity : 0; });
  }

  function column(points, k) { return points.map(function (p) { return p[k]; }); }

  function fillLutRows(L) {
    var m = L.curves.master;
    ['red', 'green', 'blue'].forEach(function (ch, k) {
      var c = L.curves[ch];
      var lut = composedLut(column(m, 0), column(m, 1), column(c, 0), column(c, 1), 256);
      lutData.set(lut, (L.lutRow + k) * 256);
    });
  }

  function uploadLuts() {
    var rows = Math.max(1, doc.lutRows);
    if (!lutData || lutData.length !== rows * 256) {
      lutData = new Float32Array(rows * 256);
      if (texLuts) gl.deleteTexture(texLuts);
      texLuts = gl.createTexture();
      gl.bindTexture(gl.TEXTURE_2D, texLuts);
      gl.texStorage2D(gl.TEXTURE_2D, 1, gl.R32F, 256, rows);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    }
    doc.layers.forEach(function (L) { if (L.kind === 'curves') fillLutRows(L); });
    gl.bindTexture(gl.TEXTURE_2D, texLuts);
    gl.texSubImage2D(gl.TEXTURE_2D, 0, 0, 0, 256, rows, gl.RED, gl.FLOAT, lutData);
    S.stats.lutUploads++;
  }

  // ---- drawing ------------------------------------------------------------

  function bindAndSet(canvasH, v, exact) {
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
    gl.uniform1fv(uni.u_opacity, opacity);
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

  function resize() {
    var dpr = window.devicePixelRatio || 1;
    var w = Math.max(1, Math.round(canvas.clientWidth * dpr));
    var h = Math.max(1, Math.round(canvas.clientHeight * dpr));
    if (canvas.width !== w || canvas.height !== h) {
      var cx = view.ox + canvas.width / (2 * view.scale);
      var cy = view.oy + canvas.height / (2 * view.scale);
      canvas.width = w;
      canvas.height = h;
      view.ox = cx - w / (2 * view.scale);
      view.oy = cy - h / (2 * view.scale);
      requestDraw();
    }
  }

  // ---- pan and zoom -------------------------------------------------------

  function installNavigation() {
    var drag = null;
    canvas.addEventListener('pointerdown', function (e) {
      drag = { x: e.clientX, y: e.clientY };
      canvas.setPointerCapture(e.pointerId);
      canvas.style.cursor = 'grabbing';
    });
    canvas.addEventListener('pointermove', function (e) {
      if (!drag) return;
      var dpr = window.devicePixelRatio || 1;
      view.ox -= (e.clientX - drag.x) * dpr / view.scale;
      view.oy -= (e.clientY - drag.y) * dpr / view.scale;
      drag = { x: e.clientX, y: e.clientY };
      requestDraw();
    });
    function end() { drag = null; canvas.style.cursor = ''; }
    canvas.addEventListener('pointerup', end);
    canvas.addEventListener('pointercancel', end);
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
    canvas.addEventListener('dblclick', function () { fitView(); status(); requestDraw(); });
  }

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

  // A plan change brings a new shader; the pixels stay where they are.
  function reloadStructure() {
    return fetchJSON(api('doc')).then(function (d) {
      var repixel = d.rasterCount !== doc.rasterCount || d.maskCount !== doc.maskCount;
      doc = d;
      buildProgram(doc.shader);
      syncOpacity();
      uploadLuts();
      return repixel ? uploadPixels() : null;
    }).then(function () { status(); requestDraw(); });
  }

  // ---- the layer panel ----------------------------------------------------

  function el(tag, attrs, kids) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      if (k === 'text') e.textContent = attrs[k]; else e.setAttribute(k, attrs[k]);
    });
    (kids || []).forEach(function (k) { e.appendChild(k); });
    return e;
  }

  function buildPanel() {
    layersEl.textContent = '';
    for (var n = doc.layers.length - 1; n >= 0; n--) layersEl.appendChild(layerRow(doc.layers[n]));
  }

  function layerRow(L) {
    var i = L.index;
    var vis = el('input', { type: 'checkbox' });
    vis.checked = L.visible;
    vis.addEventListener('change', function () {
      L.visible = vis.checked;
      syncOpacity();
      requestDraw();
      post({ layer: i, slot: 'visible', value: L.visible });
    });
    var kind = L.kind === 'raster' ? 'raster' : 'curves';
    var head = el('div', { 'class': 'st-head' }, [
      vis, el('span', { 'class': 'st-name', text: L.name }),
      el('span', { 'class': 'st-kind', text: kind + (L.mask ? ' + mask' : '') })]);
    var range = el('input', { type: 'range', min: '0', max: '100', value: String(Math.round(L.opacity * 100)) });
    var val = el('span', { text: Math.round(L.opacity * 100) + '%' });
    range.addEventListener('input', function () {
      L.opacity = range.value / 100;
      val.textContent = range.value + '%';
      syncOpacity();
      requestDraw();
      postSoon('opacity' + i, { layer: i, slot: 'opacity', value: L.opacity });
    });
    var row = el('div', { 'class': 'st-layer' }, [head, el('div', { 'class': 'st-row' }, [range, val])]);
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
      row.appendChild(el('div', { 'class': 'st-row' }, [el('span', { text: 'blend' }), sel]));
    } else {
      row.appendChild(curveEditor(L));
    }
    return row;
  }

  // A small curves editor: drag a point; click empty space to add one;
  // double-click a point to remove it.  The table is rebuilt in the
  // browser on every move; the server hears about it when the drag rests.
  function curveEditor(L) {
    var size = 180, pad = 8;
    var wrap = el('div');
    var chan = el('select', { 'class': 'st-chan' });
    ['master', 'red', 'green', 'blue'].forEach(function (c) { chan.appendChild(el('option', { value: c, text: c })); });
    var cv = el('canvas', { 'class': 'st-curve', width: String(size), height: String(size) });
    var ctx = cv.getContext('2d');
    var active = -1;
    function pts() { return L.curves[chan.value]; }
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
      uploadLuts();
      requestDraw();
      paint();
      postSoon('curve' + L.index + chan.value,
               { layer: L.index, slot: 'curve', channel: chan.value, points: pts().slice() });
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
    S.curveEditors = S.curveEditors || {};
    S.curveEditors[L.index] = { layer: L, changed: changed };
    return wrap;
  }

  // ---- test hooks ---------------------------------------------------------

  // Render the whole document at 1:1 with texelFetch into an offscreen
  // target, read it back, and compare with the server's CPU render.
  S.parity = function () {
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
        if (L.kind !== 'curves') return;
        var m = L.curves.master;
        ['red', 'green', 'blue'].forEach(function (ch, k) {
          var c = L.curves[ch];
          var lut = composedLut(column(m, 0), column(m, 1), column(c, 0), column(c, 1), 256);
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
    var btn = el('button', { type: 'button', text: 'Benchmark (pan + curve drag)' });
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
    statusEl = document.getElementById('stratum-status');
    layersEl = document.getElementById('stratum-layers');
    gl = canvas.getContext('webgl2', { antialias: false, preserveDrawingBuffer: true });
    if (!gl) { status('WebGL 2 is not available in this browser.'); S.error = 'no webgl2'; return; }
    vao = gl.createVertexArray();
    installNavigation();
    var dpr = window.devicePixelRatio || 1;
    canvas.width = Math.max(1, Math.round(canvas.clientWidth * dpr));
    canvas.height = Math.max(1, Math.round(canvas.clientHeight * dpr));
    fetchJSON(api('doc')).then(function (d) {
      doc = d;
      fitView();
      buildProgram(doc.shader);
      syncOpacity();
      uploadLuts();
      buildPanel();
      return uploadPixels();
    }).then(function () {
      status();
      draw();
      installBenchButton();
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
