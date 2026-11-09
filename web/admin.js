// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// The admin page, /stratum/admin (source/admin.lisp): the visitors with
// what each did, session by session, and the sessions in memory with
// their documents' layers.  It only reads.  Everything shown here that
// a visitor typed -- a prompt, a document's name, a request's body --
// goes into the page as text, never as markup.

(function () {
  'use strict';

  var root = document.getElementById('admin');
  var opened = {};   // visitor key -> true while that visitor's card is open
  var limits = {};   // visitor key -> how many events to ask for
  var live = {};     // session id -> true for the sessions in memory
  var EVENTS = 300;

  function el(tag, attrs, kids) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      if (k === 'text') e.textContent = attrs[k]; else e.setAttribute(k, attrs[k]);
    });
    (kids || []).forEach(function (k) {
      if (k) e.appendChild(typeof k === 'string' ? document.createTextNode(k) : k);
    });
    return e;
  }

  function get(path) {
    return fetch(path, { credentials: 'same-origin', cache: 'no-store' }).then(function (r) {
      if (!r.ok) return r.text().then(function (t) { throw new Error(r.status + ' ' + t.slice(0, 300)); });
      return r.json();
    });
  }

  function when(iso) {
    var d = iso ? new Date(iso) : null;
    if (!d) return '';
    if (isNaN(d.getTime())) return String(iso);
    return d.toLocaleString(undefined, { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit' });
  }

  function ago(iso) {
    var d = iso ? new Date(iso) : null;
    if (!d || isNaN(d.getTime())) return '';
    var s = Math.max(0, Math.round((Date.now() - d.getTime()) / 1000));
    if (s < 90) return s + ' s ago';
    if (s < 5400) return Math.round(s / 60) + ' min ago';
    if (s < 129600) return Math.round(s / 3600) + ' h ago';
    return Math.round(s / 86400) + ' days ago';
  }

  function count(n, one, many) {
    return n + ' ' + (n === 1 ? one : (many || one + 's'));
  }

  function pill(text, kind) {
    return el('span', { 'class': 'ad-pill' + (kind ? ' ad-' + kind : ''), text: text });
  }

  function fact(label, value, kind) {
    return el('span', kind ? { 'class': 'ad-' + kind } : {}, [label + ' ', el('b', { text: String(value) })]);
  }

  function more(label, text) {
    return el('details', { 'class': 'ad-more' }, [el('summary', { text: label }), el('pre', { 'class': 'ad-mono', text: text })]);
  }

  function shortId(id) {
    return id && id.length > 14 ? id.slice(0, 12) + '…' : id;
  }

  // ---- one visitor's record -------------------------------------------------

  function pictures(links) {
    return el('div', { 'class': 'ad-pictures' }, links.map(function (p) {
      return el('figure', { title: p.title || p.label }, [
        el('a', { href: p.full || p.src, target: '_blank', rel: 'noopener' },
           [el('img', { src: p.src, alt: p.label, loading: 'lazy' })]),
        el('figcaption', { text: p.label })]);
    }));
  }

  function eventRow(ev, label, kind, what) {
    return el('div', { 'class': 'ad-event' }, [
      el('span', { 'class': 'ad-time', title: ev.time || '', text: when(ev.time) }),
      el('span', {}, [pill(label, kind)]),
      el('div', { 'class': 'ad-what' }, what)]);
  }

  // A prompt as the activity log has it: the request's body, which may
  // have been cut short and then is not JSON any more.
  function asked(body) {
    try {
      var parsed = JSON.parse(body);
      return { prompt: parsed.prompt, generate: parsed.generate };
    } catch (e) {
      return { raw: body };
    }
  }

  function lineRow(v, ev) {
    var doc = ev.document ? el('span', { 'class': 'ad-dim', text: ' in ' + ev.document }) : null;
    if (ev.kind === 'visit') return eventRow(ev, 'visit', '', ['Opened the page', doc]);
    if (ev.kind === 'download') return eventRow(ev, 'download', '', [el('span', { 'class': 'ad-mono', text: ev.door }), doc]);
    if (ev.kind === 'upload') {
      var src = '/stratum/api/admin/upload?who=' + encodeURIComponent(v.key) + '&file=' + encodeURIComponent(ev.file);
      return eventRow(ev, 'upload', '', [
        el('span', { 'class': 'ad-mono', text: ev.door }), doc,
        el('span', { 'class': 'ad-dim', text: ' ' + (ev.bytes === null ? '' : Math.round(ev.bytes / 1024) + ' KB') }),
        pictures([{ src: src, full: src + '&size=1280', label: ev.file.slice(ev.file.lastIndexOf('/') + 1) }])]);
    }
    if (ev.kind === 'asked') {
      var a = asked(ev.body || '');
      return eventRow(ev, 'asked', '', [
        el('span', { 'class': 'ad-dim', text: a.generate ? 'Sent the Generate dialog' : 'Sent a prompt' }), doc,
        a.raw !== undefined ? more('request body', a.raw)
          : el('div', { 'class': 'ad-words', text: a.prompt || (a.generate ? JSON.stringify(a.generate) : '') })]);
    }
    return eventRow(ev, 'by hand', 'hand', [
      el('span', { 'class': 'ad-mono', text: (ev.method || '').toUpperCase() + ' ' + ev.door }), doc,
      ev.body ? more('request body', ev.body) : null]);
  }

  function promptRow(ev) {
    var facts = [ev.byHand ? pill('by hand: Generate dialog', 'hand') : pill('planner: ' + (ev.model || 'unknown'), 'model')];
    if (ev.document) facts.push(el('span', { 'class': 'ad-dim', text: ' in ' + ev.document }));
    if (ev.seconds !== null) facts.push(el('span', { 'class': 'ad-dim', text: ' ' + (Math.round(ev.seconds * 10) / 10) + ' s' }));
    if (ev.rounds) facts.push(el('span', { 'class': 'ad-dim', text: ' ' + count(ev.rounds, 'round') }));
    if (ev.effort) facts.push(el('span', { 'class': 'ad-dim', text: ' effort ' + ev.effort }));
    if (ev.declined) facts.push(el('span', { 'class': 'ad-warn', text: ' declined' }));
    if (ev.stopped) facts.push(el('span', { 'class': 'ad-warn', text: ' stopped' }));
    var what = [el('div', { 'class': 'ad-words', text: ev.prompt || '' }), el('div', {}, facts)];
    if (ev.failure) what.push(el('div', { 'class': 'ad-bad', text: 'Failed: ' + ev.failure }));
    if (ev.results.length) {
      what.push(el('div', { 'class': 'ad-forms ad-mono' }, ev.results.map(function (r) {
        var said = r.ok ? r.value : r.error;
        return el('div', { 'class': 'ad-form' + (r.ok ? '' : ' ad-failed') }, [
          r.form || '',
          said ? el('span', { 'class': 'ad-value', text: '  → ' + said }) : null]);
      })));
    }
    if (ev.reply) what.push(el('div', { 'class': 'ad-reply', text: ev.reply }));
    if (ev.pictures.length) {
      what.push(pictures(ev.pictures.map(function (name) {
        var look = /^look-(\d+)$/.exec(name);
        return { src: '/stratum/api/admin/picture?turn=' + encodeURIComponent(ev.turn) + '&name=' + encodeURIComponent(name),
                 label: name, title: look ? (ev.looks[Number(look[1]) - 1] || name) : name };
      })));
    }
    if (ev.state) what.push(more('the document as the planner was told it', ev.state));
    return eventRow(ev, ev.byHand ? 'generate' : 'prompt', ev.byHand ? 'hand' : 'model', what);
  }

  function inviteFacts(v) {
    var facts = [];
    if (v.invited) {
      facts.push(fact('invite issued', v.issued || '?'));
      facts.push(fact('uses since the server started', v.uses === null ? 0 : v.uses));
      facts.push(fact('link last used', v.seen ? when(v.seen) : 'not since the server started'));
    } else {
      facts.push(el('span', { text: v.name ? 'No invite in this name now (revoked, or from another roster).' : 'Requests made with the gate open.' }));
    }
    if (v.first) facts.push(fact('first recorded', when(v.first)));
    if (v.downloads) facts.push(fact('downloads', v.downloads));
    return el('div', { 'class': 'ad-facts' }, facts);
  }

  function sessionHead(v, id) {
    var known = v.sessions.filter(function (s) { return s.id === id; })[0];
    var kids = [el('b', { 'class': 'ad-mono', title: id, text: 'Session ' + shortId(id) })];
    if (live[id]) kids.push(' ', pill('in memory', 'live'));
    if (known) {
      kids.push(el('span', { 'class': 'ad-dim', text: '  ' + count(known.requests, 'request') + ', ' + count(known.prompts, 'prompt') +
                             (known.documents.length ? ' · ' + known.documents.join(', ') : '') }));
    }
    return el('div', { 'class': 'ad-session' }, kids);
  }

  function loadVisitor(v, body) {
    var limit = limits[v.key] || EVENTS;
    body.textContent = 'Loading…';
    get('/stratum/api/admin/visitor?who=' + encodeURIComponent(v.key) + '&limit=' + limit).then(function (res) {
      var session = null;
      body.textContent = '';
      body.appendChild(inviteFacts(v));
      if (res.total > res.events.length) {
        var all = el('button', { text: 'Show all' });
        all.addEventListener('click', function () { limits[v.key] = 20000; loadVisitor(v, body); });
        body.appendChild(el('p', { 'class': 'ad-dim' }, ['The last ' + res.events.length + ' of ' + res.total + ' recorded.  ', all]));
      }
      if (!res.events.length) body.appendChild(el('p', { 'class': 'ad-dim', text: 'Nothing recorded yet.' }));
      res.events.forEach(function (ev) {
        // a line with no session (the page itself) stays with the one before
        if (ev.session && ev.session !== session) {
          session = ev.session;
          body.appendChild(sessionHead(v, session));
        }
        body.appendChild(ev.kind === 'prompt' ? promptRow(ev) : lineRow(v, ev));
      });
    }).catch(function (e) {
      body.textContent = 'Could not load this visitor: ' + e.message;
    });
  }

  function visitorCard(v) {
    var body = el('div', { 'class': 'ad-body' });
    var head = [el('span', { 'class': 'ad-name', text: v.name || 'Open gate (no invite)' })];
    if (!v.invited && v.name) head.push(pill('no invite now'));
    head.push(el('span', { 'class': 'ad-dim', text: [count(v.visits, 'visit'), count(v.prompts, 'prompt'),
                                                     count(v.changes, 'change') + ' by hand', count(v.uploads, 'upload'),
                                                     count(v.sessions.length, 'session')].join(' · ') }));
    head.push(el('span', { 'class': 'ad-dim', text: v.last ? 'last ' + ago(v.last) : 'nothing recorded' }));
    var card = el('details', { 'class': 'ad-card' }, [el('summary', {}, head), body]);
    var shown = false;
    function show() {
      opened[v.key] = card.open;
      if (card.open && !shown) { shown = true; loadVisitor(v, body); }
    }
    card.addEventListener('toggle', show);
    if (opened[v.key]) { card.open = true; show(); }
    return card;
  }

  // ---- the sessions in memory -------------------------------------------------

  function layerTable(doc) {
    var rows = [el('tr', {}, ['layer', 'kind', 'blend', 'opacity', 'mask'].map(function (h) { return el('th', { text: h }); }))];
    doc.layers.slice().reverse().forEach(function (layer) {   // the top of the stack first
      rows.push(el('tr', layer.visible ? {} : { 'class': 'ad-hidden', title: 'hidden' }, [
        el('td', { text: layer.name + (layer.visible ? '' : ' (hidden)') }),
        el('td', { text: layer.kind }),
        el('td', { text: layer.blend || '' }),
        el('td', { text: Math.round(layer.opacity * 100) + '%' }),
        el('td', { text: layer.mask ? 'mask' : '' })]));
    });
    return el('div', { 'class': 'ad-scroll' }, [el('table', { 'class': 'ad-layers' }, rows)]);
  }

  function sessionCard(s) {
    var head = [el('span', { 'class': 'ad-name ad-mono', title: s.id || '', text: s.id ? shortId(s.id) : 'Shared documents' })];
    if (s.who) head.push(el('span', { text: s.who }));
    else if (s.id) head.push(el('span', { 'class': 'ad-dim', text: 'nobody on record' }));
    head.push(el('span', { 'class': 'ad-dim', text: s.busy ? 'busy with a request just now' : count(s.documents.length, 'document') }));
    head.push(el('span', { 'class': 'ad-dim', text: s.id ? 'touched ' + ago(s.touched) : 'no session id: the API by hand' }));
    var body = el('div', { 'class': 'ad-body' });
    if (!s.documents.length) body.appendChild(el('p', { 'class': 'ad-dim', text: s.busy ? 'Refresh to see its documents.' : 'No documents.' }));
    s.documents.forEach(function (doc) {
      if (doc.error) {
        body.appendChild(el('p', { 'class': 'ad-bad', text: doc.name + ': ' + doc.error }));
        return;
      }
      body.appendChild(el('div', {}, [
        el('b', { text: doc.name }),
        el('span', { 'class': 'ad-dim', text: '  ' + doc.width + ' × ' + doc.height + ', ' + doc.mode + ', ' +
                     count(doc.layers.length, 'layer') + ', undo ' + doc.undo + ', ' + count(doc.prompts, 'prompt') })]));
      body.appendChild(layerTable(doc));
    });
    var card = el('details', { 'class': 'ad-card' }, [el('summary', {}, head), body]);
    var key = 'session ' + (s.id || '');
    card.addEventListener('toggle', function () { opened[key] = card.open; });
    if (opened[key]) card.open = true;
    return card;
  }

  // ---- the page ---------------------------------------------------------------

  function render(res) {
    var refresh = el('button', { text: 'Refresh' });
    refresh.addEventListener('click', load);
    var server = res.server;
    live = {};
    res.sessions.forEach(function (s) { if (s.id) live[s.id] = true; });
    root.textContent = '';
    root.appendChild(el('div', { 'class': 'ad-head' }, [
      el('h1', { text: 'Stratum admin' }), el('a', { href: '/stratum/', text: 'the editor' }), refresh]));
    root.appendChild(el('div', { 'class': 'ad-facts' }, [
      fact('as of', when(res.now)),
      fact('Lisp', server.lisp),
      server.heapUsed === null ? null : fact('heap', Math.round(server.heapUsed * 100) + '%'),
      fact('gate', server.gate ? 'invites only' : 'open', server.gate ? '' : 'warn'),
      fact('activity log', server.activityLog ? 'on' : 'off', server.activityLog ? '' : 'warn'),
      fact('prompt log', server.promptLog ? server.promptsKept + ' kept of ' + server.promptLimit : 'off', server.promptLog ? '' : 'warn')]));
    if (!server.activityLog) {
      root.appendChild(el('p', { 'class': 'ad-warn', text: 'The activity log is off: visits, uploads and requests made by hand are not being recorded.' }));
    }
    if (!server.promptLog) {
      root.appendChild(el('p', { 'class': 'ad-warn', text: 'The prompt log is off: prompts, their forms and their pictures are not being recorded.' }));
    }
    root.appendChild(el('h2', { text: 'Visitors (' + res.visitors.length + ')' }));
    if (!res.visitors.length) root.appendChild(el('p', { 'class': 'ad-dim', text: 'Nobody on the roster and nothing recorded.' }));
    res.visitors.forEach(function (v) { root.appendChild(visitorCard(v)); });
    root.appendChild(el('h2', { text: 'Sessions in memory (' + res.sessions.length + ')' }));
    root.appendChild(el('p', { 'class': 'ad-dim', text: 'A session is forgotten after ' + Math.round(server.idleSeconds / 3600) +
                            ' hours idle, or sooner when memory runs short.  Looking here does not keep one alive.' }));
    res.sessions.forEach(function (s) { root.appendChild(sessionCard(s)); });
  }

  function load() {
    get('/stratum/api/admin').then(render).catch(function (e) {
      root.textContent = 'Could not load the summary: ' + e.message;
    });
  }

  load();
})();
