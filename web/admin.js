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
// their documents' layers.  It reads, and deletes from the two logs what
// its operator asks it to, each time after asking back: one line, one
// prompt, or everything kept of a visitor.  Everything shown here that
// a visitor typed -- a prompt, a document's name, a request's body --
// goes into the page as text, never as markup.  The pieces it is built
// from (elements, fetching, times, pills, the delete button) are
// window.visitorAdmin, Gendl's visitor-log's, which the page loads first.

(function () {
  'use strict';

  var root = document.getElementById('admin');
  var opened = {};   // visitor key -> true while that visitor's card is open
  var limits = {};   // visitor key -> how many events to ask for
  var live = {};     // session id -> true for the sessions in memory
  var heads = {};    // visitor key -> the head of that visitor's card
  var facts = null;  // the line about the server, under the title
  var EVENTS = 300;

  // What every operator's page is built from, served by
  // /stratum/admin-kit.js (Gendl's visitor-log).
  var A = window.visitorAdmin;
  var el = A.el, fill = A.fill, get = A.get, when = A.when, ago = A.ago, count = A.count,
      pill = A.pill, fact = A.fact, more = A.more, shortId = A.shortId;

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
    return A.eventRow(ev.time, label, kind, what);
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
    if (ev.kind === 'note') {
      // Help > Send a Note: the visitor's own words, first of all
      var said = [el('span', { 'class': 'ad-dim', text: 'Sent a note' }), doc,
                  el('div', { 'class': 'ad-words', text: ev.note || '' })];
      if (ev.file) {
        var shot = '/stratum/api/admin/upload?who=' + encodeURIComponent(v.key) + '&file=' + encodeURIComponent(ev.file);
        said.push(pictures([{ src: shot, full: shot + '&size=1280', label: 'the picture as it was' }]));
      }
      return eventRow(ev, 'note', 'note', said);
    }
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

  // ---- deleting ---------------------------------------------------------------

  function whose(v) {
    return v.name || 'the open gate';
  }

  // A button that deletes what BODY names once QUESTION is answered yes,
  // since nothing brings a record back; THEN runs when it is gone.
  function remover(label, kind, question, body, then) {
    return A.remover('/stratum/api/admin/delete', label, kind, question, body, then);
  }

  function entryRemover(v, ev, then) {
    if (ev.kind === 'prompt') {
      return remover('Delete', 'del', 'Delete this prompt of ' + whose(v) + ', with its forms and its pictures?  It cannot be brought back.',
                     { who: v.key, turn: ev.turn }, then);
    }
    var what = ev.kind === 'upload' ? 'upload, and the picture kept of it,'
      : ev.kind === 'note' ? 'note' + (ev.file ? ', and the picture kept with it,' : '') : 'line';
    return remover('Delete', 'del', 'Delete this ' + what +
                   ' from the record of ' + whose(v) + '?  It cannot be brought back.',
                   { who: v.key, line: ev.line, mark: ev.mark }, then);
  }

  function everythingRemover(v, total) {
    return remover('Delete everything kept of ' + whose(v), 'danger',
                   'Delete everything kept of ' + whose(v) + ': ' + count(total, 'line or prompt', 'lines and prompts') +
                   ', with every picture kept with them?  ' + (v.invited ? 'Their invite stays.  ' : '') + 'It cannot be brought back.',
                   { who: v.key, everything: true },
                   function () { opened[v.key] = false; load(); });
  }

  // After one thing of V's has gone: the counts on their card and under
  // the title, and their record again, all in place, so that the page
  // stays where it was for the next one.
  function recount(v, body) {
    get('/stratum/api/admin').then(function (res) {
      var now = res.visitors.filter(function (x) { return x.key === v.key; })[0];
      if (!now || !heads[v.key] || !facts) { render(res); return; }
      Object.keys(now).forEach(function (k) { v[k] = now[k]; });
      fill(heads[v.key], visitorHead(v));
      fill(facts, serverFacts(res));
      loadVisitor(v, body);
    }).catch(load);
  }

  function loadVisitor(v, body) {
    var limit = limits[v.key] || EVENTS;
    if (!body.firstChild) body.textContent = 'Loading…';
    get('/stratum/api/admin/visitor?who=' + encodeURIComponent(v.key) + '&limit=' + limit).then(function (res) {
      var session = null;
      body.textContent = '';
      body.appendChild(inviteFacts(v));
      if (res.total) body.appendChild(el('p', {}, [everythingRemover(v, res.total)]));
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
        var row = ev.kind === 'prompt' ? promptRow(ev) : lineRow(v, ev);
        row.appendChild(entryRemover(v, ev, function () { recount(v, body); }));
        body.appendChild(row);
      });
    }).catch(function (e) {
      body.textContent = 'Could not load this visitor: ' + e.message;
    });
  }

  function visitorHead(v) {
    var head = [el('span', { 'class': 'ad-name', text: v.name || 'Open gate (no invite)' })];
    if (!v.invited && v.name) head.push(pill('no invite now'));
    if (v.notes) head.push(pill(count(v.notes, 'note'), 'note'));
    head.push(el('span', { 'class': 'ad-dim', text: [count(v.visits, 'visit'), count(v.prompts, 'prompt'),
                                                     count(v.changes, 'change') + ' by hand', count(v.uploads, 'upload'),
                                                     count(v.sessions.length, 'session')].join(' · ') }));
    head.push(el('span', { 'class': 'ad-dim', text: v.last ? 'last ' + ago(v.last) : 'nothing recorded' }));
    return head;
  }

  function visitorCard(v) {
    var body = el('div', { 'class': 'ad-body' });
    var card = el('details', { 'class': 'ad-card' }, [heads[v.key] = el('summary', {}, visitorHead(v)), body]);
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
    return el('div', { 'class': 'ad-scroll' }, [el('table', { 'class': 'ad-table' }, rows)]);
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

  function serverFacts(res) {
    var server = res.server;
    return [
      fact('as of', when(res.now)),
      fact('Lisp', server.lisp),
      server.heapUsed === null ? null : fact('heap', Math.round(server.heapUsed * 100) + '%'),
      fact('gate', server.gate ? 'invites only' : 'open', server.gate ? '' : 'warn'),
      fact('activity log', server.activityLog ? 'on' : 'off', server.activityLog ? '' : 'warn'),
      fact('prompt log', server.promptLog ? server.promptsKept + ' kept of ' + server.promptLimit : 'off', server.promptLog ? '' : 'warn')];
  }

  function render(res) {
    var refresh = el('button', { text: 'Refresh' });
    refresh.addEventListener('click', load);
    var server = res.server;
    live = {};
    heads = {};
    res.sessions.forEach(function (s) { if (s.id) live[s.id] = true; });
    root.textContent = '';
    root.appendChild(el('div', { 'class': 'ad-head' }, [
      el('h1', { text: 'Stratum admin' }), el('a', { href: '/stratum/', text: 'the editor' }), refresh]));
    root.appendChild(facts = el('div', { 'class': 'ad-facts' }, serverFacts(res)));
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
