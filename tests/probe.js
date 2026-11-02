#!/usr/bin/env node
// Copyright © 2026 Genworks International
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.  Distributed WITHOUT
// ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
//
// Load a Stratum page in headless Chromium, wait for the viewer, run
// STRATUM.selfTest(), print its result as JSON and exit 0 when it
// passed, 1 when it failed, 2 when the page never got ready.
// Usage: node probe.js URL [screenshot.png]
// Talks to Chromium over the DevTools protocol with Node's own fetch
// and WebSocket (Node 22 or later); no packages.

const { spawn } = require('child_process');
const fs = require('fs');

const url = process.argv[2];
const shot = process.argv[3];
const bin = process.env.CHROMIUM || 'chromium';
const profile = `/tmp/stratum-probe-${process.pid}`;
const chrome = spawn(bin, ['--headless=new', '--enable-unsafe-swiftshader', '--no-sandbox',
  '--hide-scrollbars', '--remote-debugging-port=0', '--window-size=1440,900',
  `--user-data-dir=${profile}`, 'about:blank'], { stdio: 'ignore' });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function devtoolsPort() {
  for (let i = 0; i < 100; i++) {
    try {
      const p = Number(fs.readFileSync(`${profile}/DevToolsActivePort`, 'utf8').split('\n')[0]);
      if (p > 0) return p;
    } catch {}
    await sleep(100);
  }
  throw new Error('no DevTools port');
}

async function main() {
  const port = await devtoolsPort();
  let targets = [];
  for (let i = 0; i < 50 && !targets.length; i++) {
    try { targets = await (await fetch(`http://127.0.0.1:${port}/json`)).json(); } catch {}
    if (!targets.length) await sleep(200);
  }
  const ws = new WebSocket(targets[0].webSocketDebuggerUrl);
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = () => rej(new Error('DevTools socket')); });
  let id = 0;
  const pending = new Map();
  const errors = [];
  ws.onmessage = (m) => {
    const msg = JSON.parse(m.data);
    if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id); }
    else if (msg.method === 'Runtime.exceptionThrown') errors.push(msg.params.exceptionDetails.text);
  };
  const send = (method, params = {}) => new Promise((res) => {
    pending.set(++id, res);
    ws.send(JSON.stringify({ id, method, params }));
  });
  const evaluate = async (expression) => {
    const r = await send('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (r.result.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails).slice(0, 400));
    return r.result.result.value;
  };
  await send('Runtime.enable');
  await send('Page.enable');
  await send('Page.navigate', { url });
  let state = null;
  for (let i = 0; i < 900; i++) {
    await sleep(200);
    try { state = await evaluate('window.STRATUM && (STRATUM.ready ? "ready" : STRATUM.error || null)'); } catch {}
    if (state) break;
  }
  if (state !== 'ready') {
    console.log(JSON.stringify({ ok: false, error: state || 'viewer never became ready', errors }));
    process.exitCode = 2;
    return;
  }
  const result = await evaluate('STRATUM.selfTest()');
  if (shot) {
    const r = await send('Page.captureScreenshot', { format: 'png' });
    fs.writeFileSync(shot, Buffer.from(r.result.data, 'base64'));
  }
  result.pageErrors = errors;
  console.log(JSON.stringify(result, null, 1));
  process.exitCode = result.ok && !errors.length ? 0 : 1;
  ws.close();
}

main().catch((e) => { console.error('probe: ' + e.message); process.exitCode = 3; })
  .finally(() => {
    try { chrome.kill(); } catch {}
    try { fs.rmSync(profile, { recursive: true, force: true }); } catch {}
  });
