import { spawn } from 'node:child_process';
import { createServer } from 'node:http';
import { readFile, writeFile, mkdtemp, rm } from 'node:fs/promises';
import { resolve, extname, join } from 'node:path';
import { tmpdir } from 'node:os';
import assert from 'node:assert/strict';

const delay = ms => new Promise(r => setTimeout(r, ms));
const root = resolve('dist');
const server = createServer(async (req, res) => {
  try {
    const path = resolve(root, '.' + new URL(req.url, 'http://localhost').pathname.replace(/^\/$/, '/index.html'));
    if (!path.startsWith(root + '/')) throw Error('Invalid path');
    res.setHeader('Content-Type', ({ '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.woff2': 'font/woff2', '.png': 'image/png', '.svg': 'image/svg+xml' })[extname(path)] || 'application/octet-stream');
    res.end(await readFile(path));
  } catch { res.writeHead(404); res.end(); }
});
await new Promise(r => server.listen(0, '127.0.0.1', r));
const profile = await mkdtemp(join(tmpdir(), 'standard-trial-review-'));
const browser = spawn('/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge', [
  '--headless=new', '--disable-gpu', '--no-first-run', '--remote-debugging-port=0',
  '--remote-allow-origins=*', '--user-data-dir=' + profile, 'about:blank',
], { stdio: ['ignore', 'ignore', 'pipe'] });
let ws;
try {
  const endpoint = await new Promise((r, j) => {
    let log = '';
    browser.stderr.on('data', c => { log += c; const m = log.match(/DevTools listening on (ws:\/\/\S+)/); if (m) r(m[1]); });
    setTimeout(() => j(Error('Browser startup timeout')), 15000).unref();
  });
  ws = new WebSocket(endpoint);
  await new Promise(r => ws.addEventListener('open', r));
  let id = 0;
  const pending = new Map();
  const errors = [];
  ws.addEventListener('message', e => {
    const m = JSON.parse(e.data), p = pending.get(m.id);
    if (p) { pending.delete(m.id); m.error ? p.reject(m.error) : p.resolve(m.result); }
    if (m.method === 'Runtime.exceptionThrown') errors.push(m.params.exceptionDetails);
  });
  const call = (method, params = {}, sessionId) => new Promise((resolve, reject) => {
    pending.set(++id, { resolve, reject }); ws.send(JSON.stringify({ id, method, params, sessionId }));
  });
  const { targetId } = await call('Target.createTarget', { url: 'about:blank' });
  const { sessionId } = await call('Target.attachToTarget', { targetId, flatten: true });
  const send = (m, p) => call(m, p, sessionId);
  const evaluate = async expression => {
    const r = await send('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
    if (r.exceptionDetails) throw Error(JSON.stringify(r.exceptionDetails));
    return r.result.value;
  };
  await send('Page.enable'); await send('Runtime.enable');
  await send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-reduced-motion', value: 'reduce' }] });
  const base = 'http://127.0.0.1:' + server.address().port;
  const navigate = async path => {
    await send('Page.navigate', { url: base + path }); await delay(650); await evaluate('document.fonts.ready'); await delay(100);
  };
  const results = [];
  for (const [width, height] of [[1280, 720], [1920, 1080]]) {
    await send('Emulation.setDeviceMetricsOverride', { width, height, deviceScaleFactor: 1, mobile: false });
    await navigate('/?view=full');
    const ids = await evaluate('Array.from(document.querySelectorAll(".slide"), s => s.id.slice(6))');
    assert.equal(ids.length, 29);
    for (const id of ids) {
      await evaluate(`location.hash = ${JSON.stringify(id)}`); await delay(700);
      const result = await evaluate(`(() => {
        const s = document.querySelector('.slide.is-active');
        const brand = document.querySelector('#trial-branding');
        const limit = brand.hidden ? innerHeight : brand.getBoundingClientRect().top;
        const content = s.querySelector('.slide-inner');
        const visible = [...content.children].filter(e => getComputedStyle(e).display !== 'none');
        const bottom = Math.max(...visible.map(e => e.getBoundingClientRect().bottom));
        return {id: s.id.slice(6), overflowX: s.scrollWidth-s.clientWidth, overflowY: s.scrollHeight-s.clientHeight,
          brandingCollision: bottom > limit + 1, bottom: Math.round(bottom), limit: Math.round(limit),
          brokenImages: [...s.querySelectorAll('img')].filter(i => !i.complete || !i.naturalWidth).length};
      })()`);
      results.push({ width, ...result });
      assert.equal(result.id, id);
      if (width === 1280) {
        const { data } = await send('Page.captureScreenshot', { format: 'png' });
        await writeFile(resolve('visual-review', id + '.png'), Buffer.from(data, 'base64'));
      }
    }
  }
  for (const [route, count] of [['/', 25], ['/?view=brief', 13], ['/?view=full', 29]]) {
    await navigate(route);
    assert.equal(await evaluate('document.querySelectorAll(".slide").length'), count);
    await evaluate('document.querySelector("#overview-toggle").click()');
    assert.equal(await evaluate('document.querySelector("#overview").hidden'), false);
    await evaluate('document.querySelector("#overview-close").click()'); await delay(500);
    assert.equal(await evaluate('document.querySelector("#overview").hidden'), true);
    await send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'ArrowRight', code: 'ArrowRight' }); await delay(700);
    assert.equal(await evaluate('location.hash'), '#background');
  }
  // Print from the title, without first visiting the animated design slide.
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 720, deviceScaleFactor: 1, mobile: false });
  await navigate('/');
  await send('Emulation.setEmulatedMedia', { media: 'print' });
  assert.equal(await evaluate('getComputedStyle(document.querySelector(".print-design")).display'), 'block');
  const printOverflow = await evaluate(`Array.from(document.querySelectorAll('.slide'), s => ({id:s.id, overflow:s.scrollHeight-s.clientHeight})).filter(s=>s.overflow>1)`);
  const { data: pdf } = await send('Page.printToPDF', { printBackground: true, preferCSSPageSize: true, displayHeaderFooter: false });
  await writeFile(resolve('standard-trial.pdf'), Buffer.from(pdf, 'base64'));
  await writeFile(resolve('visual-review/results.json'), JSON.stringify({ results, printOverflow, errors }, null, 2));
  console.log(JSON.stringify({ results: results.filter(r => r.overflowX || r.overflowY || r.brandingCollision || r.brokenImages), printOverflow, errors }, null, 2));
  assert.equal(errors.length, 0, 'Browser runtime errors');
  assert.ok(results.every(r => !r.overflowX && !r.overflowY && !r.brandingCollision && !r.brokenImages), 'Slide layout or asset errors');
  assert.equal(printOverflow.length, 0, 'Print overflow');
  console.log('PASS: 29 slides at 1280×720 and 1920×1080; 3 routes; overview; keyboard navigation; core PDF.');
} finally {
  ws?.close(); browser.kill(); server.close(); await delay(400); await rm(profile, { recursive: true, force: true });
}
