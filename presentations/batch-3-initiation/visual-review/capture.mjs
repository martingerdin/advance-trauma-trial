import {spawn} from 'node:child_process';
import {createServer} from 'node:http';
import {readFile,writeFile,mkdtemp,rm} from 'node:fs/promises';
import {resolve,extname,join} from 'node:path';
import {tmpdir} from 'node:os';
const delay=ms=>new Promise(r=>setTimeout(r,ms));
const root=resolve('dist');
const server=createServer(async(req,res)=>{try{const p=resolve(root,'.'+new URL(req.url,'http://localhost').pathname.replace(/^\/$/,'/index.html'));if(!p.startsWith(root+'/'))throw Error();res.setHeader('Content-Type',({'.html':'text/html','.js':'text/javascript','.css':'text/css','.woff2':'font/woff2','.png':'image/png','.svg':'image/svg+xml'})[extname(p)]||'application/octet-stream');res.end(await readFile(p));}catch{res.writeHead(404);res.end();}});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const profile=await mkdtemp(join(tmpdir(),'slide-review-'));
const browser=spawn('/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge',['--headless=new','--disable-gpu','--no-first-run','--remote-debugging-port=0','--remote-allow-origins=*','--user-data-dir='+profile,'about:blank'],{stdio:['ignore','ignore','pipe']});
let ws;
try{
const endpoint=await new Promise((r,j)=>{let log='';browser.stderr.on('data',c=>{log+=c;const m=log.match(/DevTools listening on (ws:\/\/\S+)/);if(m)r(m[1]);});setTimeout(()=>j(Error('Startup timeout')),15000).unref();});
ws=new WebSocket(endpoint);await new Promise(r=>ws.addEventListener('open',r));let id=0;const pending=new Map();ws.addEventListener('message',e=>{const m=JSON.parse(e.data),p=pending.get(m.id);if(p){pending.delete(m.id);m.error?p.reject(m.error):p.resolve(m.result);}});
const call=(method,params={},sessionId)=>new Promise((resolve,reject)=>{pending.set(++id,{resolve,reject});ws.send(JSON.stringify({id,method,params,sessionId}));});
const {targetId}=await call('Target.createTarget',{url:'about:blank'});const {sessionId}=await call('Target.attachToTarget',{targetId,flatten:true});const send=(m,p)=>call(m,p,sessionId);
const evaluate=async expression=>{const r=await send('Runtime.evaluate',{expression,awaitPromise:true,returnByValue:true});if(r.exceptionDetails)throw Error(JSON.stringify(r.exceptionDetails));return r.result.value;};
await send('Page.enable');await send('Emulation.setDeviceMetricsOverride',{width:1280,height:720,deviceScaleFactor:1,mobile:false});await send('Emulation.setEmulatedMedia',{features:[{name:'prefers-reduced-motion',value:'reduce'}]});
const results=[];
const capture=async name=>{await evaluate('document.fonts.ready');await delay(400);const {data}=await send('Page.captureScreenshot',{format:'png'});await writeFile(resolve('visual-review',name+'.png'),Buffer.from(data,'base64'));results.push(await evaluate(`(()=>{const s=document.querySelector('.slide.is-active');return {name:${JSON.stringify(name)},overflow:[s.scrollWidth-s.clientWidth,s.scrollHeight-s.clientHeight]}})()`));};
for(const slide of ['title','background','aim','design','batch-timeline','intervention','training-selection','training-distributed','training-selection-emphasis','eligibility','sample-size','data-collection','consent','safety','closing']){await send('Page.navigate',{url:'http://127.0.0.1:'+server.address().port+'/#'+slide});await delay(1400);await capture(slide);}
await send('Emulation.setEmulatedMedia',{features:[{name:'prefers-reduced-motion',value:'no-preference'}]});
await send('Page.navigate',{url:'http://127.0.0.1:'+server.address().port+'/#training-selection'});await delay(1400);
const brandingSamples=await evaluate(`new Promise(resolve=>{const samples=[];const brand=document.querySelector('#trial-branding');const start=performance.now();document.querySelector('#next').click();function sample(){const style=getComputedStyle(brand);samples.push({hidden:brand.hidden,display:style.display,opacity:style.opacity,y:brand.getBoundingClientRect().y});if(performance.now()-start<1000)requestAnimationFrame(sample);else resolve(samples);}sample();})`);
if(brandingSamples.some(s=>s.hidden||s.display==='none'||s.opacity!=='1'||Math.abs(s.y-brandingSamples[0].y)>0.1))throw Error('Branding changed during slide transition: '+JSON.stringify(brandingSamples));
console.log('Verified persistent branding across '+brandingSamples.length+' slide-transition frames.');
await send('Page.navigate',{url:'http://127.0.0.1:'+server.address().port+'/#design'});await delay(800);
for(let n=0;n<130;n++){if(await evaluate('document.querySelector(".wedge-play-pause")?.dataset.state === "paused"'))break;await delay(100);}
await capture('design-site');await evaluate('document.querySelector("[data-stage=batch]").click()');
let capturedRunning=false;
const gaps=[];
for(let n=0;n<150;n++){
  const state=await evaluate(`(()=>{const svg=document.querySelector('.stepped-wedge-chart');const tick=svg.querySelector('.wedge-axis-tick-label');const title=svg.querySelector('.wedge-axis-xtitle');const cluster=svg.querySelector('.wedge-cluster-label');const yTitle=svg.querySelector('.wedge-axis-ylabel');const scale=svg.getScreenCTM().a;return {stage:svg.dataset.stage,timelineOpacity:Number(svg.querySelector('.wedge-axis-timeline').getAttribute('opacity')),play:document.querySelector('.wedge-play-pause').dataset.state,xGap:(Number(title.getAttribute('y'))-Number(tick.getAttribute('y')))*scale,yGap:(cluster.getBBox().x-Number(yTitle.getAttribute('x')))*scale};})()`);
  if(state.stage==='batch'){
    gaps.push(state);
    if(!capturedRunning && state.play==='playing' && state.timelineOpacity===1){await capture('design-batch-running');capturedRunning=true;}
    if(state.play==='paused')break;
  }
  await delay(100);
}
await capture('design-batch');
if(!capturedRunning)throw Error('Did not capture the running five-hospital reveal');
const incorrect=gaps.filter(s=>Math.abs(s.xGap-32)>0.1||Math.abs(s.yGap-16)>0.1);
if(incorrect.length)throw Error('Axis gaps changed during animation: '+JSON.stringify(incorrect));
console.log('Verified stable axis spacing across '+gaps.length+' running/paused five-hospital samples.');
const chartLayout=await evaluate(`(()=>{const rect=document.querySelector('.wedge-chart-stack').getBoundingClientRect();return {top:rect.top,height:rect.height};})()`);
await evaluate('document.querySelector("[data-stage=full]").click()');
const fullMonths=[];
for(let n=0;n<150;n++){
  const state=await evaluate(`(()=>{const svg=document.querySelector('.stepped-wedge-chart');const rect=document.querySelector('.wedge-chart-stack').getBoundingClientRect();return {stage:svg.dataset.stage,month:Number(svg.dataset.revealMonth),play:document.querySelector('.wedge-play-pause').dataset.state,top:rect.top,height:rect.height};})()`);
  if(Math.abs(state.top-chartLayout.top)>0.1||Math.abs(state.height-chartLayout.height)>0.1)throw Error('Chart container jumped entering full trial: '+JSON.stringify({chartLayout,state}));
  if(state.stage==='full'&&state.play==='playing')fullMonths.push(state.month);
  if(state.stage==='full'&&state.play==='complete')break;
  await delay(100);
}
if(!fullMonths.some((month,i)=>i>0&&month>fullMonths[i-1]&&month<40))throw Error('Full trial button did not play a progressive reveal: '+JSON.stringify(fullMonths));
await capture('design-full-running-button');
console.log('Verified stage buttons play five-hospital and full-trial animations.');
await evaluate('document.querySelector("[data-stage=batch]").click()');
const reverseSamples=[];
for(let n=0;n<130;n++){
  const state=await evaluate(`(()=>{const svg=document.querySelector('.stepped-wedge-chart');const row=svg.querySelector('.wedge-row[data-cluster="1"] .wedge-segment');const tick=svg.querySelector('.wedge-axis-tick-label');return {stage:svg.dataset.stage,month:Number(svg.dataset.revealMonth),gap:tick.getBoundingClientRect().top-row.getBoundingClientRect().bottom,play:document.querySelector('.wedge-play-pause').dataset.state};})()`);
  if(state.month>0.05&&state.stage==='full')reverseSamples.push(state);
  if(n===5)await capture('design-full-reversing');
  if(state.stage==='batch'&&state.play==='paused')break;
  await delay(100);
}
if(reverseSamples.length<3||reverseSamples.some(s=>s.gap<5))throw Error('Axes overlap during full-trial reverse wipe: '+JSON.stringify(reverseSamples));
await capture('design-batch-after-reverse');
console.log('Verified full-trial axis clearance across '+reverseSamples.length+' reverse-wipe samples.');
console.log(JSON.stringify(results,null,2));
}finally{ws?.close();browser.kill();server.close();await delay(400);await rm(profile,{recursive:true,force:true});}
