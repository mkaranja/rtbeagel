/* global Shiny */
  
  /* ============================================================================
  zebra_print.js  -  browser -> USB Zebra ZT411

Bundled by Rhino: app/js/index.js imports this, `rhino::build_js()` runs
webpack over it, and the output lands in app/static/js/app.min.js which
Rhino includes automatically. Nothing here is loaded by a <script> tag.

WHY THIS IS CLIENT-SIDE AT ALL
The app deploys to shinyapps.io, so the R process runs in Amazon's cloud
     and has no route to a USB cable on a bench PC. Every server-side approach
     - system("lp"), writing /dev/usb/lp0, a socket to port 9100 - addresses a
     machine in a data centre. Only the browser can reach localhost.

         shinyapps.io --(ZPL over websocket)--> browser
         browser --(https://localhost:9101)--> Browser Print --USB--> ZT411

   MIXED CONTENT
     shinyapps.io is https, and an https page may not call http://. Zebra
     Browser Print listens on http 9100 AND https 9101, installing a trusted
     localhost certificate for the latter. 9101 is therefore tried first and is
     the only one that can work in production; 9100 is tried second because it
     is what a local `shiny::runApp()` over http will find in development.
   ========================================================================== */

const BASES = ['https://localhost:9101', 'http://localhost:9100'];

let base = null;      // resolved endpoint
let device = null;    // chosen printer
let probing = false;

// AbortController rather than the browser's default timeout: when Browser
// Print is not installed the connection can hang, and a hanging print button
// is indistinguishable from a broken one.
function withTimeout(url, options, ms) {
  const ctl = new AbortController();
  const timer = setTimeout(() => ctl.abort(), ms);
  return fetch(url, { ...options, signal: ctl.signal })
  .finally(() => clearTimeout(timer));
}

function status(state, message, printer) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue('rtb_print_status', {
      state,                       // ready | printing | done | unavailable | error
      message: message || '',
      printer: printer || (device ? device.name || device.uid : null),
      n: Math.random()
    }, { priority: 'event' });
  }
}

async function tryBase(url) {
  const res = await withTimeout(`${url}/available`, { method: 'GET' }, 4000);
  if (!res.ok) throw new Error(`HTTP ${res.status}`);
  return res.json();
}

async function discover() {
  if (probing) return !!device;
  probing = true;
  try {
    for (const candidate of BASES) {
      let list;
      try {
        list = await tryBase(candidate);
      } catch (e) {
        continue;
      }
      base = candidate;
      // `printer` is the documented key; some builds return `device`.
      const devices = (list && (list.printer || list.device)) || [];
      if (!devices.length) {
        status('unavailable', 'Browser Print is running but no printer is connected.');
        return false;
      }
      // Prefer something that looks like the ZT411, else take the first. A lab
      // may have a desktop label printer sitting alongside it.
      device = devices.find(
        (d) => /zt4?11/i.test(`${d.name || ''}${d.deviceType || ''}`)
      ) || devices[0];
      status('ready', 'Connected', device.name || device.uid);
      return true;
    }
    status('unavailable', 'Zebra Browser Print is not running on this computer.');
    return false;
  } finally {
    probing = false;
  }
}

function write(zpl) {
  return withTimeout(`${base}/write`, {
    method: 'POST',
    headers: { 'Content-Type': 'text/plain;charset=UTF-8' },
    body: JSON.stringify({ device, data: zpl })
  }, 15000);
}

async function sendJob(msg) {
  status('printing', `Sending ${msg.labels || 1} label(s)...`);
  const res = await write(msg.zpl);
  if (!res.ok) throw new Error(`Printer returned HTTP ${res.status}`);
  status('done', `${msg.labels || 1} label(s) sent to the printer.`);
}

async function handlePrint(msg) {
  if (!msg || !msg.zpl) {
    status('error', 'Nothing to print.');
    return;
  }
  if (!device || !base) {
    const ok = await discover();
    if (!ok) return;
  }
  try {
    await sendJob(msg);
  } catch (first) {
    // A device can vanish between discovery and printing - somebody unplugs
    // it, or Browser Print restarts. Re-discover once and retry before giving
    // up, rather than reporting it broken when it has only moved.
    device = null;
    base = null;
    const ok = await discover();
    if (!ok) return;
    try {
      await sendJob(msg);
    } catch (second) {
      status('error', second.message || first.message || 'Print failed.');
    }
  }
}

function register() {
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
  Shiny.addCustomMessageHandler('rtb_print', handlePrint);
  Shiny.addCustomMessageHandler('rtb_print_probe', () => {
    device = null;
    base = null;
    discover();
  });
  // Probe once so the button shows its true state before the first click,
  // rather than looking available and then failing.
  setTimeout(discover, 800);
}

export default function initZebraPrint() {
  // The bundle can evaluate before Shiny has initialised. Registering a
  // handler on an undefined Shiny fails silently and the print button then
  // does nothing forever, so wait for the connection if it has not happened.
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    register();
  } else {
    document.addEventListener('shiny:connected', register, { once: true });
  }
}