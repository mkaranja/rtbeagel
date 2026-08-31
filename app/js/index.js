// Rhino JS entry point.
//
// `rhino::build_js()` runs webpack over this file and writes the result to
// app/static/js/app.min.js, which Rhino includes in the page automatically.
// Anything imported here ends up in that one bundle.
//
// Keep this file a list of initialisers. Logic belongs in its own module, so
// each piece can be linted, replaced and reasoned about on its own.

import initZebraPrint from './zebra_print.js';

initZebraPrint();