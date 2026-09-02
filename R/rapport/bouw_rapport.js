// Voegt _output/rapport_<extract>.json in de HTML-bron en schrijft het rapport weg.
//   node R/rapport/bouw_rapport.js 20260730
const fs = require("fs"), path = require("path");
const extract = process.argv[2] || "20260730";
const here = __dirname;
const src = fs.readFileSync(path.join(here, "rapport_stikstof_src.html"), "utf8");
const data = fs.readFileSync(path.join(here, "..", "_output", `rapport_${extract}.json`), "utf8").replace(/<\/script/gi, "<\/script");
JSON.parse(data);
const out = path.join(here, "..", "_output", `rapport_stikstof_${extract}.html`);
fs.writeFileSync(out, src.replace("__DATA__", () => data));
console.log("geschreven:", out);
