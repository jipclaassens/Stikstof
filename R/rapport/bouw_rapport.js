// Voegt de JSON-uitvoer van 06_rapport.R en 07_plannen.R in de HTML-bron
// en schrijft het rapport weg.
//   node R/rapport/bouw_rapport.js 20260730
const fs = require("fs"), path = require("path");
const extract = process.argv[2] || "20260730";
const here = __dirname;
const src = fs.readFileSync(path.join(here, "rapport_stikstof_src.html"), "utf8");
const lees = (naam) => {
  const t = fs.readFileSync(path.join(here, "..", "_output", `${naam}_${extract}.json`), "utf8").replace(/<\/script/gi, "<\\/script");
  JSON.parse(t);
  return t;
};
const data = lees("rapport");
let plannen = "null";
try { plannen = lees("plannen"); } catch (e) { console.warn("geen plannen-JSON gevonden, plannenspoor blijft leeg"); }
const out = path.join(here, "..", "_output", `rapport_stikstof_${extract}.html`);
fs.writeFileSync(out, src.replace("__DATA__", () => data).replace("__PLANNEN__", () => plannen));
console.log("geschreven:", out);
