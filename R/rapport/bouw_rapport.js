// Voegt de JSON-uitvoer van 06_rapport.R, 07_plannen.R, 08_strikt.R en
// 09_aanvragen.R tot en met 26_samenstelling.R in de HTML-bron, plus versie- en
// bouwinformatie, en schrijft het rapport weg.
//   node R/rapport/bouw_rapport.js 20260730
const fs = require("fs"), path = require("path"), cp = require("child_process");

// Verhoog dit bij elke inhoudelijke wijziging van het rapport.
const VERSIE = "3.6";

const extract = process.argv[2] || "20260730";
const here = __dirname;
const src = fs.readFileSync(path.join(here, "rapport_stikstof_src.html"), "utf8");

const lees = (naam) => {
  const t = fs.readFileSync(path.join(here, "..", "_output", `${naam}_${extract}.json`), "utf8")
              .replace(/<\/script/gi, "<\\/script");
  JSON.parse(t);
  return t;
};

const data = lees("rapport");
let plannen = "null";
try { plannen = lees("plannen"); } catch (e) { console.warn("geen plannen-JSON gevonden, plannenspoor blijft leeg"); }
let strikt = "null";
try { strikt = lees("strikt"); } catch (e) { console.warn("geen strikt-JSON gevonden, hoofdstuk over de strenge vergelijking blijft leeg"); }
// 09 tot en met 23: aanvragen, landelijk tempo, ruimtelijke toetsen, beroep, binnenste ring, netcongestie, natuurbesluiten, kernen, registratie, NNN, ringen, plannen in Noord-Holland, verdichting, de lange reeks, de landelijke plannen, stad en land, de projectomvang, de uitgebreide procedure, de bestemmingsplannen in heel Nederland
const extra = {};
for (const naam of ["aanvragen", "landelijk", "ruimte", "beroep", "binnenring", "netcongestie", "natuurbesluiten", "kernen", "registratie", "nnn", "ringen", "plannen_nh", "verdichting", "langereeks", "plannen_nl", "samenstelling", "omvang", "uitgebreid", "plannen_natuur"]) {
  extra[naam] = "null";
  try { extra[naam] = lees(naam); } catch (e) { console.warn(`geen ${naam}-JSON gevonden, dat hoofdstuk blijft leeg`); }
}

let git = null;
try { git = cp.execSync("git rev-parse --short HEAD", { cwd: here, stdio: ["ignore", "pipe", "ignore"] }).toString().trim(); } catch (e) {}

const nu = new Date();
const pad = n => String(n).padStart(2, "0");
const build = {
  versie: VERSIE,
  extract,
  git,
  gebouwd: `${nu.getFullYear()}-${pad(nu.getMonth() + 1)}-${pad(nu.getDate())} ${pad(nu.getHours())}:${pad(nu.getMinutes())}`
};

const out = path.join(here, "..", "_output", `rapport_stikstof_${extract}.html`);
fs.writeFileSync(out, src.replace("__DATA__", () => data)
                         .replace("__PLANNEN__", () => plannen)
                         .replace("__STRIKT__", () => strikt)
                         .replace("__AANVRAGEN__", () => extra.aanvragen)
                         .replace("__LANDELIJK__", () => extra.landelijk)
                         .replace("__RUIMTE__", () => extra.ruimte)
                         .replace("__BEROEP__", () => extra.beroep)
                         .replace("__BINNENRING__", () => extra.binnenring)
                         .replace("__NETCONGESTIE__", () => extra.netcongestie)
                         .replace("__NATUURBESLUITEN__", () => extra.natuurbesluiten)
                         .replace("__KERNEN__", () => extra.kernen)
                         .replace("__REGISTRATIE__", () => extra.registratie)
                         .replace("__NNN__", () => extra.nnn)
                         .replace("__RINGEN__", () => extra.ringen)
                         .replace("__PLANNEN_NH__", () => extra.plannen_nh)
                         .replace("__VERDICHTING__", () => extra.verdichting)
                         .replace("__LANGEREEKS__", () => extra.langereeks)
                         .replace("__PLANNEN_NL__", () => extra.plannen_nl)
                         .replace("__SAMENSTELLING__", () => extra.samenstelling)
                         .replace("__OMVANG__", () => extra.omvang)
                         .replace("__UITGEBREID__", () => extra.uitgebreid)
                         .replace("__PLANNEN_NATUUR__", () => extra.plannen_natuur)
                         .replace("__BUILD__", () => JSON.stringify(build)));
console.log(`geschreven: ${out}  (versie ${VERSIE}${git ? ", commit " + git : ""}, ${build.gebouwd})`);
