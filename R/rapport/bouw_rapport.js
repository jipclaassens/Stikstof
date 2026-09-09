// Voegt de JSON-uitvoer van 06_rapport.R en 07_plannen.R in de HTML-bron,
// plus versie- en bouwinformatie, en schrijft het rapport weg.
//   node R/rapport/bouw_rapport.js 20260730
const fs = require("fs"), path = require("path"), cp = require("child_process");

// Verhoog dit bij elke inhoudelijke wijziging van het rapport.
const VERSIE = "1.6";

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
                         .replace("__BUILD__", () => JSON.stringify(build)));
console.log(`geschreven: ${out}  (versie ${VERSIE}${git ? ", commit " + git : ""}, ${build.gebouwd})`);
