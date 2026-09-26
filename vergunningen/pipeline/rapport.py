"""Rapportgenerator: van resultaten.json (analyse.R) en de meetuitvoer naar een zelfstandige HTML-pagina.

Gebruik: py -3.13 rapport.py <analysemap> <teksten.md> <uitvoer.html>
De grafieken zijn SVG, getekend op schaal uit de getallen; de interpretatie komt uit teksten.md, met per sectie een
kop "## sleutel" en daaronder alinea's (lege regel ertussen). Sleutels: samenvatting, data, methode, aanvragen,
uitkomsten, gradient, beroep, beperkingen, vervolg. Ontbreekt een sleutel, dan blijft de sectie leeg.
"""
import csv
import html
import json
import math
import sys
from pathlib import Path

KLASSEN = ["0_250", "250_500", "500_1000", "1000_2500", "2500_5000", "5000_"]
KLASSE_LABEL = {"0_250": "0 tot 250 m", "250_500": "250 tot 500 m", "500_1000": "500 tot 1.000 m", "1000_2500": "1.000 tot 2.500 m",
                "2500_5000": "2.500 tot 5.000 m", "5000_": "verder dan 5 km", "onbekend": "onbekend"}
EVENTS = [("2022K4", "Porthos"), ("2024K1", "Omgevingswet"), ("2025K1", "intern salderen")]
EVENTS_BAG = [("2019K2", "PAS-uitspraak"), ("2021K3", "bouwvrijstelling")] + EVENTS


def nl(x, dec=0):
    if x is None or x == "" or (isinstance(x, float) and math.isnan(x)):
        return ""
    if dec == 0:
        return f"{int(round(x)):,}".replace(",", ".")
    s = f"{x:,.{dec}f}"
    return s.replace(",", "X").replace(".", ",").replace("X", ".")


def pct(x, dec=1):
    return "" if x is None or x == "" else nl(100 * float(x), dec) + "%"


def lees_csv(pad):
    with open(pad, encoding="utf-8") as f:
        return list(csv.DictReader(f, delimiter=";"))


def lees_teksten(pad):
    teksten, sleutel, buf = {}, None, []
    for regel in Path(pad).read_text(encoding="utf-8").splitlines() if Path(pad).exists() else []:
        if regel.startswith("## "):
            if sleutel:
                teksten[sleutel] = buf
            sleutel, buf = regel[3:].strip(), []
        else:
            buf.append(regel)
    if sleutel:
        teksten[sleutel] = buf
    uit = {}
    for k, regels in teksten.items():
        alineas, cur = [], []
        for r in regels + [""]:
            if r.strip() == "":
                if cur:
                    alineas.append(" ".join(cur))
                    cur = []
            else:
                cur.append(r.strip())
        uit[k] = "".join(f"<p>{html.escape(a)}</p>" for a in alineas)
    return uit


# ---------- SVG-hulpfuncties ----------

class Schaal:
    def __init__(self, d0, d1, r0, r1):
        self.d0, self.d1, self.r0, self.r1 = d0, d1, r0, r1

    def __call__(self, v):
        if self.d1 == self.d0:
            return (self.r0 + self.r1) / 2
        return self.r0 + (v - self.d0) / (self.d1 - self.d0) * (self.r1 - self.r0)


def ticks(lo, hi, n=5):
    if hi <= lo:
        return [lo]
    ruw = (hi - lo) / n
    mag = 10 ** math.floor(math.log10(ruw))
    for stap in (1, 2, 2.5, 5, 10):
        if ruw / mag <= stap:
            stap *= mag
            break
    start = math.floor(lo / stap) * stap
    uit = []
    v = start
    while v <= hi + 1e-9:
        if v >= lo - 1e-9:
            uit.append(round(v, 10))
        v += stap
    return uit


TELLER = {"figuur": 0, "tabel": 0}


def svg_kop(b, h, titel):
    TELLER["figuur"] += 1
    return (f'<figure class="grafiek"><figcaption>Figuur {TELLER["figuur"]}. {html.escape(titel)}</figcaption>'
            f'<svg viewBox="0 0 {b} {h}" role="img" aria-label="Figuur {TELLER["figuur"]}: {html.escape(titel)}" width="100%">')


def noot_svg(L, H, tekst, breedte=150):
    """Voetnoot onder een grafiek; langer dan een regel wordt op een woordgrens over twee regels gezet."""
    if len(tekst) <= breedte:
        return f'<text x="{L}" y="{H - 8}" class="noot">{html.escape(tekst)}</text>'
    knip = tekst.rfind(" ", 0, breedte)
    return (f'<text x="{L}" y="{H - 22}" class="noot">{html.escape(tekst[:knip])}</text>'
            f'<text x="{L}" y="{H - 8}" class="noot">{html.escape(tekst[knip + 1:])}</text>')


def svg_einde():
    return "</svg></figure>"


def grafiek_event_study(rijen, titel, ref="2022K3", events=EVENTS, stap=2, grootheid="aanvragen", per="kwartaal"):
    """Coefficienten van nabij x kwartaal (of jaar) met 95%-interval; referentieperiode op nul."""
    pts = []
    for r in rijen:
        term = r["term"]
        if "::" not in term:
            continue
        kw = term.split("::")[1].split(":")[0]
        est, se = float(r["schatting"]), float(r["se"])
        pts.append((kw, est, est - 1.96 * se, est + 1.96 * se))
    if not pts:
        return ""
    kws = sorted({p[0] for p in pts} | {ref})
    B, H, L, R, T, D = 960, 380, 64, 24, 28, 70
    lo = min(min(p[2] for p in pts), 0) * 1.1
    hi = max(max(p[3] for p in pts), 0) * 1.1
    x = Schaal(0, len(kws) - 1, L, B - R)
    y = Schaal(lo, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(lo, hi, 6):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(100 * tv, 0)}%</text>'
    s += f'<line x1="{L}" x2="{B - R}" y1="{y(0):.1f}" y2="{y(0):.1f}" class="nul"/>'
    for kw, naam in events:
        if kw in kws:
            xe = x(kws.index(kw)) - (x(1) - x(0)) / 2
            s += f'<line x1="{xe:.1f}" x2="{xe:.1f}" y1="{T}" y2="{H - D}" class="event"/>'
            tweede = events.index((kw, naam)) % 2
            s += (f'<text x="{xe - 4:.1f}" y="{T + 26}" class="eventlabel" text-anchor="end">{html.escape(naam)}</text>' if tweede
                  else f'<text x="{xe + 4:.1f}" y="{T + 12}" class="eventlabel">{html.escape(naam)}</text>')
    for i, kw in enumerate(kws):
        if i % stap == 0:
            s += f'<text x="{x(i):.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{kw}</text>'
    for kw, est, l, h in pts:
        xi = x(kws.index(kw))
        s += f'<line x1="{xi:.1f}" x2="{xi:.1f}" y1="{y(l):.1f}" y2="{y(h):.1f}" class="interval"/>'
        s += f'<circle cx="{xi:.1f}" cy="{y(est):.1f}" r="4.5" class="punt"><title>{kw}: {nl(100 * est, 1)}% (95%: {nl(100 * l, 1)} tot {nl(100 * h, 1)})</title></circle>'
    xi = x(kws.index(ref))
    s += f'<circle cx="{xi:.1f}" cy="{y(0):.1f}" r="4.5" class="punt referentie"><title>{ref}: referentie</title></circle>'
    s += noot_svg(L, H, f'Procentueel verschil in {grootheid} ten opzichte van de vergelijkingsgroep, per {per}, tegenover {ref}; punten met 95%-interval, standaardfouten geclusterd op gemeente.')
    return s + svg_einde()


def grafiek_reeksen(besch, titel, veld="aanvragen", events=EVENTS, stap=2,
                    noot="Aantal aanvragen van gemeenten per kwartaal, alle activiteiten; het laatste kwartaal is onvolledig."):
    """Aanvragen per kwartaal, nabij en ver, in aantallen."""
    per = {}
    for r in besch:
        per.setdefault(r["kwartaal"], {})[str(r["nabij"])] = float(r[veld])
    kws = sorted(per)
    reeksen = {naam: [(kw, per[kw].get(nab, 0)) for kw in kws] for nab, naam in (("1", "nabij"), ("0", "ver"))}
    B, H, L, R, T, D = 960, 360, 72, 150, 24, 60
    alle = [v for rr in reeksen.values() for _, v in rr]
    lo, hi = 0, max(alle) * 1.12
    x = Schaal(0, len(kws) - 1, L, B - R)
    y = Schaal(lo, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(lo, hi, 5):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(tv, 0)}</text>'
    for kw, naam in events:
        if kw in kws:
            xe = x(kws.index(kw)) - (x(1) - x(0)) / 2
            s += f'<line x1="{xe:.1f}" x2="{xe:.1f}" y1="{T}" y2="{H - D}" class="event"/>'
            tweede = events.index((kw, naam)) % 2
            s += (f'<text x="{xe - 4:.1f}" y="{T + 26}" class="eventlabel" text-anchor="end">{html.escape(naam)}</text>' if tweede
                  else f'<text x="{xe + 4:.1f}" y="{T + 12}" class="eventlabel">{html.escape(naam)}</text>')
    for i, kw in enumerate(kws):
        if i % stap == 0:
            s += f'<text x="{x(i):.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{kw}</text>'
    for naam, rr in reeksen.items():
        pad = " ".join(f"{'M' if i == 0 else 'L'}{x(i):.1f},{y(v):.1f}" for i, (_, v) in enumerate(rr))
        s += f'<path d="{pad}" class="lijn {naam}"/>'
        kw, v = rr[-1]
        s += f'<circle cx="{x(len(rr) - 1):.1f}" cy="{y(v):.1f}" r="4" class="punt {naam}"/>'
        s += f'<text x="{x(len(rr) - 1) + 8:.1f}" y="{y(v) + 4:.1f}" class="label {naam}">{"binnen 2.500 m" if naam == "nabij" else "verder dan 2.500 m"}</text>'
    s += noot_svg(L, H, noot)
    return s + svg_einde()


def grafiek_stikstof(stikstof, titel):
    """Aandeel uitspraken per jaar dat stikstof, AERIUS of depositie noemt; een lijn met punten en het eindpunt gelabeld."""
    rijen = [r for r in stikstof if r.get("jaar")]
    if not rijen:
        return ""
    jaren = [r["jaar"] for r in rijen]
    vals = [100 * float(r["aandeel_stikstof"]) for r in rijen]
    B, H, L, R, T, D = 960, 300, 64, 120, 24, 60
    hi = max(vals) * 1.2
    x = Schaal(0, len(jaren) - 1, L, B - R)
    y = Schaal(0, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(0, hi, 4):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(tv, 0)}%</text>'
    if "2019" in jaren:
        xe = x(jaren.index("2019")) - (x(1) - x(0)) / 2
        s += f'<line x1="{xe:.1f}" x2="{xe:.1f}" y1="{T}" y2="{H - D}" class="event"/>'
        s += f'<text x="{xe + 4:.1f}" y="{T + 12}" class="eventlabel">PAS-uitspraak</text>'
    pad = " ".join(f"{'M' if i == 0 else 'L'}{x(i):.1f},{y(v):.1f}" for i, v in enumerate(vals))
    s += f'<path d="{pad}" class="lijn nabij"/>'
    for i, (j, v) in enumerate(zip(jaren, vals)):
        s += f'<circle cx="{x(i):.1f}" cy="{y(v):.1f}" r="4" class="punt"><title>{j}: {nl(v, 1)}% van {rijen[i]["uitspraken"]} uitspraken</title></circle>'
        s += f'<text x="{x(i):.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{j}</text>'
    s += f'<text x="{x(len(vals) - 1) + 8:.1f}" y="{y(vals[-1]) + 4:.1f}" class="label nabij">{nl(vals[-1], 1)}% in {jaren[-1]}</text>'
    s += f'<text x="{L}" y="{H - 8}" class="noot">Aandeel van de gepubliceerde uitspraken in het omgevingsrecht dat stikstof, AERIUS of depositie noemt.</text>'
    return s + svg_einde()


def grafiek_gradient(rijen, titel):
    """Kleine veelvouden: per uitkomst de coefficient per afstandsklasse (referentie verder dan 5 km) met 95%-interval."""
    modellen = {}
    for r in rijen:
        modellen.setdefault(r["model"], []).append(r)
    if not modellen:
        return ""
    namen = {"geweigerd": "geweigerd (procentpunt)", "ingetrokken": "ingetrokken (procentpunt)", "geen": "geen uitkomst (procentpunt)",
             "verleend": "verleend (procentpunt)", "log(doorlooptijd)": "doorlooptijd verleend (procent)", "verlengingen": "verlengingen (procent)"}
    panelen = []
    for model, rr in modellen.items():
        kort = next((k for k in namen if k in model), model)
        pts = {}
        for r in rr:
            kl = r["term"].split("::")[1] if "::" in r["term"] else r["term"]
            est, se = float(r["schatting"]), float(r["se"])
            pts[kl] = (est, est - 1.96 * se, est + 1.96 * se)
        panelen.append((namen.get(kort, kort), pts, kort in ("log(doorlooptijd)", "verlengingen")))
    n = len(panelen)
    kol = 3 if n > 4 else 2
    rijn = math.ceil(n / kol)
    PB, PH = 300, 190
    B, H = kol * PB + 40, rijn * PH + 40
    s = svg_kop(B, H, titel)
    for i, (naam, pts, procent) in enumerate(panelen):
        ox, oy = 20 + (i % kol) * PB, 20 + (i // kol) * PH
        L, R, T, D = 44, 12, 26, 44
        vals = [v for kl in KLASSEN[:-1] if kl in pts for v in pts[kl]]
        if not vals:
            continue
        lo, hi = min(min(vals), 0) * 1.15, max(max(vals), 0) * 1.15
        x = Schaal(0, len(KLASSEN) - 2, ox + L, ox + PB - R)
        y = Schaal(lo, hi, oy + PH - D, oy + T)
        s += f'<text x="{ox + L}" y="{oy + 14}" class="paneeltitel">{html.escape(naam)}</text>'
        for tv in ticks(lo, hi, 4):
            s += f'<line x1="{ox + L}" x2="{ox + PB - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
            s += f'<text x="{ox + L - 6}" y="{y(tv) + 3:.1f}" class="as klein" text-anchor="end">{nl(100 * tv, 0)}</text>'
        s += f'<line x1="{ox + L}" x2="{ox + PB - R}" y1="{y(0):.1f}" y2="{y(0):.1f}" class="nul"/>'
        for j, kl in enumerate(KLASSEN[:-1]):
            if kl not in pts:
                continue
            est, l, h = pts[kl]
            xi = x(j)
            s += f'<line x1="{xi:.1f}" x2="{xi:.1f}" y1="{y(l):.1f}" y2="{y(h):.1f}" class="interval"/>'
            s += f'<circle cx="{xi:.1f}" cy="{y(est):.1f}" r="4" class="punt"><title>{KLASSE_LABEL[kl]}: {nl(100 * est, 1)} (95%: {nl(100 * l, 1)} tot {nl(100 * h, 1)})</title></circle>'
            lab = ["0-250", "250-500", "500-1000", "1000-2500", "2500-5000"][j]
            s += f'<text x="{xi:.1f}" y="{oy + PH - D + 14}" class="as klein" text-anchor="middle">{lab}</text>'
        s += f'<text x="{ox + L}" y="{oy + PH - D + 30}" class="as klein">meter tot habitat; referentie verder dan 5 km</text>'
    return s + svg_einde()


def grafiek_beroep(per_jaar_object, stikstof, titel):
    """Uitspraken per jaar over omgevingsvergunning en plannen (staven) en het aandeel met stikstof als grond (lijn erboven)."""
    if not per_jaar_object:
        return ""
    jaren = sorted({r["jaar"] for r in per_jaar_object if r["jaar"]})
    tel = {}
    for r in per_jaar_object:
        tel[(r["jaar"], r["object"])] = int(r["N"])
    reeksen = [("omgevingsvergunning", "omgevingsvergunning"), ("bestemmingsplan", "bestemmingsplan of omgevingsplan")]
    B, H, L, R, T, D = 960, 360, 64, 190, 30, 60
    hi = max(tel.get((j, "omgevingsvergunning"), 0) + tel.get((j, "bestemmingsplan"), 0) + tel.get((j, "omgevingsplan"), 0) for j in jaren) * 1.1 or 1
    x = Schaal(0, len(jaren), L, B - R)
    y = Schaal(0, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(0, hi, 5):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(tv, 0)}</text>'
    breedte = (x(1) - x(0)) * 0.72
    aandeel = {r["jaar"]: float(r["aandeel_stikstof"]) for r in stikstof} if stikstof else {}
    for i, j in enumerate(jaren):
        xi = x(i) + (x(1) - x(0)) * 0.14
        a = tel.get((j, "omgevingsvergunning"), 0)
        b = tel.get((j, "bestemmingsplan"), 0) + tel.get((j, "omgevingsplan"), 0)
        s += f'<rect x="{xi:.1f}" y="{y(a):.1f}" width="{breedte:.1f}" height="{max(0, y(0) - y(a)):.1f}" class="staaf nabij"><title>{j}: {nl(a)} over een omgevingsvergunning</title></rect>'
        s += f'<rect x="{xi:.1f}" y="{y(a + b) - 2:.1f}" width="{breedte:.1f}" height="{max(0, y(a) - y(a + b)):.1f}" class="staaf ver"><title>{j}: {nl(b)} over een plan</title></rect>'
        s += f'<text x="{xi + breedte / 2:.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{j}</text>'
        if i == len(jaren) - 1:
            s += f'<text x="{xi + breedte + 6:.1f}" y="{y(a / 2) + 4:.1f}" class="label nabij">omgevingsvergunning</text>'
            s += f'<text x="{xi + breedte + 6:.1f}" y="{y(a + b / 2) + 4:.1f}" class="label ver">plan</text>'
    s += f'<text x="{L}" y="{H - 8}" class="noot">Gepubliceerde uitspraken met tekst in het rechtsgebied omgevingsrecht; groen: over een omgevingsvergunning, blauwgrijs: over een bestemmingsplan of omgevingsplan. Het laatste jaar loopt tot september.</text>'
    return s + svg_einde()


# ---------- tabellen ----------

def grafiek_lijn(punten, titel, eenheid, noot, markeringen=()):
    """Een reeks per jaar als lijn met punten; punten is een lijst (jaar, waarde)."""
    if not punten:
        return ""
    B, H, L, R, T, D = 960, 340, 64, 24, 30, 64
    jaren = [j for j, _ in punten]
    hi = max(v for _, v in punten) * 1.15
    x = Schaal(min(jaren), max(jaren), L, B - R)
    y = Schaal(0, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(0, hi, 5):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(tv, 0)}</text>'
    for j, naam in markeringen:
        if min(jaren) <= j <= max(jaren):
            s += f'<line x1="{x(j):.1f}" x2="{x(j):.1f}" y1="{T}" y2="{H - D}" class="event"/>'
            s += f'<text x="{x(j) + 4:.1f}" y="{T + 12}" class="eventlabel">{html.escape(naam)}</text>'
    for j in jaren:
        if j % 2 == 1 or len(jaren) < 12:
            s += f'<text x="{x(j):.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{j}</text>'
    pad = " ".join(f"{'M' if i == 0 else 'L'}{x(j):.1f},{y(v):.1f}" for i, (j, v) in enumerate(punten))
    s += f'<path d="{pad}" class="lijn nabij"/>'
    for j, v in punten:
        s += f'<circle cx="{x(j):.1f}" cy="{y(v):.1f}" r="4" class="punt"><title>{j}: {nl(v, 0)} {html.escape(eenheid)}</title></circle>'
    s += noot_svg(L, H, noot)
    return s + svg_einde()


def grafiek_twee(a, b, namen, titel, eenheid, noot, markeringen=()):
    """Twee reeksen per jaar (nabij en ver) als lijnen met punten en een label aan het eind; a en b zijn lijsten (jaar, waarde).
    Een punt staat voor het hele jaar; een markering is een datum als jaar met fractie en staat een half jaar naar links."""
    if not a and not b:
        return ""
    B, H, L, R, T, D = 960, 340, 64, 80, 30, 64
    jaren = sorted({j for j, _ in a} | {j for j, _ in b})
    hi = max(v for _, v in a + b) * 1.15
    x = Schaal(min(jaren), max(jaren), L, B - R)
    y = Schaal(0, hi, H - D, T)
    s = svg_kop(B, H, titel)
    for tv in ticks(0, hi, 5):
        s += f'<line x1="{L}" x2="{B - R}" y1="{y(tv):.1f}" y2="{y(tv):.1f}" class="rooster"/>'
        s += f'<text x="{L - 8}" y="{y(tv) + 4:.1f}" class="as" text-anchor="end">{nl(tv, 0)}</text>'
    for i, (t, naam) in enumerate(markeringen):
        xm = t - 0.5
        if min(jaren) <= xm <= max(jaren):
            s += f'<line x1="{x(xm):.1f}" x2="{x(xm):.1f}" y1="{T}" y2="{H - D}" class="event"/>'
            s += (f'<text x="{x(xm) - 4:.1f}" y="{T + 26}" class="eventlabel" text-anchor="end">{html.escape(naam)}</text>' if i % 2
                  else f'<text x="{x(xm) + 4:.1f}" y="{T + 12}" class="eventlabel">{html.escape(naam)}</text>')
    for j in jaren:
        s += f'<text x="{x(j):.1f}" y="{H - D + 18}" class="as" text-anchor="middle">{j}</text>'
    for reeks, klasse, naam in ((a, "nabij", namen[0]), (b, "ver", namen[1])):
        if not reeks:
            continue
        pad = " ".join(f"{'M' if i == 0 else 'L'}{x(j):.1f},{y(v):.1f}" for i, (j, v) in enumerate(reeks))
        s += f'<path d="{pad}" class="lijn {klasse}"/>'
        for j, v in reeks:
            s += f'<circle cx="{x(j):.1f}" cy="{y(v):.1f}" r="4.5" class="punt {klasse}"><title>{html.escape(naam)} {j}: {nl(v, 1)} {html.escape(eenheid)}</title></circle>'
        j, v = reeks[-1]
        s += f'<text x="{x(j) + 10:.1f}" y="{y(v) + 4:.1f}" class="label {klasse}">{html.escape(naam)}</text>'
    s += noot_svg(L, H, noot)
    return s + svg_einde()


def tabel(kop, rijen, titel=None, klasse="tabel", links=None):
    s = ""
    if links is None:
        eerste = rijen[0] if rijen else []
        nl_links = next((i for i, c in enumerate(eerste) if any(ch.isdigit() for ch in str(c)[:3])), 1) or 1
    else:
        nl_links = links
    if titel:
        TELLER["tabel"] += 1
        s += f'<p class="tabeltitel">Tabel {TELLER["tabel"]}. {html.escape(titel)}</p>'
    s += f'<div class="tabelwrap"><table class="{klasse}"><thead><tr>' + "".join(f'<th class="{"l" if i < nl_links else ""}">{html.escape(k)}</th>' for i, k in enumerate(kop)) + "</tr></thead><tbody>"
    for r in rijen:
        s += "<tr>" + "".join(f'<td class="{"l" if i < nl_links else ""}">{c}</td>' for i, c in enumerate(r)) + "</tr>"
    return s + "</tbody></table></div>"


def tabel_coef(rijen, labels, titel=None):
    """Coefficienten als tabel: term, schatting (procent), 95%-interval, p."""
    uit = []
    for r in rijen:
        est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
        naam = labels.get(r["term"], r["term"])
        ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
        uit.append([html.escape(naam), nl(100 * est, 1) + ster, f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3), nl(int(float(r["n"])))])
    return tabel(["term", "schatting", "95%-interval", "p", "n"], uit, titel)


def bouw(analysemap, teksten_pad, uit_pad):
    map_ = Path(analysemap)
    res = json.loads((map_ / "resultaten.json").read_text(encoding="utf-8"))
    bag = json.loads((map_ / "bag_resultaten.json").read_text(encoding="utf-8")) if (map_ / "bag_resultaten.json").exists() else {}
    bl = json.loads((map_ / "blootstelling_resultaten.json").read_text(encoding="utf-8")) if (map_ / "blootstelling_resultaten.json").exists() else {}
    T = lees_teksten(teksten_pad)
    meta = res.get("meta", {})
    besch = res.get("beschrijvend_kwartaal_nabij", [])
    klasse = res.get("beschrijvend_klasse", [])
    did_labels = {"nabij:post_pas": "nabij x vanaf 2019K3 (PAS-uitspraak)", "nabij:post_porthos": "nabij x vanaf 2023K1 (Porthos)",
                  "nabij:post_ow": "nabij x vanaf 2024K1 (Omgevingswet)", "nabij:post_is": "nabij x vanaf 2025K1 (intern salderen)"}
    # kerncijfers
    tot_aanvr = sum(int(r["aanvragen"]) for r in besch)
    tot_nabij = sum(int(r["aanvragen"]) for r in besch if str(r["nabij"]) == "1")
    porthos = next((r for r in res.get("did_aanvragen", []) if r["term"] == "nabij:post_porthos"), None)

    hoofdstuk = {"n": 0}

    def sectie(sleutel, kop, inhoud=""):
        hoofdstuk["n"] += 1
        return f'<section id="{sleutel}"><h2>{hoofdstuk["n"]}. {html.escape(kop)}</h2>{T.get(sleutel, "")}{inhoud}</section>'

    klasse_rijen = [[KLASSE_LABEL.get(r["klasse"], r["klasse"]), nl(int(r["aanvragen"])), pct(r["verleend"]), pct(r["geweigerd"]), pct(r["ingetrokken"]),
                     pct(r["geen"]), nl(float(r["mediaan_dagen"])) if r.get("mediaan_dagen") not in (None, "") else "", nl(float(r["verlengingen_per_100"]), 1)]
                    for r in sorted(klasse, key=lambda r: KLASSEN.index(r["klasse"]) if r["klasse"] in KLASSEN else 99)]
    # tabellen worden pas gebouwd op hun plek in de pagina, zodat de nummering de leesvolgorde volgt
    def maak_uitk_tabel():
        per_model = {}
        for r in res.get("did_uitkomsten", []):
            per_model.setdefault(r["model"], []).append(r)
        return "".join(tabel_coef(rr, did_labels, model) for model, rr in per_model.items())

    SOORT_LABEL = {"nieuwbouw_woning": "nieuwbouw woningen", "nieuwbouw_overig": "nieuwbouw overig (bedrijf, stal, voorziening)",
                   "klein_verbouw": "klein werk en verbouw", "sloop_kap": "sloop en kap", "overig": "overig of onbepaald"}

    def maak_soort_tabellen():
        verd = res.get("soort_verdeling", [])
        s = tabel(["projecttype", "aanvragen", "waarvan nabij", "verleend", "geweigerd", "ingetrokken", "geen uitkomst", "mediaan dagen"],
                  [[SOORT_LABEL.get(r["soort"], r["soort"]), nl(int(r["aanvragen"])), nl(int(r["nabij"])), pct(r["verleend"]), pct(r["geweigerd"]),
                    pct(r["ingetrokken"]), pct(r["geen"]), nl(float(r["mediaan_dagen"])) if r.get("mediaan_dagen") not in (None, "") else ""] for r in verd],
                  "Aanvragen naar projecttype, uit de titels van de zaak") if verd else ""
        did = res.get("soort_did_aanvragen", [])
        if did:
            rijen = []
            for r in did:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                soort = r["model"].split(",")[-1].strip()
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                rijen.append([SOORT_LABEL.get(soort, soort), html.escape(did_labels.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["projecttype", "term", "schatting", "95%-interval", "p"], rijen,
                       "Difference-in-differences op aanvragen per projecttype (Poisson, gemeente maal nabij en kwartaal als vaste effecten)")
        uitk = res.get("soort_did_uitkomsten", [])
        if uitk:
            rijen = []
            for r in uitk:
                if "log(doorlooptijd)" not in r["model"] and "verleend ~" not in r["model"]:
                    continue
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                soort = r["model"].split(",")[-1].strip()
                maat = "doorlooptijd verleend (procent)" if "log(doorlooptijd)" in r["model"] else "aandeel verleend (procentpunt)"
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                rijen.append([SOORT_LABEL.get(soort, soort), maat, html.escape(did_labels.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["projecttype", "maat", "term", "schatting", "95%-interval", "p"], rijen,
                       "Difference-in-differences op het aandeel verleend en op de doorlooptijd, per projecttype")
        grad = res.get("soort_gradient", [])
        if grad:
            rijen = []
            for r in grad:
                if not (r["term"].endswith("::0_250") or r["term"].endswith("::250_500")):
                    continue
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                soort = r["model"].split(",")[-1].strip()
                maat = "doorlooptijd verleend (procent)" if "log(doorlooptijd)" in r["model"] else ("aandeel verleend (procentpunt)" if "verleend ~" in r["model"] else "aandeel geen uitkomst (procentpunt)")
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                rijen.append([SOORT_LABEL.get(soort, soort), maat, KLASSE_LABEL[r["term"].split("::")[1]], nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["projecttype", "maat", "afstandsklasse", "schatting", "95%-interval", "p"], rijen,
                       "Afstandsgradiënt per projecttype, de twee dichtstbijzijnde klassen, ten opzichte van verder dan 5 km, met gemeente- en kwartaaleffecten")
        return s

    def maak_aerius_tabellen():
        alt = res.get("alt_overbelast", [])
        if not alt:
            return ""
        rijen = []
        for r in alt:
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            model = r["model"]
            if "tertiel" in model:
                maat = "doorlooptijd verleend (procent)" if "log(doorlooptijd)" in model else ("aandeel verleend (procentpunt)" if model.startswith("feols verleend") else "aandeel geen uitkomst (procentpunt)")
                term = r["term"].replace("ovb_tertiel::", "overbelasting binnen 5 km ").replace(":post_porthos", " x na Porthos")
                blootstelling = "mate van overbelasting"
            else:
                if "log(doorlooptijd)" in model:
                    maat = "doorlooptijd verleend (procent)" + (", nieuwbouw woningen" if "nieuwbouw" in model else "")
                elif model.startswith("fepois aanvragen"):
                    maat = "aanvragen (procent)"
                else:
                    maat = "aandeel " + model.split(" ~")[0].replace("feols ", "") + " (procentpunt)"
                term = did_labels.get(r["term"], r["term"]).replace("nabij x", "nabij overbelast hexagon x")
                blootstelling = "binnen 2.500 m van een overbelast hexagon"
            ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
            rijen.append([blootstelling, maat, html.escape(term), nl(100 * est, 1) + ster, f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
        return tabel(["blootstelling", "maat", "term", "schatting", "95%-interval", "p"], rijen,
                     "Dezelfde modellen met de blootstelling uit de AERIUS-hexagonen (depositie boven de kritische depositiewaarde, AERIUS 2025, depositie 2023)")

    def maak_oud():
        """Hoofdstuk over het vaste panel van gemeenten die al voor 2021 publiceerden: de PAS-uitspraak in de bekendmakingen."""
        es = res.get("oud_event_study_aanvragen", [])
        if not es:
            return ""
        om = res.get("oud_meta", {})
        s = f"""
    <div class="kern">
      <div class="tegel"><div class="waarde">{nl(om.get('gemeenten', 0))}</div><div class="omschrijving">gemeenten met minstens 40 aanvragen per jaar in 2016 tot 2020</div></div>
      <div class="tegel"><div class="waarde">{nl(om.get('aanvragen', 0))}</div><div class="omschrijving">aanvragen in het venster 2016K1 tot 2022K2</div></div>
      <div class="tegel"><div class="waarde">{pct(om.get('nabij', 0) / om.get('aanvragen', 1) if om.get('aanvragen') else 0)}</div><div class="omschrijving">binnen 2.500 m van stikstofgevoelig habitat</div></div>
    </div>"""
        dek = res.get("oud_dekking_jaar", [])
        if dek:
            s += tabel(["jaar", "gemeenten met aanvragen", "aanvragen"], [[str(r["jaar"]), nl(int(r["gemeenten"])), nl(int(r["aanvragen"]))] for r in dek],
                       "Dekking van de bekendmakingen per jaar: gemeenten die aanvragen publiceerden (alle stromen)")
        s += grafiek_event_study(es, "Event study: aanvragen nabij ten opzichte van ver, per kwartaal, vast panel 2016K1 tot 2022K2, tegenover 2019K1",
                                 ref="2019K1", events=EVENTS_BAG, stap=2)
        labels_oud = {"nabij:post_pas": "nabij x vanaf 2019K3 (PAS-uitspraak)", "t_voor": "nabij x trend voor de uitspraak, per jaar",
                      "sprong": "nabij x sprong bij de uitspraak", "t_na": "nabij x trend na de uitspraak, per jaar"}
        if res.get("oud_did_aanvragen"):
            s += tabel_coef(res["oud_did_aanvragen"], labels_oud, "Difference-in-differences op aanvragen in het vaste panel (Poisson, gemeente maal nabij en kwartaal als vaste effecten): alle aanvragen, bouwaanvragen, en alle aanvragen met een trendbreuk (trend voor, sprong, trend na)")
        uitk = res.get("oud_did_uitkomsten", [])
        if uitk:
            rijen = []
            for r in uitk:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                model = r["model"]
                if model.startswith("fepois"):
                    maat = "aanvragen nieuwbouw woningen (procent)"
                elif "log(doorlooptijd)" in model:
                    maat = "doorlooptijd verleend (procent)" + (", nieuwbouw woningen" if "nieuwbouw" in model else "")
                else:
                    maat = "aandeel " + model.split(" ~")[0].replace("feols ", "") + " (procentpunt)"
                rijen.append([maat, html.escape(labels_oud.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3), nl(int(float(r["n"])))])
            s += tabel(["maat", "term", "schatting", "95%-interval", "p", "n"], rijen,
                       "Uitkomsten en doorlooptijd in het vaste panel na de PAS-uitspraak, zaken met aanvraag 2016K1 tot 2022K2")
        jaar = res.get("oud_beschrijvend_jaar_nabij", [])
        if jaar:
            s += tabel(["jaar", "ligging", "aanvragen", "verleend", "geweigerd", "ingetrokken", "besluit zonder uitkomst", "geen uitkomst", "mediaan dagen"],
                       [[str(r["jaar"]), "nabij" if str(r["nabij"]) == "1" else "ver", nl(int(r["aanvragen"])), pct(r["verleend"]), pct(r["geweigerd"]), pct(r["ingetrokken"]),
                         pct(r["besluit_onbekend"]), pct(r["geen"]), nl(float(r["mediaan_dagen"])) if r.get("mediaan_dagen") not in (None, "") else ""] for r in jaar],
                       "Het vaste panel per jaar en ligging: aanvragen en uitkomsten")
        return s

    vp = json.loads((map_ / "vastpanel_resultaten.json").read_text(encoding="utf-8")) if (map_ / "vastpanel_resultaten.json").exists() else {}

    def maak_vastpanel():
        """Het vaste panel doorgetrokken tot 2026 en de strenge vergelijking binnen gemeente en kwartaal."""
        if not vp:
            return ""
        vm = vp.get("meta", {})
        rijen = []
        for v, oms in (("A", "minstens 40 aanvragen per jaar 2016 tot 2020"), ("B", "minstens 20 per jaar 2017 tot 2020"),
                       ("C", "minstens 20 per jaar 2018 tot 2020"), ("L", "alle gemeenten, landelijk dekkend venster")):
            m = vm.get(v)
            if m:
                rijen.append([v, oms, str(m.get("start", "")), nl(int(m.get("gemeenten", 0))), nl(int(m.get("aanvragen", 0))),
                              pct(m.get("nabij", 0) / m.get("aanvragen", 1) if m.get("aanvragen") else 0)])
        s = tabel(["variant", "gemeenten", "vanaf", "aantal gemeenten", "aanvragen tot 2026K2", "aandeel nabij"], rijen,
                  "De panels van de strenge vergelijking", links=2)
        s += grafiek_event_study(vp.get("vp_event_study_A", []), "Event study binnen gemeente en kwartaal: aanvragen nabij ten opzichte van ver, vast panel A, 2016K1 tot 2026K2, tegenover 2019K1",
                                 ref="2019K1", events=EVENTS_BAG, stap=4, grootheid="aanvragen nabij habitat binnen dezelfde gemeente en hetzelfde kwartaal")
        s += grafiek_event_study(vp.get("vp_event_study_L", []), "Event study binnen gemeente en kwartaal: aanvragen nabij ten opzichte van ver, alle gemeenten, 2021K3 tot 2026K2, tegenover 2022K3",
                                 ref="2022K3", events=EVENTS, stap=2, grootheid="aanvragen nabij habitat binnen dezelfde gemeente en hetzelfde kwartaal")
        vl = dict(did_labels)
        vl["nabij:post_bvr"] = "nabij x vanaf 2021K3 (bouwvrijstelling)"
        spec = lambda m: (("variant " + m.rsplit("variant ", 1)[1]) + (", alleen kwartaal" if "+ kwartaal" in m else "")
                          + (", bouwaanvragen" if m.startswith("fepois bouw_") else ", nieuwbouw woningen" if "nieuwbouw" in m else ""))
        rij_did = []
        for r in vp.get("vp_did", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
            rij_did.append([html.escape(spec(r["model"])), html.escape(vl.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                            f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
        s += tabel(["specificatie", "term", "schatting (procent)", "95%-interval", "p"], rij_did,
                   "Difference-in-differences op aanvragen, binnen gemeente en kwartaal (tenzij alleen kwartaal vermeld), per panel", links=2)
        rij_u = []
        for r in vp.get("vp_uitkomsten", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
            m = r["model"]
            maat = ("doorlooptijd verleend (procent)" + (", nieuwbouw woningen" if "nieuwbouw" in m else ", klein werk" if "klein" in m else "")) if "log(" in m else \
                   ("aandeel verleend (procentpunt)" if m.startswith("feols verleend") else "aandeel zonder uitkomst (procentpunt)")
            rij_u.append(["variant " + m.rsplit("variant ", 1)[1], maat, html.escape(vl.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                          f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
        s += tabel(["panel", "maat", "term", "schatting", "95%-interval", "p"], rij_u,
                   "Uitkomsten en doorlooptijd binnen gemeente en kwartaal, zaken met minstens negen maanden vervolg", links=3)
        tbl = {"t_voor": "nabij x trend voor de uitspraak, per jaar", "sprong": "nabij x sprong bij de uitspraak", "t_na": "nabij x trend na de uitspraak, per jaar"}
        tbl.update(vl)
        rij_t = []
        for r in vp.get("vp_trendbreuk", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
            rij_t.append(["variant " + r["model"].rsplit("variant ", 1)[1], html.escape(tbl.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                          f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
        s += tabel(["panel", "term", "schatting (procent)", "95%-interval", "p"], rij_t,
                   "Trendbreuk bij de PAS-uitspraak binnen gemeente en kwartaal, met de latere wisselingen als sprongen", links=2)
        return s

    def maak_bag():
        """Hoofdstuk over de BAG-cohorten: vergunde nieuwe panden per kwartaal sinds 2012 en wat er daarna mee gebeurt."""
        if not bag:
            return ""
        bmeta = bag.get("meta", {})
        s = f"""
    <div class="kern">
      <div class="tegel"><div class="waarde">{nl(bmeta.get('panden', 0))}</div><div class="omschrijving">nieuwe panden met een vergunning, 2012 tot medio 2026</div></div>
      <div class="tegel"><div class="waarde">{nl(bmeta.get('woonpanden', 0))}</div><div class="omschrijving">waarvan met minstens een woning</div></div>
      <div class="tegel"><div class="waarde">{nl(bmeta.get('woningen', 0))}</div><div class="omschrijving">verblijfsobjecten met woonfunctie in die panden</div></div>
      <div class="tegel"><div class="waarde">{html.escape(str(bmeta.get('stand_extract', '')))}</div><div class="omschrijving">stand van het BAG-extract</div></div>
    </div>"""
        s += grafiek_reeksen(bag.get("bag_beschrijvend_kwartaal_nabij", []), "Vergunde woonpanden per kwartaal van vergunningverlening, binnen en buiten 2.500 meter van stikstofgevoelig habitat",
                             veld="woonpanden", events=EVENTS_BAG, stap=4,
                             noot="Aantal nieuwe panden met minstens een woning waarvoor in dat kwartaal de status Bouwvergunning verleend werd geregistreerd; alle gemeenten; bron LV BAG-extract.")
        s += grafiek_event_study(bag.get("bag_event_study_woonpanden", []), "Event study: vergunde woonpanden nabij ten opzichte van ver, per kwartaal, tegenover 2019K1",
                                 ref="2019K1", events=EVENTS_BAG, stap=4, grootheid="vergunde woonpanden")
        did = bag.get("bag_did_aantallen", [])
        if did:
            rijen = []
            for r in did:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                grootheid = r["model"].split("~")[0].replace("fepois ", "").strip()
                venster = "2016K3 tot 2022K2, alleen de PAS-uitspraak" if "2016K3" in r["model"] else "2012K1 tot 2026K2, gestapeld"
                rijen.append([grootheid, venster, html.escape(did_labels.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["grootheid", "venster", "term", "schatting (procent)", "95%-interval", "p"], rijen,
                       "Difference-in-differences op vergunde nieuwe panden en woningen (Poisson, gemeente maal nabij en kwartaal als vaste effecten)")
        jaar = bag.get("bag_beschrijvend_jaar_nabij", [])
        if jaar:
            rijen = []
            for r in jaar:
                rijen.append([str(r["jaar"]), "nabij" if str(r["nabij"]) == "1" else "ver", nl(int(r["panden"])), nl(int(r["woonpanden"])), nl(int(r["woningen"])),
                              pct(r["gestart_365"]) if r.get("gestart_365") not in (None, "") else "",
                              nl(float(r["mediaan_dagen_bouw"])) if r.get("mediaan_dagen_bouw") not in (None, "") else "",
                              pct(r["gebruik_730"]) if r.get("gebruik_730") not in (None, "") else "",
                              pct(r["nietger_1095"]) if r.get("nietger_1095") not in (None, "") else ""])
            s += tabel(["jaar vergunning", "ligging", "panden", "woonpanden", "woningen", "bouw gestart binnen 1 jaar", "mediaan dagen tot bouwstart",
                        "in gebruik binnen 2 jaar", "niet gerealiseerd binnen 3 jaar"], rijen,
                       "Cohorten naar jaar van vergunningverlening en ligging; aandelen alleen voor cohorten met genoeg vervolg")
        uitk = bag.get("bag_did_uitkomsten", [])
        if uitk:
            rijen = []
            MAAT = {"gestart_365": "bouw gestart binnen 1 jaar (procentpunt)", "gebruik_730": "in gebruik binnen 2 jaar (procentpunt)",
                    "nietger_1095": "niet gerealiseerd binnen 3 jaar (procentpunt)"}
            for r in uitk:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                model = r["model"]
                maat = "dagen tot bouwstart (procent)" if "log(" in model else MAAT.get(model.split("~")[0].replace("feols ", "").strip(), model)
                groep = "woonpanden" if "woonpanden" in model else "alle panden"
                rijen.append([maat, groep, html.escape(did_labels.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3), nl(int(float(r["n"])))])
            s += tabel(["maat", "groep", "term", "schatting", "95%-interval", "p", "n"], rijen,
                       "Difference-in-differences op wat er na de vergunning gebeurt, per pand, gemeente maal nabij en kwartaal van vergunning als vaste effecten")
        gr = bag.get("bag_did_grootte", [])
        if gr:
            rijen = []
            for r in gr:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                model = r["model"]
                grootte = model.split(",")[-1].strip()
                maat = "vergunde panden (procent)" if model.startswith("fepois") else ("dagen tot bouwstart (procent)" if "log(" in model else "bouw gestart binnen 1 jaar (procentpunt)")
                rijen.append([grootte, maat, html.escape(did_labels.get(r["term"], r["term"])), nl(100 * est, 1) + ster,
                              f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["projectgrootte", "maat", "term", "schatting", "95%-interval", "p"], rijen,
                       "Dezelfde modellen naar projectgrootte (woningen per pand)")
        grad = bag.get("bag_gradient_pas", [])
        if grad:
            rijen = []
            for r in grad:
                est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
                ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
                model = r["model"]
                maat = "dagen tot bouwstart (procent)" if "log(" in model else ("bouw gestart binnen 1 jaar (procentpunt)" if "gestart_365" in model else "niet gerealiseerd binnen 3 jaar (procentpunt)")
                kl = r["term"].split("::")[1].split(":")[0] if "::" in r["term"] else r["term"]
                rijen.append([maat, KLASSE_LABEL.get(kl, kl), nl(100 * est, 1) + ster, f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
            s += tabel(["maat", "afstandsklasse x na PAS-uitspraak", "schatting", "95%-interval", "p"], rijen,
                       "Verandering per afstandsklasse na de PAS-uitspraak ten opzichte van verder dan 5 km, 2016K3 tot 2022K2, gemeente maal klasse en kwartaal als vaste effecten")
        return s

    GROEP_LABEL = {"Q2": "depositiekwartiel 2", "Q3": "depositiekwartiel 3", "Q4": "depositiekwartiel 4 (hoogst)",
                   "nabij_hoog": "nabij, hoge depositie", "nabij_laag": "nabij, lage depositie", "nabij": "nabij",
                   "nabij_veel_vee": "nabij, veel vee binnen 1 km", "nabij_weinig_vee": "nabij, weinig vee binnen 1 km",
                   "nabij_met_melders": "nabij, met PAS-melders in 5 km", "nabij_geen_melders": "nabij, zonder PAS-melders in 5 km",
                   "rood_2024": "afnamenet rood sinds april 2024", "rood_2025": "afnamenet rood sinds juli 2025"}
    VAR_LABEL = {"post_pas": "na de PAS-uitspraak (vanaf 2019K3)", "post_porthos": "na Porthos (vanaf 2023K1)",
                 "post_ow": "na de Omgevingswet (vanaf 2024K1)", "post_is": "na intern salderen (vanaf 2025K1)",
                 "t_voor": "trend voor de uitspraak, per jaar", "t_na": "trend na de uitspraak, per jaar",
                 "post_2024": "na april 2024", "post_2025": "na juli 2025"}

    def term_label(term):
        if term.startswith("groep::"):
            rest = term[len("groep::"):]
            g, _, v = rest.rpartition(":")
            return f"{GROEP_LABEL.get(g, g)} x {VAR_LABEL.get(v, v)}"
        if ":" in term:
            g, _, v = term.partition(":")
            g = {"nabij": "nabij", "dep_hoog": "hoogste depositiekwartiel"}.get(g, g)
            return f"{g} x {VAR_LABEL.get(v, v)}"
        return term

    def coef_tabel(rijen, titel, specificatie=None):
        if not rijen:
            return ""
        uit = []
        for r in rijen:
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            ster = "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
            spec = specificatie(r["model"]) if specificatie else ""
            uit.append(([spec] if specificatie else []) + [html.escape(term_label(r["term"])), nl(100 * est, 1) + ster,
                                                          f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}", nl(p, 3)])
        kop = (["specificatie"] if specificatie else []) + ["term", "schatting (procent)", "95%-interval", "p"]
        return tabel(kop, uit, titel)

    def maak_blootstelling():
        if not bl:
            return ""
        bm = bl.get("meta", {})
        did = bl.get("bl_bag_did", [])
        s = ""
        dosis = bl.get("bl_dosis_beschrijvend", [])
        if dosis:
            s += tabel(["depositiekwartiel 2018", "vergunde woonpanden 2012 tot 2026", "aandeel nabij habitat", "mediane depositie 2018 (mol N per ha per jaar)"],
                       [[r["dep_kw"], nl(int(r["woonpanden"])), pct(r["nabij"]), nl(float(r["ndep_2018_mediaan"]))] for r in dosis],
                       "Vergunde woonpanden naar de stikstofdepositie ter plaatse in 2018 (RIVM GDN, 1 km), kwartielen over de panden van voor de uitspraak")
        es = [r for r in bl.get("bl_dosis_event_study", []) if r["model"].endswith("Q4 tegenover Q1")]
        s += grafiek_event_study(es, "Event study: vergunde woonpanden in het hoogste tegenover het laagste depositiekwartiel, per kwartaal, tegenover 2019K1",
                                 ref="2019K1", events=EVENTS_BAG, stap=4, grootheid="vergunde woonpanden in het hoogste depositiekwartiel")
        s += coef_tabel([r for r in did if "depositiekwartiel 2018 x post" in r["model"] or "kwartiel x (trend voor" in r["model"]
                         or r["model"].startswith("fepois woonpanden ~ depositiekwartiel x post")],
                        "Difference-in-differences per depositiekwartiel: zonder trend, met een eigen lineaire trend per kwartiel, en met een eigen trend per gemeente en kwartiel tot 2022K2 (Poisson, gemeente maal kwartiel en kwartaal als vaste effecten)",
                        lambda m: "trend per gemeente" if "met eigen trend" in m else "met trend" if "trend voor" in m else "zonder trend")
        es = [r for r in bl.get("bl_dosis_nabij_event_study", []) if r["model"].endswith("nabij_hoog tegenover ver")]
        s += grafiek_event_study(es, "Event study: vergunde woonpanden nabij habitat met hoge depositie tegenover ver, per kwartaal, tegenover 2019K1",
                                 ref="2019K1", events=EVENTS_BAG, stap=4, grootheid="vergunde woonpanden nabij habitat met hoge depositie")
        s += coef_tabel([r for r in did if r["model"].startswith("fepois woonpanden ~ nabij")],
                        "Nabij habitat: de hoofdspecificatie met een lineaire voortrend, met een voorperiode vanaf 2015, en naar depositie, vee en PAS-melders",
                        lambda m: ("trend per gemeente" if "met eigen trend" in m else "trendbreuk" if "trend voor" in m else "vanaf 2015" if "2015K1" in m else "laag of hoog depositie" if "laag of hoog" in m
                                   else "vee binnen 1 km" if "vee" in m else "PAS-melders" if "melders" in m else "binnen de contour" if "binnen de contour" in m else "buiten de contour" if "buiten de contour" in m else "nabij x post"))
        s += coef_tabel(bl.get("bl_aanvragen_did", []), "Aanvragen in de bekendmakingen naar depositiekwartiel 2018 en, voor nieuwbouw van woningen, nabij habitat binnen en buiten de verstedelijkingscontour, juli 2021 tot september 2026",
                        lambda m: ("nieuwbouw woningen, binnen de contour" if "binnen de contour" in m else "nieuwbouw woningen, buiten de contour" if "buiten de contour" in m
                                   else "nieuwbouw woningen" if "nieuwbouw" in m else "alle aanvragen"))
        proj = bl.get("bl_projecten_beschrijvend", [])
        if proj:
            s += tabel(["jaar aanvraag", "ligging", "projecten", "woningen", "mediaan dagen", "75e percentiel"],
                       [[str(r["jaar"]), "nabij" if str(r["nabij"]) == "1" else "ver", nl(int(r["projecten"])), nl(int(r["woningen"])),
                         nl(float(r["mediaan_dagen"])), nl(float(r["p75_dagen"]))] for r in proj],
                       "Echte woningbouwprojecten: dagen van de aanvraagpublicatie tot de status Bouwvergunning verleend in de BAG, zaken met een nieuw woonpand, aanvragen tot een jaar voor de stand")
        s += coef_tabel(bl.get("bl_projecten_did", []), "Difference-in-differences op de log-dagen van aanvraag tot BAG-vergunning (lineair, gemeente maal groep en kwartaal van aanvraag als vaste effecten)",
                        lambda m: m.split(",")[-1].strip() if "," in m else ("depositie" if "depositie" in m else "alle"))
        nw = map_.parent / "natuur" / "natuur_woningbouw_per_jaar.csv"
        if nw.exists():
            with open(nw, encoding="utf-8", newline="") as f:
                rr = list(csv.DictReader(f, delimiter=";"))
            s += tabel(["jaar", "gebiedsbescherming (Natura 2000, stikstof)", "Wet natuurbescherming zonder nadere aanduiding", "soortenbescherming"],
                       [[r["jaar"], nl(int(r["gebied"])), nl(int(r["wnb_onbepaald"])), nl(int(r["soorten"]))] for r in rr],
                       "Provinciale natuurbesluiten die in de titel een woningbouwproject noemen, per jaar van publicatie (Provinciaal blad, ondergrens)", links=1)
        cb = bl.get("bl_congestie_beschrijvend", [])
        if cb:
            s += tabel(["netcongestie afname", "woonpanden 2022 tot 2026", "woningen", "aandeel nabij"],
                       [[html.escape(r["groep"]), nl(int(r["woonpanden"])), nl(int(r["woningen"])), pct(r["nabij"])] for r in cb],
                       "Vergunde woonpanden naar de eerste stand waarin het afnamenet ter plaatse rood was")
        es = bl.get("bl_congestie_event_study_bag", [])
        if es:
            s += grafiek_event_study(es, "Event study: vergunde woonpanden waar het afnamenet sinds april 2024 rood is, tegenover nooit rood, per kwartaal, tegenover 2024K1",
                                     ref="2024K1", events=[("2024K2", "rood april 2024")], stap=2, grootheid="vergunde woonpanden in gebieden met netcongestie")
        s += coef_tabel(bl.get("bl_congestie_did", []), "Netcongestie op het afnamenet als tweede rem (Poisson, gemeente maal groep en kwartaal als vaste effecten)",
                        lambda m: "BAG-woonpanden" if m.startswith("fepois woonpanden") else m.split(",")[-1].strip())
        return s

    def maak_validatie():
        """Toets van de zaken aan DSO-LV en aan de open registers van Den Haag en Eindhoven (valideer_registers.py)."""
        vr = map_ / "validatie_registers_maand.csv"
        if not vr.exists():
            return ""
        import statistics
        per = {}
        with open(vr, encoding="utf-8", newline="") as f:
            for r in csv.DictReader(f, delimiter=";"):
                j = r["maand"][:4]
                if r["gemeente"] == "Eindhoven" and j < "2022":
                    continue
                d = per.setdefault((r["gemeente"], j), [0, 0, [], []])
                d[0] += int(r["register_ingediend"] or 0)
                d[1] += int(r["zaken"] or 0)
                if r["register_mediaan_dagen"]:
                    d[2].append(float(r["register_mediaan_dagen"]))
                if r["zaken_mediaan_dagen_verleend"]:
                    d[3].append(float(r["zaken_mediaan_dagen_verleend"]))
        rijen = [[g, j, nl(a), nl(b), pct(b / a if a else 0), nl(statistics.median(c)) if c else "", nl(statistics.median(d)) if d else ""]
                 for (g, j), (a, b, c, d) in sorted(per.items())]
        s = tabel(["gemeente", "jaar", "aanvragen in het register", "zaken met aanvraagpublicatie", "verhouding", "mediaan dagen register", "mediaan dagen zaken, verleend"],
                  rijen, "Toets aan de open vergunningregisters van Den Haag (Wabo, 2019 tot 2023) en Eindhoven (Wabo en Omgevingswet, 2022 tot 2026)", links=2)
        vd = map_ / "validatie_dso_maand.csv"
        if vd.exists():
            with open(vd, encoding="utf-8", newline="") as f:
                rr = list(csv.DictReader(f, delimiter=";"))
            s += tabel(["maand", "aanvragen bij gemeenten volgens DSO-LV", "zaken met aanvraagpublicatie", "verhouding"],
                       [[r["maand"], nl(int(r["dso_aanvragen_gemeenten"])), nl(int(r["zaken_met_aanvraagpublicatie"])), pct(float(r["verhouding"]))] for r in rr],
                       "Toets aan de telling van het Omgevingsloket (DSO-LV, Kadaster voor de Monitor Werking Omgevingswet), 2025", links=1)
        return s

    def maak_rechtspraak_verdieping():
        """Doorlooptijd van de Raad van State uit de jaarverslagen en de verdiepte lezing van de woningbouwuitspraken."""
        rv = map_.parent / "bron" / "rechtspraak_verdieping"
        if not rv.exists():
            return ""
        s = ""
        punten = {}
        with open(rv / "doorlooptijden_raadvanstate.csv", encoding="utf-8", newline="") as f:
            for r in csv.DictReader(f, delimiter=";"):
                if (("ordening" in r["kamer"].lower() or r["kamer"] == "Omgevingskamer") and r["maat"] == "doorlooptijd_gemiddeld"
                        and r["categorie"] == "alle zaken, hoofdzaken" and r["waarde"]):
                    punten[int(r["jaar"])] = float(r["waarde"].replace(",", "."))
        s += grafiek_lijn(sorted(punten.items()), "Gemiddelde doorlooptijd van hoofdzaken in de ruimtelijke-ordeningskamer (sinds 2020 de Omgevingskamer) van de Raad van State, per jaar",
                          "weken", "Weken, uit de jaarverslagen van de Raad van State 2010 tot 2025; in 2020 gingen de ruimtelijke-ordeningskamer en de milieukamer op in de Omgevingskamer.",
                          markeringen=[(2019, "PAS-uitspraak")])
        rijen = []
        with open(rv / "tabel_per_jaar_instantie.csv", encoding="utf-8", newline="") as f:
            for r in csv.DictReader(f, delimiter=";"):
                if r["instantie"] == "Raad van State" and r["jaar"].isdigit() and int(r["jaar"]) >= 2020:
                    rijen.append([r["jaar"], nl(int(r["n_uitspraken"])), nl(int(r["n_woningbouw"])), nl(int(r["woningen_in_geding"])),
                                  nl(int(r["n_stikstof_grond"])), nl(int(r["n_woningbouw_stikstof_grond"])), nl(int(r["n_woningbouw_stikstof_gegrond"])),
                                  nl(float(r["mediaan_dagen_woningbouw_besluit_uitspraak"])) if r["mediaan_dagen_woningbouw_besluit_uitspraak"] else ""])
        s += tabel(["jaar", "uitspraken", "over woningbouw", "woningen in het geding", "stikstof als grond, alle", "stikstof als grond, woningbouw",
                    "waarvan gegrond op stikstof", "mediaan dagen besluit tot uitspraak, woningbouw"], rijen,
                   "Gepubliceerde uitspraken van de Raad van State in het omgevingsrecht, verdiept gelezen, 2020 tot september 2026", links=1)
        return s

    pr = json.loads((map_ / "projecten_resultaten.json").read_text(encoding="utf-8")) if (map_ / "projecten_resultaten.json").exists() else {}

    def maak_projecten():
        """Grote woningbouwprojecten op projectniveau (projecten.py en analyse_projecten.R)."""
        if not pr:
            return ""
        pm = pr.get("meta", {})
        pj = pr.get("pr_per_jaar", [])
        tot = {0: [0, 0], 1: [0, 0]}
        for r in pj:
            tot[int(r["nabij"])][0] += int(r["projecten"])
            tot[int(r["nabij"])][1] += int(r["met_stikstofprocedure"])
        aandeel = lambda n: tot[n][1] / tot[n][0] if tot[n][0] else 0
        br = pr.get("pr_beroep", [])
        ster = lambda p: "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.1 else ""
        interval = lambda est, se: f"{nl(100 * (est - 1.96 * se), 1)} tot {nl(100 * (est + 1.96 * se), 1)}"
        s = f"""
    <div class="kern">
      <div class="tegel"><div class="waarde">{nl(pm.get('projecten_20', 0))}</div><div class="omschrijving">projecten van twintig woningen of meer, aanvraag juli 2021 tot september 2026</div></div>
      <div class="tegel"><div class="waarde">{nl(pm.get('woningen_20', 0))}</div><div class="omschrijving">woningen in die projecten</div></div>
      <div class="tegel"><div class="waarde">{pct(aandeel(1))}</div><div class="omschrijving">van de projecten nabij habitat met een stikstofprocedure binnen 500 m (ver {pct(aandeel(0))})</div></div>
      <div class="tegel"><div class="waarde">{nl(sum(int(r['stikstof_grond']) for r in br))}</div><div class="omschrijving">projecten met een uitspraak waarin stikstof als grond is aangevoerd (gegrond op stikstof: {nl(sum(int(r['stikstof_gegrond']) for r in br))})</div></div>
    </div>"""
        reeks = {0: [], 1: []}
        for r in sorted(pj, key=lambda r: int(r["jaar"])):
            reeks[int(r["nabij"])].append((int(r["jaar"]), 100 * float(r["aandeel_stikstofprocedure"])))
        s += grafiek_twee(reeks[1], reeks[0], ("nabij", "ver"), "Aandeel grote projecten met een stikstofprocedure binnen 500 meter en drie jaar, per jaar van aanvraag, nabij en ver van habitat",
                          "procent", "Procent van de projecten van twintig woningen of meer. Stikstofprocedure: een natuurbesluit van provincie of Rijk op een precieze locatie dat stikstof, AERIUS, Natura 2000, "
                          "een passende beoordeling of salderen noemt. 2021 vanaf juli, 2026 tot september.",
                          markeringen=[(2022.84, "Porthos"), (2024.0, "Omgevingswet"), (2024.96, "intern salderen")])
        s += tabel(["jaar aanvraag", "ligging", "projecten", "woningen", "met stikstofprocedure", "aandeel", "met soortenontheffing binnen 250 m", "met uitspraak", "BAG-vergunning gevonden"],
                   [[str(r["jaar"]), "nabij" if int(r["nabij"]) else "ver", nl(int(r["projecten"])), nl(int(r["woningen"])), nl(int(r["met_stikstofprocedure"])),
                     pct(r["aandeel_stikstofprocedure"]), nl(int(r["met_soortenprocedure"])), nl(int(r["met_beroep"])), pct(r["bag_vergund"])]
                    for r in sorted(pj, key=lambda r: (int(r["jaar"]), -int(r["nabij"])))],
                   "Projecten van twintig woningen of meer per jaar van aanvraag en ligging; de BAG-vergunning is voor de laatste jaren nog onvolledig", links=2)
        gr = pr.get("pr_per_grootte", [])
        if gr:
            orde = {"20 tot 49": 0, "50 tot 99": 1, "100 of meer": 2}
            s += tabel(["woningen per project", "ligging", "projecten", "woningen", "met stikstofprocedure", "met soortenontheffing", "met uitspraak"],
                       [[r["grootte"], "nabij" if int(r["nabij"]) else "ver", nl(int(r["projecten"])), nl(int(r["woningen"])), pct(r["aandeel_stikstofprocedure"]),
                         pct(r["aandeel_soortenprocedure"]), pct(r["aandeel_beroep"])] for r in sorted(gr, key=lambda r: (orde.get(r["grootte"], 9), -int(r["nabij"])))],
                       "Naar omvang van het project", links=2)
        tm = pr.get("pr_timing", [])
        if tm:
            s += tabel(["ligging", "projecten met stikstofprocedure", "eerste besluit voor de aanvraag", "mediaan dagen", "25e percentiel", "75e percentiel", "met woningbouwwoord binnen 1 km"],
                       [["nabij" if int(r["nabij"]) else "ver", nl(int(r["projecten"])), pct(r["aandeel_voor_aanvraag"]), nl(float(r["mediaan_dagen"])), nl(float(r["p25"])),
                         nl(float(r["p75"])), nl(int(r["met_woningbouwwoord_1km"]))] for r in sorted(tm, key=lambda r: -int(r["nabij"]))],
                       "Wanneer valt het eerste stikstofbesluit: dagen van de aanvraag tot het vroegste besluit binnen 500 meter, negatief is ervoor", links=1)
        uk = pr.get("pr_uitkomsten", [])
        if uk:
            s += tabel(["ligging", "stikstofprocedure", "projecten", "verleend", "geweigerd", "ingetrokken", "geen uitkomst", "BAG-vergunning gevonden", "mediaan dagen tot verleend", "mediaan dagen tot BAG-vergunning"],
                       [["nabij" if int(r["nabij"]) else "ver", "ja" if int(r["stikstofprocedure"]) else "nee", nl(int(r["projecten"])), pct(r["verleend"]), pct(r["geweigerd"]),
                         pct(r["ingetrokken"]), pct(r["geen"]), pct(r["bag_vergund"]), nl(float(r["mediaan_dagen_publicatie"])), nl(float(r["mediaan_dagen_bag"]))]
                        for r in sorted(uk, key=lambda r: (-int(r["nabij"]), int(r["stikstofprocedure"])))],
                       "Uitkomsten van projecten met minstens een jaar vervolg, naar ligging en stikstofprocedure", links=2)
        MAAT = {"verleend": "aandeel verleend (procentpunt)", "geweigerd": "aandeel geweigerd (procentpunt)", "ingetrokken": "aandeel ingetrokken (procentpunt)",
                "geen": "aandeel zonder uitkomst (procentpunt)", "bag_vergund": "BAG-vergunning gevonden (procentpunt)", "stikstofprocedure": "met stikstofprocedure (procentpunt)",
                "log(dagen aanvraag tot BAG-vergunning)": "dagen tot BAG-vergunning (procent)", "log(doorlooptijd)": "doorlooptijd tot verleend (procent)",
                "BAG-vergunning gevonden": "BAG-vergunning gevonden (procentpunt)"}
        rijen = []
        for r in pr.get("pr_did", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            m = r["model"]
            rijen.append([MAAT.get(m.split(" ~")[0].replace("feols ", ""), m), "12" if "12 of meer" in m else "20", html.escape(did_labels.get(r["term"], r["term"])),
                          nl(100 * est, 1) + ster(p), interval(est, se), nl(p, 3), nl(int(float(r["n"])))])
        s += tabel(["maat", "vanaf woningen", "term", "schatting", "95%-interval", "p", "n"], rijen,
                   "Difference-in-differences op de uitkomsten van grote projecten, gemeente maal nabij en kwartaal van aanvraag als vaste effecten, projecten met minstens een jaar vervolg", links=3)
        TERM = {"stikstofprocedure": "stikstofprocedure binnen 500 m", "soortenprocedure": "soortenontheffing binnen 250 m", "log(woningen)": "logaritme van het aantal woningen",
                "stikstofprocedure:nabij": "stikstofprocedure x nabij (extra nabij)"}
        rijen = []
        for r in pr.get("pr_verband", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            m = r["model"]
            maat = MAAT.get(m.split(" ~")[0].replace("feols ", ""), m) + (", met nabij apart" if "x nabij" in m else "")
            rijen.append([maat, html.escape(TERM.get(r["term"], r["term"])), nl(100 * est, 1) + ster(p), interval(est, se), nl(p, 3), nl(int(float(r["n"])))])
        s += tabel(["maat", "term", "schatting", "95%-interval", "p", "n"], rijen,
                   "Hoeveel langer met een stikstofprocedure: verband binnen gemeente en kwartaal bij gelijke omvang, geen effect", links=2)
        es = pr.get("pr_vastpanel_event_study", [])
        s += grafiek_event_study(es, f"Event study: grote projecten nabij habitat ten opzichte van ver, per jaar van aanvraag, vast panel van {nl(pm.get('vastpanel_gemeenten', 0))} gemeenten, tegenover 2018",
                                 ref="2018", events=[("2019", "PAS-uitspraak"), ("2023", "Porthos")], stap=1, grootheid="projecten van twintig woningen of meer (woningaantal uit de titel)", per="jaar")
        VT = {"nabij:overgang_2019": "nabij x 2019 (jaar van de uitspraak)", "nabij:post_pas": "nabij x vanaf 2020 (na de PAS-uitspraak)", "nabij:post_porthos": "nabij x vanaf 2023 (na Porthos)"}
        rijen = []
        for r in pr.get("pr_vastpanel_did", []):
            est, se, p = float(r["schatting"]), float(r["se"]), float(r["p"])
            m = r["model"]
            spec = ("woningen in die projecten" if m.startswith("fepois woningen") else "aantal projecten") + (", gemeente maal jaar" if "gemeente^jaar" in m else ", jaar")
            rijen.append([spec, html.escape(VT.get(r["term"], r["term"])), nl(100 * est, 1) + ster(p), interval(est, se), nl(p, 3), nl(int(float(r["n"])))])
        s += tabel(["grootheid en tijdseffect", "term", "schatting (procent)", "95%-interval", "p", "n"], rijen,
                   "Difference-in-differences op grote projecten in het vaste panel, 2016 tot 2025 (Poisson, gemeente maal nabij en jaar of gemeente maal jaar als vaste effecten)", links=2)
        cs = pr.get("pr_casus", [])
        if cs:
            s += tabel(["gemeente", "jaar", "woningen", "uitkomst", "dagen tot uitkomst", "dagen tot BAG-vergunning", "bouwstart volgens BAG", "stikstofbesluiten binnen 500 m", "eerste stikstofbesluit", "uitspraken"],
                       [[html.escape(r["gemeente"]), str(r["jaar"]), nl(int(r["woningen"])), html.escape(r["uitkomst"] or ""), nl(r["dagen_publicatie"]) if r.get("dagen_publicatie") is not None else "",
                         nl(r["dagen_tot_bagvergunning"]) if r.get("dagen_tot_bagvergunning") is not None else "", html.escape(r.get("bag_bouwstart") or ""),
                         nl(int(r["n_natuur_gebied_500m"])), html.escape(r.get("eerste_natuur_gebied") or ""), nl(int(r["beroep"]))] for r in cs[:25]],
                       "De 25 grootste projecten nabij habitat met een stikstofprocedure in de buurt", links=1)
        return s

    def maak_beroep_tabellen():
        beroep_uitk = res.get("beroep_uitkomst", [])
        duur = res.get("beroep_duur", [])
        s = tabel(["object", "uitkomst", "uitspraken"], [[html.escape(r["object"]), html.escape(r["uitkomst"]), nl(int(r["N"]))] for r in beroep_uitk],
                  "Uitkomst van beroepen naar object, gepubliceerde uitspraken 2010 tot 2026") if beroep_uitk else ""
        s += tabel(["object", "jaar", "uitspraken", "mediaan dagen besluit tot uitspraak", "p75"],
                   [[html.escape(r["object"]), r["jaar"], nl(int(r["n"])), nl(float(r["mediaan_dagen"])), nl(float(r["p75"]))] for r in duur],
                   "Tijd van bestreden besluit tot uitspraak, per object en jaar van de uitspraak") if duur else ""
        return s

    kern = f"""
    <div class="kern">
      <div class="tegel"><div class="waarde">{nl(tot_aanvr)}</div><div class="omschrijving">aanvragen van gemeenten sinds juli 2021{(' (' + nl(meta.get('zaken_alle_jaren', 0)) + ' sinds 2013)') if meta.get('zaken_alle_jaren') else ''}</div></div>
      <div class="tegel"><div class="waarde">{pct(tot_nabij / tot_aanvr if tot_aanvr else 0)}</div><div class="omschrijving">binnen 2.500 m van stikstofgevoelig habitat</div></div>
      <div class="tegel"><div class="waarde">{(nl(100 * float(porthos['schatting']), 1) + '%') if porthos else 'n.v.t.'}</div><div class="omschrijving">verschil in aanvragen nabij na Porthos (p = {nl(float(porthos['p']), 2) if porthos else ''})</div></div>
      <div class="tegel"><div class="waarde">{nl(meta.get('uitspraken', 0))}</div><div class="omschrijving">uitspraken van de bestuursrechter met tekst</div></div>
    </div>"""

    body = f"""<title>Vergunningen nabij Natura 2000</title>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Source+Serif+4:opsz,wght@8..60,500;8..60,600&family=Source+Sans+3:wght@400;600&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
:root {{ --bg:#F6F8F5; --vlak:#FFFFFF; --inkt:#1B211E; --inkt2:#4A5550; --gedempt:#7A8580; --lijn:#D9E0DB; --nabij:#1F6F4A; --ver:#5B6B8C; --nabijzacht:#DCEFE4; --verzacht:#E1E6F0; --event:#B0873A;
  --serif:"Source Serif 4", Georgia, "Times New Roman", serif; --sans:"Source Sans 3", "Segoe UI", system-ui, sans-serif; --mono:"IBM Plex Mono", Consolas, monospace; }}
@media (prefers-color-scheme: dark) {{ :root:not([data-theme="light"]) {{ color-scheme: dark; --bg:#141916; --vlak:#1C2320; --inkt:#E8ECE9; --inkt2:#B6C0BA; --gedempt:#8A948E; --lijn:#2A332E; --nabij:#4FB283; --ver:#8FA2C9; --nabijzacht:#1F3A2C; --verzacht:#252E3D; --event:#D3A85A; }} }}
:root[data-theme="dark"] {{ color-scheme: dark; --bg:#141916; --vlak:#1C2320; --inkt:#E8ECE9; --inkt2:#B6C0BA; --gedempt:#8A948E; --lijn:#2A332E; --nabij:#4FB283; --ver:#8FA2C9; --nabijzacht:#1F3A2C; --verzacht:#252E3D; --event:#D3A85A; }}
body {{ background:var(--bg); color:var(--inkt); font-family:var(--sans); font-size:17px; line-height:1.55; margin:0; padding-block:0 64px; padding-inline:16px; }}
.pagina {{ max-width:1320px; margin:0 auto; }}
header.kop {{ padding-block:40px 24px; border-bottom:1px solid var(--lijn); }}
.eyebrow {{ font-family:var(--mono); font-size:12px; letter-spacing:.08em; text-transform:uppercase; color:var(--gedempt); }}
h1 {{ font-family:var(--serif); font-weight:600; font-size:clamp(30px, 4.5vw, 44px); line-height:1.12; margin:8px 0 12px; text-wrap:balance; max-width:30ch; }}
h2 {{ font-family:var(--serif); font-weight:600; font-size:26px; margin:48px 0 12px; text-wrap:balance; }}
h3 {{ font-family:var(--sans); font-weight:600; font-size:16px; margin:24px 0 6px; color:var(--inkt2); }}
p {{ max-width:none; margin:0 0 14px; }}
.ondertitel {{ color:var(--inkt2); max-width:none; font-size:18px; }}
nav.inhoud {{ display:flex; flex-wrap:wrap; gap:6px 18px; padding-block:14px; font-size:14px; border-bottom:1px solid var(--lijn); position:sticky; top:env(safe-area-inset-top, 0px); background:var(--bg); z-index:2; }}
nav.inhoud a {{ color:var(--inkt2); text-decoration:none; }} nav.inhoud a:hover, nav.inhoud a:focus-visible {{ color:var(--nabij); text-decoration:underline; outline:none; }}
.kern {{ display:grid; grid-template-columns:repeat(auto-fit, minmax(190px, 1fr)); gap:12px; margin:24px 0 8px; }}
.tegel {{ background:var(--vlak); border:1px solid var(--lijn); border-radius:6px; padding:14px 16px; }}
.tegel .waarde {{ font-family:var(--mono); font-size:28px; font-weight:500; font-variant-numeric:tabular-nums; }}
.tegel .omschrijving {{ color:var(--inkt2); font-size:14px; margin-top:2px; }}
figure.grafiek {{ margin:20px 0 28px; background:var(--vlak); border:1px solid var(--lijn); border-radius:6px; padding:12px 12px 6px; }}
figure.grafiek figcaption {{ font-weight:600; font-size:15px; margin:0 0 6px 4px; }}
svg text {{ font-family:var(--sans); fill:var(--inkt2); font-size:13px; }} svg text.klein {{ font-size:11px; }} svg text.noot {{ font-size:12px; fill:var(--gedempt); }}
svg text.paneeltitel {{ font-weight:600; fill:var(--inkt); }} svg text.eventlabel {{ font-size:12px; fill:var(--event); paint-order:stroke; stroke:var(--vlak); stroke-width:4px; stroke-linejoin:round; }}
svg .rooster {{ stroke:var(--lijn); stroke-width:1; }} svg .nul {{ stroke:var(--inkt2); stroke-width:1; }} svg .event {{ stroke:var(--event); stroke-width:1.5; stroke-dasharray:4 4; }}
svg .interval {{ stroke:var(--nabij); stroke-width:2; }} svg .punt {{ fill:var(--nabij); stroke:var(--vlak); stroke-width:2; }} svg .punt.referentie {{ fill:var(--vlak); stroke:var(--nabij); }}
svg .lijn {{ fill:none; stroke-width:2.5; }} svg .lijn.nabij {{ stroke:var(--nabij); }} svg .lijn.ver {{ stroke:var(--ver); stroke-dasharray:7 5; }}
svg .punt.ver {{ fill:var(--ver); }} svg text.label {{ font-size:13px; }} svg text.label.nabij {{ fill:var(--nabij); font-weight:600; }} svg text.label.ver {{ fill:var(--ver); font-weight:600; }}
svg .staaf {{ rx:3; }} svg .staaf.nabij {{ fill:var(--nabij); }} svg .staaf.ver {{ fill:var(--ver); }}
.tabelwrap {{ overflow-x:auto; margin:12px 0 20px; }}
table.tabel {{ border-collapse:collapse; font-size:14px; min-width:520px; font-variant-numeric:tabular-nums; }}
table.tabel th, table.tabel td {{ text-align:right; padding:6px 10px; border-bottom:1px solid var(--lijn); white-space:nowrap; }}
table.tabel th:first-child, table.tabel td:first-child, table.tabel th.l, table.tabel td.l {{ text-align:left; }} table.tabel td.l {{ font-family:var(--sans); font-size:14px; }} table.tabel th {{ color:var(--inkt2); font-weight:600; }}
table.tabel td {{ font-family:var(--mono); font-size:13px; }} table.tabel td:first-child {{ font-family:var(--sans); font-size:14px; }}
.noot {{ color:var(--gedempt); font-size:14px; }}
.tabeltitel {{ font-weight:600; font-size:15px; margin:20px 0 4px; }}
footer {{ margin-top:56px; padding-top:16px; border-top:1px solid var(--lijn); color:var(--gedempt); font-size:13px; }}
@media (prefers-reduced-motion: no-preference) {{ nav.inhoud a {{ transition:color .15s; }} }}
</style>
<div class="pagina">
<header class="kop">
  <div class="eyebrow">Stikstof en bouwvergunningen, bekendmakingen 2016 tot 2026 en BAG 2012 tot 2026</div>
  <h1>Vergunningen nabij Natura 2000</h1>
  <p class="ondertitel">Aanvragen, uitkomsten, doorlooptijd en beroep uit de officiële bekendmakingen en de rechtspraak, nabij en ver van stikstofgevoelig habitat. Stand {html.escape(meta.get('datum', ''))}.</p>
</header>
<nav class="inhoud"><a href="#samenvatting">Samenvatting</a><a href="#data">Data</a><a href="#methode">Methode</a><a href="#aanvragen">Aanvragen</a><a href="#uitkomsten">Uitkomsten</a><a href="#gradient">Afstand</a><a href="#soort">Projecttype</a><a href="#aerius">AERIUS</a>{'<a href="#oud">Vast panel</a>' if res.get("oud_event_study_aanvragen") else ''}{'<a href="#strikt">Strenge vergelijking</a>' if vp else ''}<a href="#bag">BAG sinds 2012</a>{'<a href="#blootstelling">Dosis en voortrend</a>' if bl else ''}{'<a href="#projecten">Grote projecten</a>' if pr else ''}<a href="#beroep">Beroep</a><a href="#beperkingen">Beperkingen</a><a href="#vervolg">Vervolg</a></nav>
{sectie("samenvatting", "Samenvatting", kern)}
{sectie("data", "Data", maak_validatie())}
{sectie("methode", "Methode")}
{sectie("aanvragen", "Aanvragen nabij en ver", grafiek_reeksen(besch, "Aanvragen per kwartaal, binnen en buiten 2.500 meter van stikstofgevoelig habitat") + grafiek_event_study(res.get("event_study_aanvragen", []), "Event study: aanvragen nabij ten opzichte van ver, per kwartaal") + (tabel_coef(res.get("did_aanvragen", []), did_labels, "Difference-in-differences op aanvragen: Poisson met gemeente maal nabij en kwartaal als vaste effecten, standaardfouten geclusterd op gemeente") if res.get("did_aanvragen") else "") + (tabel_coef(res.get("did_bouwaanvragen", []), did_labels, "Hetzelfde model, alleen zaken met een bouwwerk") if res.get("did_bouwaanvragen") else ""))}
{sectie("uitkomsten", "Uitkomsten en doorlooptijd", maak_uitk_tabel())}
{sectie("gradient", "De afstandsgradiënt", tabel(["afstand tot habitat", "aanvragen", "verleend", "geweigerd", "ingetrokken", "geen uitkomst", "mediaan dagen", "verlengingen per 100"], klasse_rijen, "Ruwe verschillen per afstandsklasse, aanvragen van gemeenten juli 2021 tot september 2026") + grafiek_gradient(res.get("gradient_klasse", []), "Verschil per afstandsklasse ten opzichte van verder dan 5 km, met gemeente- en kwartaaleffecten"))}
{sectie("soort", "Nieuwbouw tegenover klein werk", maak_soort_tabellen())}
{sectie("aerius", "Blootstelling volgens AERIUS", maak_aerius_tabellen())}
{sectie("oud", "De PAS-uitspraak in de bekendmakingen: het vaste panel", maak_oud()) if res.get("oud_event_study_aanvragen") else ""}
{sectie("strikt", "De strenge vergelijking: binnen gemeente en kwartaal, 2016 tot 2026", maak_vastpanel()) if vp else ""}
{sectie("bag", "Van vergunning naar bouwstart: de BAG sinds 2012", maak_bag()) if bag else ""}
{sectie("blootstelling", "Dosis, voortrend en andere remmen", maak_blootstelling()) if bl else ""}
{sectie("projecten", "Grote projecten op projectniveau", maak_projecten()) if pr else ""}
{sectie("beroep", "Beroep bij de bestuursrechter", grafiek_beroep(res.get("beroep_per_jaar_object", []), res.get("beroep_stikstof_per_jaar", []), "Gepubliceerde uitspraken per jaar over omgevingsvergunningen en plannen") + grafiek_stikstof(res.get("beroep_stikstof_per_jaar", []), "Aandeel uitspraken met stikstof als grond, per jaar") + maak_beroep_tabellen() + maak_rechtspraak_verdieping())}
{sectie("beperkingen", "Beperkingen")}
{sectie("vervolg", "Vervolg")}
<footer>Bron: officiële bekendmakingen via repository.overheid.nl (CC0), Rechtspraak Open Data, AERIUS relevante habitatkartering (connect.aerius.nl), LV BAG-extract van het Kadaster via PDOK (CC0), CBS StatLine. Schattingen met fixest {html.escape(str(meta.get('fixest', '')))}; standaardfouten geclusterd op gemeente; * p onder 0,10, ** onder 0,05, *** onder 0,01. Pijplijn en documentatie in de map vergunningen van de repository Stikstof.</footer>
</div>
"""
    Path(uit_pad).write_text(body, encoding="utf-8")
    print(f"geschreven: {uit_pad} ({len(body)} tekens)")


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print(__doc__)
        sys.exit(2)
    bouw(sys.argv[1], sys.argv[2], sys.argv[3])
