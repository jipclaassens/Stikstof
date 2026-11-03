"""Koppelt de woningen in het Vinex-raster aan de 71 locaties in realisatie_VINEX.xlsx, per CBS-wijk.

Per rastergebied ging het mis waar twee Vinex-locaties tegen elkaar aan liggen (Ypenburg en
Leidschenveen, Vathorst en Nieuwland). Per wijk is dat op te lossen, want de wijk draagt vaak de
Vinex-naam. Volgorde van bewijs per wijk:
  1. een naam van de locatie zit in de wijknaam;
  2. anders: het aandeel woningen in de wijk waarvan de buurtnaam een naam van de locatie bevat;
  3. anders: de locatienaam noemt de gemeente (Almere, Haarlemmermeer) en er is in die gemeente geen
     andere locatie met een betere treffer.
Wat daarna nog over is, staat in het tabblad Niet gekoppeld, om met de hand toe te wijzen.

Naast de telling binnen het raster telt het script ook alle nieuwbouw vanaf 1995 in de gekoppelde
wijken, binnen en buiten het raster. Het raster is een plangrens; bij Waalsprong dekt het alleen een
deel van Lent, terwijl Ressen en Oosterhout in dezelfde wijk Nijmegen-Noord liggen. Die tweede maat
staat in eigen tabbladen; de tabbladen van de telling binnen het raster blijven zoals ze waren.

Uitvoer: vinex_realisatie_20260710.xlsx, vinex_woonobjecten_20260710_nl.csv en
woonobjecten_hele_wijk_20260710_nl.csv (puntkomma, decimale komma).
"""
import json
import os
import re
import unicodedata

import numpy as np
import pandas as pd
from shapely import STRtree, points
from shapely.geometry import shape

MAP = r"C:\LocalData\Stikstof\Vinex"
EXCEL = os.environ.get("VINEX_EXCEL", r"C:\Users\Jip\Downloads\realisatie_VINEX.xlsx")   # de tabel met de 71 Vinex-locaties
NIEUWBOUW = f"{MAP}/woonobjecten_vanaf1995_20260710.parquet"   # GeoDMS-export /Analyse/Vinex/NieuwbouwNL
CACHE = f"{MAP}/cbs_buurten_vinex.json"                         # de wijken en buurten die 02_buurten.py ophaalde
MIN_AANDEEL_BUURT = 0.25
MIN_WOORD = 6
VANAF, TOT = 1995, 2026
GENERIEK = {"noord", "zuid", "oost", "west", "nieuw", "nieuwe", "stad", "polder", "en", "de", "het", "van", "t",
            "plaslocatie", "locatie"}


def norm(s):
    s = unicodedata.normalize("NFKD", "" if pd.isna(s) else str(s)).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def termen(naam):
    """De hele naam, plus elk niet-generiek woord van zes letters of meer. Korte woorden als 'veld'
    of 'zand' staan in te veel buurtnamen; die tellen alleen als deel van de hele naam."""
    n = norm(naam)
    t = {n} if len(n) >= 4 else set()
    t |= {w for w in n.split() if len(w) >= MIN_WOORD and w not in GENERIEK}
    return t


obj = pd.read_csv(f"{MAP}/vinex_woonobjecten_buurt.csv", dtype={"vbo_id": str, "pand_id": str, "postcode": str, "buurtcode": str})
loc = pd.read_excel(EXCEL, header=None, skiprows=2, usecols=range(7),
                    names=["locatie", "doel", "gerealiseerd_9599", "gerealiseerd_0004", "gerealiseerd_9504", "vinexgebied", "doelbereik"])
loc = loc[loc["locatie"].notna()].reset_index(drop=True)
loc["termen"] = loc["locatie"].map(termen)

obj["wijk_sleutel"] = obj["gemeente"].fillna("") + " | " + obj["wijk"].fillna("")

# Locaties die in de CBS-indeling van 2024 een andere naam dragen. Elk is te controleren aan de
# buurtnamen in het tabblad 'Per wijk per jaar'.
HANDMATIG = {
    "Nijmegen | Nijmegen-Noord":                 "Waalsprong",       # buurten Lent, Ressen, Oosterhout
    "Houten | Houten Zuid-West":                 "Castellum",        # Houten-Zuid
    "Houten | Houten Zuid-Oost":                 "Castellum",
    "Zaanstad | Assendelft-Noord":               "Saendelft",        # buurten Parkrijk, Waterrijk
    "Enschede | Wijk 08 Glanerbrug en omgeving": "Eschmarke",        # buurten Eilermarke, Oikos, Eekmaat
    "Utrecht | Wijk 10 Vleuten-De Meern":        "Leidsche Rijn",    # buurten Vleuterweide; alleen het deel binnen het raster
}
obj["buurt_n"] = obj["buurt"].map(norm)

koppeling = {}      # wijk_sleutel -> (locatie-index, bewijs)
loc_index = {naam: i for i, naam in loc["locatie"].items()}
# per gemeente: de enige locatie die die gemeente in haar naam noemt, anders niets (Groningen heeft er drie)
gemeente_uniek = {}
for gem_n in obj["gemeente"].map(norm).unique():
    noemers = [i for i, r in loc.iterrows() if any(t == gem_n or t in gem_n.split() for t in r["termen"])]
    if len(noemers) == 1:
        gemeente_uniek[gem_n] = noemers[0]
for sleutel, naam in HANDMATIG.items():
    koppeling[sleutel] = (loc_index[naam], "handmatig, te controleren")
for sleutel, d in obj.groupby("wijk_sleutel"):
    if sleutel in koppeling:
        continue
    gem_n, wijk_n = norm(d["gemeente"].iloc[0]), norm(d["wijk"].iloc[0])
    beste = None
    for i, r in loc.iterrows():
        if any(t in wijk_n for t in r["termen"]):
            kandidaat = (3.0, i, "wijknaam")
        else:
            a = d["buurt_n"].map(lambda b: any(t in b for t in r["termen"])).mean()
            if a >= MIN_AANDEEL_BUURT:
                kandidaat = (2.0 + a, i, f"buurtnaam ({a:.0%})")
            elif any(t == gem_n or t in gem_n.split() for t in r["termen"]) and gemeente_uniek.get(gem_n) == i:
                kandidaat = (1.0, i, "gemeentenaam")
            else:
                continue
        if beste is None or kandidaat[0] > beste[0]:
            beste = kandidaat
    if beste:
        koppeling[sleutel] = (beste[1], beste[2])

obj["loc_i"] = obj["wijk_sleutel"].map(lambda s: koppeling.get(s, (None, None))[0])
obj["bewijs"] = obj["wijk_sleutel"].map(lambda s: koppeling.get(s, (None, None))[1])

# per locatie
rij_l = []
for i, r in loc.iterrows():
    d = obj[obj["loc_i"] == i]
    vanaf = d[d["pand_bouwjaar"] >= VANAF]
    per_jaar = vanaf.groupby("pand_bouwjaar").size().reindex(range(VANAF, TOT + 1), fill_value=0)
    cum = per_jaar.cumsum()
    gehaald = next((int(j) for j, c in cum.items() if c >= r["doel"]), None) if len(d) else None
    wijken = d.groupby("wijk_sleutel").size().sort_values(ascending=False)
    heeft = len(d) > 0

    def periode(van, tot):
        return int(vanaf["pand_bouwjaar"].between(van, tot).sum()) if heeft else None

    # dezelfde opzet als de Excel van Huub, maar dan uit de BAG, en doorgetrokken tot nu
    uit = {"locatie": r["locatie"], "vinexgebied": r["vinexgebied"], "doel": r["doel"],
           "1995_1999": periode(1995, 1999), "2000_2004": periode(2000, 2004),
           "totaal_1995_2004": periode(1995, 2004),
           "doelbereik_2004": round(periode(1995, 2004) / r["doel"], 3) if heeft else None,
           "2005_2009": periode(2005, 2009), "2010_2014": periode(2010, 2014),
           "2015_2019": periode(2015, 2019), "2020_2026": periode(2020, TOT),
           "totaal_vanaf_1995": int(len(vanaf)) if heeft else None,
           "doelbereik_nu": round(len(vanaf) / r["doel"], 3) if heeft else None,
           "doel_gehaald_in": gehaald, "jaren_vanaf_1995": (gehaald - VANAF + 1) if gehaald else None,
           "excel_1995_1999": r["gerealiseerd_9599"], "excel_2000_2004": r["gerealiseerd_0004"],
           "excel_1995_2004": r["gerealiseerd_9504"],
           "cbs_wijken": "; ".join(f"{k} ({v})" for k, v in wijken.head(6).items()),
           "bewijs": "; ".join(sorted(d["bewijs"].dropna().unique()))}
    for j in range(VANAF, TOT + 1):
        uit[f"cum_{j}"] = int(cum[j]) if heeft else None
    rij_l.append(uit)
per_locatie = pd.DataFrame(rij_l)
per_locatie["bag_1995_2004"] = per_locatie["totaal_1995_2004"]

# niet gekoppelde wijken, met hun woningen vanaf 1995
nk = obj[obj["loc_i"].isna()]
niet = (nk.groupby(["wijk_sleutel"]).agg(woningen=("vbo_id", "size"),
                                          vanaf_1995=("pand_bouwjaar", lambda s: int((s >= VANAF).sum())),
                                          rastergebieden=("vinex_gebied", lambda s: ", ".join(str(x) for x in sorted(s.unique()))),
                                          buurten=("buurt", lambda s: ", ".join(s.value_counts().head(4).index)))
        .sort_values("vanaf_1995", ascending=False).reset_index())

# per CBS-wijk per jaar (lang), voor eigen herindeling
pwj = (obj[obj["pand_bouwjaar"] >= 1990].groupby(["wijk_sleutel", "pand_bouwjaar"]).size()
       .rename("woningen").reset_index())
pwj["locatie"] = pwj["wijk_sleutel"].map(lambda s: loc.loc[koppeling[s][0], "locatie"] if s in koppeling else "")

# Tweede maat: alle woningen vanaf 1995 in de gekoppelde CBS-wijken, ook buiten het raster.
# De wijkcode volgt uit de buurtcode (BU + gemeente + wijk + buurt), de wijkgrens uit de cache van 02_buurten.py.
wijken_obj = pd.DataFrame({"wijkcode": "WK" + obj["buurtcode"].fillna("").str[2:8], "wijk_sleutel": obj["wijk_sleutel"]})
sleutel_van_code = (wijken_obj[wijken_obj["wijk_sleutel"].isin(koppeling)].drop_duplicates("wijkcode")
                    .set_index("wijkcode")["wijk_sleutel"].to_dict())
with open(CACHE, encoding="utf-8") as f:
    cache = json.load(f)
wijk_geo = {}
for gebied in cache.values():
    for ft in gebied["wijken"]:
        code = ft["properties"]["wijkcode"]
        if code in sleutel_van_code and code not in wijk_geo:
            wijk_geo[code] = shape(ft["geometry"])
ontbreekt = sorted(set(sleutel_van_code) - set(wijk_geo))
if ontbreekt:
    raise SystemExit(f"geen wijkgrens in {CACHE} voor {ontbreekt}")

nb = pd.read_parquet(NIEUWBOUW)
nb = nb[nb["pand_bouwjaar"].between(VANAF, TOT)].reset_index(drop=True)
codes = list(wijk_geo)
ip, iw = STRtree([wijk_geo[c] for c in codes]).query(points(nb[["x", "y"]].to_numpy()), predicate="within")
nb["wijkcode"] = None
nb.loc[ip, "wijkcode"] = np.array(codes, dtype=object)[iw]
nb = nb[nb["wijkcode"].notna()].copy()
nb["wijk_sleutel"] = nb["wijkcode"].map(sleutel_van_code)
nb["loc_i"] = nb["wijk_sleutel"].map(lambda s: koppeling[s][0])
nb["in_raster"] = nb["vinex_gebied"] > 0

rij_w = []
for i, r in loc.iterrows():
    d = nb[nb["loc_i"] == i]
    heeft = pd.notna(per_locatie.loc[i, "totaal_vanaf_1995"])
    per_jaar = d.groupby("pand_bouwjaar").size().reindex(range(VANAF, TOT + 1), fill_value=0)
    cum = per_jaar.cumsum()
    gehaald = next((int(j) for j, c in cum.items() if c >= r["doel"]), None) if heeft else None
    buiten = int((~d["in_raster"]).sum())

    def periode(van, tot):
        return int(d["pand_bouwjaar"].between(van, tot).sum()) if heeft else None

    uit = {"locatie": r["locatie"], "vinexgebied": r["vinexgebied"], "doel": r["doel"],
           "raster_vanaf_1995": per_locatie.loc[i, "totaal_vanaf_1995"],
           "raster_doelbereik_nu": per_locatie.loc[i, "doelbereik_nu"],
           "raster_doel_gehaald_in": per_locatie.loc[i, "doel_gehaald_in"],
           "wijk_1995_1999": periode(1995, 1999), "wijk_2000_2004": periode(2000, 2004),
           "wijk_2005_2009": periode(2005, 2009), "wijk_2010_2014": periode(2010, 2014),
           "wijk_2015_2019": periode(2015, 2019), "wijk_2020_2026": periode(2020, TOT),
           "wijk_vanaf_1995": int(len(d)) if heeft else None,
           "wijk_doelbereik_nu": round(len(d) / r["doel"], 3) if heeft else None,
           "wijk_doel_gehaald_in": gehaald, "wijk_jaren_vanaf_1995": (gehaald - VANAF + 1) if gehaald else None,
           "buiten_raster_vanaf_1995": buiten if heeft else None,
           "aandeel_buiten_raster": round(buiten / len(d), 3) if heeft and len(d) else None,
           "controle_raster_uit_landelijke_export": int(d["in_raster"].sum()) if heeft else None,
           "cbs_wijken": "; ".join(f"{k} ({v})" for k, v in d.groupby("wijk_sleutel").size().sort_values(ascending=False).items()),
           "bewijs": per_locatie.loc[i, "bewijs"]}
    for j in range(VANAF, TOT + 1):
        uit[f"wijk_cum_{j}"] = int(cum[j]) if heeft else None
    rij_w.append(uit)
per_locatie_wijk = pd.DataFrame(rij_w)

# per gekoppelde wijk: hoeveel van de nieuwbouw binnen en buiten het raster ligt
wijken_rw = (nb.groupby("wijk_sleutel").agg(wijkcode=("wijkcode", "first"),
                                             in_raster_vanaf_1995=("in_raster", "sum"),
                                             hele_wijk_vanaf_1995=("vbo_id", "size"))
             .reset_index())
wijken_rw["buiten_raster_vanaf_1995"] = wijken_rw["hele_wijk_vanaf_1995"] - wijken_rw["in_raster_vanaf_1995"]
wijken_rw["aandeel_buiten_raster"] = (wijken_rw["buiten_raster_vanaf_1995"] / wijken_rw["hele_wijk_vanaf_1995"]).round(3)
wijken_rw.insert(0, "locatie", wijken_rw["wijk_sleutel"].map(lambda s: loc.loc[koppeling[s][0], "locatie"]))
wijken_rw["bewijs"] = wijken_rw["wijk_sleutel"].map(lambda s: koppeling[s][1])
wijken_rw = wijken_rw.sort_values(["locatie", "hele_wijk_vanaf_1995"], ascending=[True, False])

uitleg = pd.DataFrame({"Toelichting": [
    "Bron woningen: BAG van 10 juli 2026, alle verblijfsobjecten met een woonfunctie die nu in de voorraad staan.",
    "Bouwjaar: het bouwjaar van het pand. Woningen die na de bouw zijn gesloopt, tellen niet mee.",
    "Vinex-begrenzing: VinexPlans.tif (raster van 100 m, 16.663 ha); een object telt mee als zijn punt in een Vinex-cel valt.",
    "Koppeling aan de locaties: via de CBS-wijk (wijk- en buurtkaart 2024). Bewijs per locatie staat in de kolom 'bewijs'.",
    "Kolommen 1995_1999 tot en met 2020_2026: woningen naar bouwjaar uit de BAG, in dezelfde opzet als realisatie_VINEX.xlsx maar doorgetrokken tot nu, zodat de hele reeks uit een bron komt.",
    "Doelbereik: woningen vanaf 1995 gedeeld door het Vinex-doel; 1 is het doel gehaald. Doel gehaald in: het eerste bouwjaar waarin dat gebeurde.",
    "Kolommen cum_1995 tot en met cum_2026: het aantal woningen vanaf 1995, opgeteld tot en met dat jaar.",
    "Kolommen excel_*: de oorspronkelijke cijfers uit realisatie_VINEX.xlsx, alleen ter controle. totaal_1995_2004 hoort daar dicht bij te liggen; een groot verschil wijst op een verkeerde of onvolledige koppeling.",
    "Niet gekoppelde CBS-wijken staan in het tabblad 'Niet gekoppeld'; die zijn met de hand toe te wijzen via 'Per wijk per jaar'.",
    "Tabblad 'Per locatie, hele wijk' (toegevoegd op 3 november 2026): dezelfde locaties, maar nu met alle woningen vanaf 1995 in de gekoppelde CBS-wijken, ook buiten het Vinex-raster. Het raster is een plangrens; bij sommige locaties ligt een deel van de latere bebouwing erbuiten, bij Waalsprong bijvoorbeeld Ressen en Oosterhout.",
    "Die tweede maat telt ook nieuwbouw in de wijk die geen Vinex is, zoals inbreiding of een uitbreiding van een oude dorpskern. Beide maten staan daarom naast elkaar: raster_* is de telling binnen het raster uit 'Per locatie', wijk_* de telling in de hele wijk. aandeel_buiten_raster laat zien hoeveel van de wijkmaat buiten het raster valt; een hoog aandeel vraagt om een blik op de kaart.",
    "controle_raster_uit_landelijke_export: de woningen binnen het raster opnieuw geteld uit de landelijke export; hoort gelijk te zijn aan raster_vanaf_1995.",
    "Tabblad 'Wijken, raster en hele wijk': per gekoppelde CBS-wijk de woningen vanaf 1995 binnen het raster en in de hele wijk.",
]})

uit = f"{MAP}/vinex_realisatie_20260710.xlsx"
with pd.ExcelWriter(uit, engine="openpyxl") as w:
    uitleg.to_excel(w, sheet_name="Toelichting", index=False)
    per_locatie.drop(columns=["bag_1995_2004"]).to_excel(w, sheet_name="Per locatie", index=False)
    niet.to_excel(w, sheet_name="Niet gekoppeld", index=False)
    pwj.to_excel(w, sheet_name="Per wijk per jaar", index=False)
    per_locatie_wijk.to_excel(w, sheet_name="Per locatie, hele wijk", index=False)
    wijken_rw.to_excel(w, sheet_name="Wijken, raster en hele wijk", index=False)
print(f"geschreven: {uit}")

# de woningen vanaf 1995 in de gekoppelde wijken, binnen en buiten het raster
nw = nb.drop(columns=["loc_i"]).copy()
nw["vinex_locatie"] = nb["loc_i"].map(lambda i: loc.loc[i, "locatie"])
nw.to_csv(f"{MAP}/woonobjecten_hele_wijk_20260710_nl.csv", sep=";", decimal=",", index=False, encoding="utf-8-sig")
print("geschreven: woonobjecten_hele_wijk_20260710_nl.csv")

# de objecten voor een Nederlandse Excel: puntkomma en decimale komma
nl = obj.drop(columns=["wijk_sleutel", "buurt_n", "loc_i", "bewijs"]).copy()
nl["vinex_locatie"] = obj["loc_i"].map(lambda i: loc.loc[i, "locatie"] if pd.notna(i) else "")
nl.to_csv(f"{MAP}/vinex_woonobjecten_20260710_nl.csv", sep=";", decimal=",", index=False, encoding="utf-8-sig")
print("geschreven: vinex_woonobjecten_20260710_nl.csv")

ok = per_locatie.dropna(subset=["bag_1995_2004"])
ok = ok[ok["excel_1995_2004"] >= 200]
binnen = int(((ok["bag_1995_2004"] - ok["excel_1995_2004"]).abs() <= 0.15 * ok["excel_1995_2004"]).sum())
print(f"\ncontrole: van de gekoppelde locaties met minstens 200 woningen in de Excel liggen er {binnen} van {len(ok)} binnen 15 procent")
print(f"locaties gekoppeld: {per_locatie['bag_1995_2004'].notna().sum()} van {len(per_locatie)}")
print(f"woningen vanaf 1995 gekoppeld: {int((obj['loc_i'].notna() & (obj['pand_bouwjaar'] >= VANAF)).sum()):,} "
      f"van {int((obj['pand_bouwjaar'] >= VANAF).sum()):,}")
print("\nlocatie | doel | Excel 95-04 | BAG 95-04 | doel gehaald | bewijs")
for _, r in per_locatie.sort_values("doel", ascending=False).iterrows():
    print(f"  {r['locatie'][:30]:<30} {r['doel']:>6.0f} {r['excel_1995_2004']:>7.0f} {str(r['bag_1995_2004']):>7} "
          f"{str(r['doel_gehaald_in']):>6}  {r['bewijs'][:40]}")
print("\ngrootste niet gekoppelde wijken (woningen vanaf 1995):")
print(niet.head(15).to_string(index=False))

pw = per_locatie_wijk.dropna(subset=["wijk_vanaf_1995"])
afwijkend = pw[pw["controle_raster_uit_landelijke_export"] != pw["raster_vanaf_1995"]]
print(f"\ncontrole: raster uit de landelijke export gelijk aan 'Per locatie' bij {len(pw) - len(afwijkend)} van {len(pw)} locaties")
if len(afwijkend):
    print(afwijkend[["locatie", "raster_vanaf_1995", "controle_raster_uit_landelijke_export"]].to_string(index=False))
print(f"woningen vanaf 1995 in de gekoppelde wijken: {int(pw['wijk_vanaf_1995'].sum()):,}, "
      f"waarvan buiten het raster {int(pw['buiten_raster_vanaf_1995'].sum()):,}")
print("\nlocatie | doel | raster | hele wijk | aandeel buiten raster | doel gehaald: raster, hele wijk")
for _, r in pw.sort_values("aandeel_buiten_raster", ascending=False).iterrows():
    print(f"  {r['locatie'][:30]:<30} {r['doel']:>6.0f} {r['raster_vanaf_1995']:>7.0f} {r['wijk_vanaf_1995']:>7.0f} "
          f"{r['aandeel_buiten_raster']:>6.0%}   {str(r['raster_doel_gehaald_in']):>6} {str(r['wijk_doel_gehaald_in']):>6}")
