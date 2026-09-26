"""Toetst de zaken uit de bekendmakingen aan twee onafhankelijke tellingen.

1. DSO-LV (Kadaster, Monitor Werking Omgevingswet): het aantal ingediende aanvragen omgevingsvergunning bij gemeenten per
   maand, januari 2024 tot en met december 2025, tegenover het aantal zaken met een aanvraagpublicatie per maand.
2. De open vergunningregisters van Den Haag (Wabo, 2019 tot januari 2024) en Eindhoven (Wabo en Omgevingswet): per maand
   van indiening het aantal aanvragen en de mediane tijd van indiening tot besluit, tegenover de zaken van dezelfde
   gemeente en de mediane tijd van de aanvraagpublicatie tot de uitkomstpublicatie.
Uitvoer: analyse\\validatie_dso_maand.csv en analyse\\validatie_registers_maand.csv, met de sommen op het scherm.
Gebruik: py -3.13 valideer_registers.py
"""
import csv
import statistics
import sys
from collections import defaultdict

from config import DATA_DIR

ZAKEN = DATA_DIR / "analyse" / "zaken.csv"
UITV = DATA_DIR / "bron" / "uitvoering"
DSO = UITV / "dso_lv_maand_bestuurslaag_2024_2025.csv"
REG = UITV / "gemeenten" / "doorlooptijden_gemeenten_per_maand.csv"
UIT_DSO = DATA_DIR / "analyse" / "validatie_dso_maand.csv"
UIT_REG = DATA_DIR / "analyse" / "validatie_registers_maand.csv"
CREATOR = {"Den Haag": "'s-Gravenhage", "Eindhoven": "Eindhoven"}


def main(argv):
    per_maand = defaultdict(int)
    per_gem = defaultdict(lambda: defaultdict(list))  # creator -> maand -> doorlooptijden verleend
    n_gem = defaultdict(lambda: defaultdict(int))
    with open(ZAKEN, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            m = r["datum_aanvraag"][:7]
            per_maand[m] += 1
            if r["gemeente"] in CREATOR.values():
                n_gem[r["gemeente"]][m] += 1
                if r["verleend"] == "1" and r["doorlooptijd"]:
                    per_gem[r["gemeente"]][m].append(float(r["doorlooptijd"]))
    dso = {}
    with open(DSO, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            if r["indicator"] == "aanvragen_omgevingsvergunning" and r["bestuurslaag"] == "gemeente" and r["maand"]:
                dso[f"{r['jaar']}-{int(r['maand']):02d}"] = float(r["waarde"].replace(",", "."))
    with open(UIT_DSO, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["maand", "dso_aanvragen_gemeenten", "zaken_met_aanvraagpublicatie", "verhouding"])
        tot_d = tot_z = 0
        for m in sorted(dso):
            z = per_maand.get(m, 0)
            tot_d += dso[m]
            tot_z += z
            w.writerow([m, int(dso[m]), z, round(z / dso[m], 3) if dso[m] else ""])
    print(f"DSO-LV gemeenten 2024 tot 2025: {int(tot_d)} aanvragen, zaken met aanvraagpublicatie {tot_z} ({100 * tot_z / max(tot_d, 1):.1f} procent), {len(dso)} maanden")
    reg = defaultdict(lambda: defaultdict(lambda: [0, []]))
    with open(REG, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            s = r["stelsel_of_type"].lower()
            if not ("aanvraag" in s or "omgevingsvergunning" in s or "wabo" in s or "omgevingswet" in s) or "beginsel" in s:
                continue
            g, m = r["gemeente"], r["maand_indiening"]
            reg[g][m][0] += int(r["n_ingediend"] or 0)
            if r["mediaan_dagen"]:
                reg[g][m][1].append((float(r["mediaan_dagen"]), int(r["n_met_einddatum"] or 0)))
    with open(UIT_REG, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["gemeente", "maand", "register_ingediend", "zaken", "register_mediaan_dagen", "zaken_mediaan_dagen_verleend"])
        for g, maanden in reg.items():
            cr = CREATOR.get(g)
            per_jaar = defaultdict(lambda: [0, 0, [], []])
            for m in sorted(maanden):
                n_reg, medianen = maanden[m]
                # gewogen mediaan over de typen in die maand: benadering met het gewogen gemiddelde van de medianen
                med_reg = sum(a * b for a, b in medianen) / max(sum(b for _, b in medianen), 1) if medianen else None
                dl = per_gem[cr][m] if cr else []
                med_z = statistics.median(dl) if dl else None
                w.writerow([g, m, n_reg, n_gem[cr][m] if cr else "", "" if med_reg is None else round(med_reg, 1), "" if med_z is None else med_z])
                pj = per_jaar[m[:4]]
                pj[0] += n_reg
                pj[1] += n_gem[cr][m] if cr else 0
                if med_reg is not None:
                    pj[2].append(med_reg)
                if med_z is not None:
                    pj[3].append(med_z)
            for j in sorted(per_jaar):
                a, b, c, d = per_jaar[j]
                print(f"{g} {j}: register {a} aanvragen, zaken {b} ({100 * b / max(a, 1):.0f} procent); mediaan dagen register {statistics.median(c) if c else '-'}, zaken {statistics.median(d) if d else '-'}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
