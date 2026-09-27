"""Eerste datum van netcongestie voor afname, per postcode (PC6).

Gebruik (standaard-Python met pandas):
  py -3.13 R/14b_congestiedatum.py <uitvoer.csv> [datamap]

uitvoer.csv  een rij per PC6 met de datum en de bron (zie onder)
datamap      de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

Waarom per postcode
---------------------------------------------------------------------
Hoofdstuk 16 van het rapport vergeleek plekken die op 17 april 2024 al rood
waren met plekken die dat later of nooit werden. Dat mist de datum waarop een
gebied vol raakte, en het telde oranje (de formele melding bij de ACM, waarmee
een wachtlijst begint) als niet vol. Dit script zet per PC6 de eerste datum
waarop afname niet meer vrij kon, uit vier bronnen in bron/netcongestie:

  1. TenneT: de vooraankondiging per congestiegebied voor afname op het
     hoogspanningsnet (ACM-register, TenneT-documenten, beleid/tijdlijn.csv).
  2. Regionale netbeheerders: de vooraankondigingen afname in het ACM-register
     (acm_register_vooraankondigingen_compact.csv), gekoppeld op
     voedingsgebied_id: Enexis via intern_id, Stedin via de CM-code, Liander
     alleen waar intern_id letterlijk een voedingsgebied is.
  3. Enexis: de startdatum van het congestieonderzoek afname per gebied
     (enexis_congestieonderzoeken_details_20260926.csv, veld teksten),
     gekoppeld op de naam van het voedingsgebied.
  4. De historische kaartstanden per PC6 (capaciteitskaart_historische_standen.gpkg,
     tabel_pc6_standen_2023_2024): de eerste stand met afname 2 of hoger. Dat is
     een bovengrens: het gebied was op die dag al vol, maar misschien al eerder.

De datum is de vroegste van 1 tot en met 3. Ligt de eerste stand van 4 daarvoor
of is er geen datum, dan wordt die stand de datum en is de datum links
gecensureerd ('op of voor'). De koppeling van PC6 aan voedingsgebied en
TenneT-gebied is die van 21 september 2026 (tabel_congestie_pc6).

Afname-codes op de kaart: 0 of lager ruimte of onbekend, 1 beperkt, 2 oranje
(vooraankondiging bij de ACM, wachtlijst), 3 rood (geen ruimte).
"""
import os
import re
import sqlite3
import sys
import unicodedata
from pathlib import Path

import pandas as pd

uit = Path(sys.argv[1])
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
NC = data / "bron" / "netcongestie"

## 1. TenneT: vooraankondiging afname per congestiegebied (naam zoals in tabel_tennetcongestie 2026)
TENNET = {
    "Noord-Brabant": ("2022-06-08", "beleid/tijdlijn.csv: TenneT, voorlopige stop grootverbruikers Noord-Brabant en Limburg"),
    "Limburg": ("2022-06-08", "idem"),
    "Flevopolder, Utrecht en Gelderland": ("2022-11-17", "TenneT, vooraankondiging afname FGU; ACM CM#06 Utrecht"),
    "Haven van Rotterdam": ("2022-11-17", "ACM CM#05 Havengebied"),
    "Zeeland": ("2023-07-19", "ACM 2023-Zeeland-afname"),
    "Drenthe": ("2023-08-17", "alleen de rapportdatum bekend, dus bovengrens"),
    "Noord-Holland": ("2023-10-18", "ACM 2023-Noord-Holland-afname"),
    "Overijssel": ("2023-12-20", "ACM 2023-Overijssel-afname"),
    "Groningen West": ("2023-12-20", "ACM 2023-Groningen-afname"),
    "Noordelijk Zuid-Holland": ("2024-12-01", "TenneT-map 2024-12, dag onbekend"),
}


def lees_gpkg(pad, sql):
    con = sqlite3.connect(f"file:{pad}?mode=ro", uri=True)
    try:
        return pd.read_sql_query(sql, con)
    finally:
        con.close()


def norm(s):
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]", "", s)


def main():
    g26 = NC / "capaciteitskaart_netbeheernederland_20260921.gpkg"
    pc6 = lees_gpkg(g26, "select postcode, afname as rnb_2026, voedingsgebied_id, tennet_id, RNB_postcode as rnb, Gemeentecode as gemeentecode from tabel_congestie_pc6")
    pc6 = pc6.drop_duplicates("postcode")
    tc = lees_gpkg(g26, "select tennet_id, congestiegebied_afname, afname as tennet_2026 from tabel_tennetcongestie").drop_duplicates("tennet_id")
    pc6 = pc6.merge(tc, on="tennet_id", how="left")
    vgn = lees_gpkg(g26, "select distinct voedingsgebied_id, voedingsgebied_naam from voedingsgebieden_rnb")

    ## 1. TenneT
    pc6["d_tennet"] = pd.to_datetime(pc6.congestiegebied_afname.map({k: v[0] for k, v in TENNET.items()}))

    ## 2. ACM-register, regionale netbeheerders
    a = pd.read_csv(NC / "acm_register_vooraankondigingen_compact.csv", sep=";", dtype=str, keep_default_na=False)
    a["nb"] = a.netbeheerder.str.split().str[0]
    a = a[a.richting.isin(["Afname", "Beide"])].copy()
    ids = set(pc6.voedingsgebied_id.dropna())
    rijen = []
    for r in a.itertuples():
        sleutel = None
        if r.nb == "Enexis" and "." in r.intern_id:
            sleutel = r.intern_id.split(".")[1]
        elif r.nb in ("Stedin", "Tennet"):
            m = re.match(r"^CM#?(\d+)", r.intern_id)
            if m:
                c = "CM" + m.group(1).zfill(2)
                if c in ids:
                    sleutel = c
                else:
                    samen = [i for i in ids if i.startswith("CM") and c[2:] in i[2:].split("/")]
                    sleutel = samen[0] if samen else None
        elif r.nb == "Liander":
            kaal = re.sub(r"\s*\(.*$", "", r.intern_id).strip()
            sleutel = kaal if kaal in ids else None
        if sleutel in ids:
            rijen.append((sleutel, r.datum_vooraankondiging, r.nb))
    acm = pd.DataFrame(rijen, columns=["voedingsgebied_id", "d", "nb"])
    acm["d"] = pd.to_datetime(acm.d, errors="coerce")
    acm_vg = acm.groupby("voedingsgebied_id").d.min()
    pc6["d_acm"] = pc6.voedingsgebied_id.map(acm_vg)
    print(f"ACM afname: {len(a)} rijen, {len(rijen)} gekoppeld aan {acm_vg.size} voedingsgebieden")

    ## 3. Enexis: startdatum onderzoek afname, gekoppeld op de naam van het voedingsgebied
    e = pd.read_csv(NC / "enexis_congestieonderzoeken_details_20260926.csv", sep=";", dtype=str, keep_default_na=False)
    e = e[e.richting == "Afname"].copy()
    e["start"] = pd.to_datetime(e.teksten.str.extract(r"Startdatum onderzoek:\s*(\d{1,2}-\d{1,2}-\d{4})")[0], format="%d-%m-%Y", errors="coerce")
    ## de naam van een voedingsgebied begint met zijn code ('BGMR Bargermeer'); een
    ## Enexis-gebied kan een deelnet noemen ('Almelo Mosterdpot: blok MS01')
    naam2id = {}
    for r in vgn.itertuples():
        kaal = re.sub(r"^" + re.escape(str(r.voedingsgebied_id)) + r"\s+", "", str(r.voedingsgebied_naam))
        naam2id.setdefault(norm(kaal), set()).add(r.voedingsgebied_id)
    def koppel(g):
        kand = naam2id.get(norm(g.split(":")[0]), set())
        return sorted(kand)[0] if len(kand) == 1 else None
    e["voedingsgebied_id"] = e.gebied.map(koppel)
    print(f"Enexis-gebieden afname gekoppeld op naam: {e.voedingsgebied_id.notna().sum()} van {len(e)}; "
          f"niet: {sorted(set(e.loc[e.voedingsgebied_id.isna(), 'gebied'].str.split(':').str[0]))[:15]}")
    en = e.dropna(subset=["start", "voedingsgebied_id"]).groupby("voedingsgebied_id").start.min()
    pc6["d_enexis"] = pc6.voedingsgebied_id.map(en)
    print(f"Enexis afname: {len(e)} onderzoeken, {e.start.notna().sum()} met startdatum, {en.size} voedingsgebieden gekoppeld")

    ## 4. Historische standen per PC6: eerste stand met afname >= 2
    gh = NC / "capaciteitskaart_historische_standen.gpkg"
    st = lees_gpkg(gh, "select stand_datum, postcode, afname from tabel_pc6_standen_2023_2024")
    st["afname"] = pd.to_numeric(st.afname, errors="coerce")
    st["stand_datum"] = pd.to_datetime(st.stand_datum)
    vol = st[st.afname >= 2].groupby("postcode").stand_datum.min()
    pc6["d_stand"] = pc6.postcode.map(vol)
    pc6["stand_20240303"] = pc6.postcode.map(st[st.stand_datum == pd.Timestamp("2024-03-03")].set_index("postcode").afname)

    ## samen
    pc6["d_exact"] = pc6[["d_tennet", "d_acm", "d_enexis"]].min(axis=1)
    def bron(r):
        if pd.isna(r.d_exact):
            return None
        return [n for n in ("tennet", "acm", "enexis") if r[f"d_{n}"] == r.d_exact][0]
    pc6["bron"] = pc6.apply(bron, axis=1)
    links = pc6.d_stand.notna() & (pc6.d_exact.isna() | (pc6.d_stand < pc6.d_exact))
    pc6["datum"] = pc6.d_exact
    pc6.loc[links, "datum"] = pc6.loc[links, "d_stand"]
    pc6.loc[links, "bron"] = "stand"
    pc6["gecensureerd"] = links.astype(int)
    ## nooit vol voor zover bekend: geen datum en in 2026 bij netbeheerder en TenneT onder oranje
    pc6["nooit"] = (pc6.datum.isna() & (pd.to_numeric(pc6.rnb_2026, errors="coerce").fillna(-1) < 2)
                    & (pd.to_numeric(pc6.tennet_2026, errors="coerce").fillna(-1) < 2)).astype(int)

    n = len(pc6)
    print(f"PC6: {n}; met datum {pc6.datum.notna().sum()} ({100 * pc6.datum.notna().mean():.1f}%), "
          f"links gecensureerd {pc6.gecensureerd.sum()}, zonder datum en nooit vol {pc6.nooit.sum()}, "
          f"zonder datum maar in 2026 vol {(pc6.datum.isna() & (pc6.nooit == 0)).sum()}")
    print("bron van de datum:", pc6.bron.value_counts(dropna=False).to_dict())
    print("datum per kwartaal:", pc6.datum.dt.to_period("Q").value_counts(dropna=False).sort_index().to_dict())
    kol = ["postcode", "voedingsgebied_id", "tennet_id", "congestiegebied_afname", "rnb", "gemeentecode", "rnb_2026", "tennet_2026",
           "stand_20240303", "d_tennet", "d_acm", "d_enexis", "d_stand", "datum", "bron", "gecensureerd", "nooit"]
    out = pc6[kol].copy()
    for c in ["d_tennet", "d_acm", "d_enexis", "d_stand", "datum"]:
        out[c] = out[c].dt.strftime("%Y-%m-%d")
    out.to_csv(uit, sep=";", index=False)
    print(f"geschreven: {uit}")


if __name__ == "__main__":
    main()
