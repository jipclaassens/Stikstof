# BAG-dataschoning & juridische inzichten (stikstof/woningbouw)

Werkdocument bij het GeoDMS-model. Verzamelt de inzichten die nodig zijn om de
BVV-/nieuwbouw-exports te schonen en correct te interpreteren voor de analyses
(RERQ-paper; statistiek door Huub Ploegmakers). Aanvulling op de wiki in
`C:/ProjDir/_Tools/Stikstof.wiki`.

## 1. Planologisch spoor vs. BAG-status

De BAG-pandstatus **"Bouwvergunning verleend" (BVV)** is een registratie-/statusvlag:
er is een omgevingsvergunning voor een te registreren object verleend, dus het VBO
ontstaat en krijgt een adres. De **planologische route** waarlangs dat mocht, staat
níet in de BAG:

- **artikel 19 WRO** (tot 2008) → **Wabo art. 2.12 afwijking** (2010–2023) →
  **(buitenplanse) omgevingsplanactiviteit (OPA/BOPA)** (Omgevingswet, sinds 1-1-2024).
  Dit is telkens dezelfde functie: de uitzonderingsroute om plan-strijdig te bouwen.
- Uit `last_pand_status`/`bv_eerste_datum` is dus **niet** af te leiden of een project
  via een afwijkroute (BOPA/art. 19) is vergund. Daarvoor is een externe bron nodig
  (DSO/Omgevingsloket, ruimtelijkeplannen.nl, gemeentelijke besluiten).

## 2. Bouwvergunning ↔ natuurvergunning zijn ontkoppeld

- Bouwvergunning = **gemeente**; natuurvergunning (Natura 2000-activiteit, art. 5.1 Ow)
  = **provincie (GS)**. Sinds 1-1-2024 is de **aanhaakverplichting vervallen** en de
  onlosmakelijke samenhang uit de Wabo weg: de aanvrager kiest zelf of hij combineert.
- Gevolg: een gemeente kan een bouwvergunning afgeven (→ BAG-status BVV) terwijl de
  natuurvergunning nog niet rond is. Bouwen mag dan feitelijk nog niet (art. 5.1 Ow).
- **Toetsbare hypothese**: panden die op BVV/Bouw_gestart blijven hangen (`IsCensored`)
  of afgeblazen worden, liggen dichter bij stikstofgevoelige N2000 (`dist2N2000_m`).
  De BAG kent de reden niet; effect is alleen indirect meetbaar (vertraagde/uitblijvende
  oplevering nabij N2000).

## 3. Omgevingswet-breuk per 1-1-2024

Twee mechanismen die de **BVV-tijdreeks** raken:

- **De knip**: de bouwvergunning is gesplitst in (a) technische bouwactiviteit (Bbl) en
  (b) omgevingsplanactiviteit bouwen.
- **Wkb, gevolgklasse 1** (veel grondgebonden woningbouw): technische toets vervangen
  door **bouwmelding + kwaliteitsborger** — geen vergunning meer.

Daardoor kan (i) het **aantal BVV-registraties** vanaf 2024 een niveaubreuk vertonen
(melding i.p.v. vergunning, of registratieachterstand in de transitie), en (ii) het
**datummoment** verschuiven (twee besluiten met eigen data). Behandel **1-1-2024 als
mogelijke structurele breuk** (aparte dummy / pre-post gevoeligheidsanalyse) en wees
terughoudend met 2024–2025 vergunning-/pipelinecijfers.

## 4. BAG-statushistorie en voorraaddefinitie

- **`Verbouwing pand`** is een **nieuwe status sinds BAG 2.0 (wet in werking 1-7-2018)**,
  samen met `ten onrechte opgevoerd`. Bedoeld voor **reeds opgeleverde** objecten.
  Het is een **transiënte** status: na afronding keert het pand terug naar
  `Pand in gebruik`. Vóór 2018 dook een verbouwing/renovatie op als een
  BVV-achtige statusovergang op een bestaand pand. → De status vangt renovatie maar
  deels; de **bouwjaar-delta (§6) is de robuuste discriminator**.
- **CBS-voorraaddefinitie**: een VBO telt tot de voorraad bij status
  `in gebruik`, `in gebruik (niet ingemeten)`, `buiten gebruik` (en `verbouwing`).
  **`buiten gebruik` telt dus mee.** Realisatie/voorraad in de Object-exports bepalen
  op **`last_vbo_status`**, niet op de pandstatus.

## 5. Bouwjaar: semantiek, betrouwbaarheid, truncatie-fix

- `oorspronkelijkBouwjaar` is een **verplicht** Pand-attribuut en wordt **al bij BVV**
  ingevuld → het is het **verwachte/geplande opleveringsjaar**, ten tijde van de
  vergunning dus een toekomstig, geschat jaar. Normaal patroon:
  vergunning 2020 → bouwjaar 2021 → oplevering 2022.
- Daarom: gebruik **`oplevering_eerste_datum`** als realisatiemoment, en bouwjaar als
  (a) nieuwbouw-vs-renovatie-discriminator en (b) plausibiliteitscheck — niet als
  realisatiedatum.
- Bouwjaar is **onbetrouwbaar** in de staarten: geschat/afgeleid voor oude voorraad
  (BAG-opbouw ~2009), soms herbouw-jaar, placeholders.
- **Truncatie-bug (gefixt in deze commit)**: de bovengrens stond hardcoded op `'2025'`,
  waardoor bouwjaar **≥ 2026** onterecht op null werd gezet (max bouwjaar in het
  20260714-extract = 2025). Nu gekoppeld aan `BAG_year+1` (dynamisch;
  `BAG_year := left(BAG_file_date,4)`). Ook `(0/0)[uint16]` → `null_w`.

## 6. Classificatie in vijf bakjes (schoning `Pand_VergunningDuur`)

Kern-discriminator: **delta = `pand_bouwjaar − vergunningjaar`**. Echte nieuwbouw heeft
bouwjaar ≥ vergunningjaar (vergunning gaat vooraf); **negatieve delta = gebouw bestond
al = verbouwing/renovatie**.

Beslisregels (volgorde telt, eerste match wint):

```
B_admin  ← status ∈ {Pand_ten_onrechte_opgevoerd, Sloopvergunning_verleend}
          OR (oplevering gezet EN duur_negatief=1)      # oplevering vóór vergunning
A_reno   ← status = Verbouwing_pand
          OR (bouwjaar aanwezig EN delta ≤ −6)          # renovatie
          OR (bouwjaar < 1900 EN vergunning ≥ 1990)     # fout/oud
K3       ← status = Niet_gerealiseerd_pand              # afgeschoten
K1b      ← status = Pand_gesloopt EN IsNieuwbouw=1      # gebouwd, later gesloopt
K1       ← status ∈ {Pand_in_gebruik, Pand_in_gebruik_niet_ingemeten, Pand_buiten_gebruik}
K2       ← status ∈ {Bouwvergunning_verleend, Bouw_gestart} EN geen oplevering
```

Bakjes: **K1** correcte levensloop → nieuwbouw; **K2** pipeline (waarschijnlijk nieuwbouw);
**K3** afgeschoten; **A** verbouwing/renovatie (weg); **B** administratieve ruis (weg).

Indicatieve aantallen op `Pand_VergunningDuur_20260714.csv` (620.816 panden):
K1 ≈ 505k (81%), K2 ≈ 71k (11%), K3 ≈ 23k (4%), A ≈ 9k (1,5%), B ≈ 8k (1,3%).

**Knoppen / open punten** (te herzien op de gecorrigeerde run):
- `buiten gebruik` → K1 (voorraad), niet B_admin; realisatie op `last_vbo_status`.
- Deelregel "in gebruik + bouwjaar ontbreekt → B" **schrappen** na de truncatie-fix
  (ving vooral de genulde 2026-opleveringen; ~397 records met oplevering in de byMiss).
- Grijze band delta −5..−1 (~5k) apart taggen en steekproefsgewijs beoordelen.
- `Pand_gesloopt`: K1 met `later_gesloopt`-vlag óf weg (keuze).
- A_reno splitsen in `reno` vs `fout_bouwjaar` (bouwjaar-plausibiliteit).
- `1-1-2024` als breukvlag meenemen.
- Nieuw veld `last_pand_status_datum` (deze commit) = datum waarop de laatste status
  begon; bruikbaar om afgeschoten/gecensureerde panden te dateren.

## 7. BVV-instroom is klonterig op gemeenteniveau

Maandreeks "nieuwe BVV-woonpanden" (o.b.v. `bv_eerste_datum`): **geen zichtbaar effect**
van PAS (2019), bouwvrijstelling (2021), Porthos (2022) of Omgevingswet (2024) op de
instroom.

Wél artefacten:
- **Juni 2026 = 9.897** (≈3× normaal) is **niet** seizoensgebonden en **niet** door de
  downloaddatum (10-7). Het is **gemeentelijke batch-registratie**: GM 1948 **Meierijstad**
  (1.379; 14%) + GM 1721 **Bernheze** (726; 7%) = 21% van de maand (normaal is de grootste
  gemeente <4%). Zonder die twee nog 7.792 → er zit ook bredere verhoging in die uit één
  snapshot niet te scheiden is.
- Historische pieken (feb–mrt 2015, sep–okt 2018) zijn juist **breed verspreid**
  (grootste gemeente ~4%), dus een ander type gebeurtenis.

**Les**: BAG-BVV-instroom is klonterig op gemeenteniveau; één gemeente kan een maand
kapen. Niet "laatste maand afknippen" als vaste regel, maar **aggregeren naar
kwartaal/jaar** (zoals het paper) en/of **gemeente-maand-uitschieters markeren/winsorizen**.

## Bronnen (extern geverifieerd)

- IPLO — buitenplanse omgevingsplanactiviteit; vergunningplicht Natura 2000-activiteit.
- VNG / Waarderingskamer — BAG & WOZ onder Omgevingswet en Wkb; "de knip".
- Wijzigingendocument BAG 2.0 (geobasisregistraties, 2018); IMBAG praktijkhandleiding.
- CBS — *Afleiding van Voorraad woningen* (StatLine 81955) en bijlage-1 transformaties.
- IMBAG praktijkhandleiding — verlenen bouwvergunning; statussen.
