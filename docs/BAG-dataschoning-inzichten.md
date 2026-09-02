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
  deels; de **bouwjaar-delta (§6)** vult hem aan. Let op: die delta-regel is
  wél de beste discriminator die we hebben, maar hij is **inert** — hij raakt
  1,5% van het bestand en verandert de uitkomsten niet (§8).
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

## 8. Waarom de bouwjaar-delta op −6 staat (issue #26)

Drie dingen die uit elkaar moeten: **de 6 is een eigen werkafspraak** (geen norm
uit CBS of BAG), hij **ligt wel ongeveer op de empirische knik**, en hij is
**inert** voor de uitkomsten.

### 8.1 Herkomst: werkafspraak, geen externe bron

- `bouwjaar_mismatch` (`|delta| > 5`) is op **18-6-2026** ingevoerd in `3ec8bd3`,
  onder een blok met het commentaar `//adhoc tests`, als plausibiliteitscheck
  tegen records als "vergunning 1950 + bouwjaar 2017".
- `reno_delta = −6` is daar in de schoningssessie van **17-7-2026** (`1a2de37`)
  het spiegelbeeld van: "> 5" = "≥ 6", genomen aan de kant die op een bestaand
  gebouw wijst. Geverifieerd: elke `bouwjaar_mismatch` valt in precies een van
  beide helften (19.437 met δ ≤ −6, 17.022 met δ ≥ +6, 0 elders; VBO-niveau).

**CBS hanteert zoiets niet.** In de mutatietypering (86098) speelt bouwjaar
alleen mee in `IDEN_N_3` (bouwjaar ≥ verslagjaar − 1); een mutatie wordt daar
**nooit** verworpen op een bouwjaar-mismatch. Er is dus geen bron om naar te
verwijzen — wek die suggestie niet.

### 8.2 De grens ligt wel ongeveer op de knik

Per delta-waarde, **op pandniveau** (`Pand_VergunningDuur`, 620.816 panden), het
aandeel panden met eindstatus "in gebruik" — bestaat dit gebouw en wordt het
bewoond?

| delta | −1 | −2 | −3 | −4 | −5 | **−6** | −7 | −8 | −9 | −10 | −15 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| panden | 3.639 | 842 | 650 | 286 | 210 | **126** | 90 | 97 | 117 | 131 | 148 |
| % in gebruik | 37,4 | 36,0 | 15,1 | 24,1 | 34,3 | **73,8** | 60,0 | 72,2 | 87,2 | 89,3 | 98,0 |

Twee kantelpunten vallen samen rond −6: de **aantallen** storten in van 3.639
(−1) naar 210 (−5) en worden daarna vlak (~90–150 per jaar), en het **karakter**
verandert — bij −1..−5 is 15–37% in gebruik, vanaf −6 is dat 60–98%. Pas onder
−6 heb je consistent bestaande, bewoonde panden, wat "verbouwing" hoort te
betekenen; de band erboven is grotendeels administratieve rommel. Achteraf
geconstateerd, niet de reden dat de grens er kwam — maar hij ligt niet toevallig
verkeerd.

### 8.3 En het maakt nauwelijks uit

Robuustheidscheck op **−4 / −6 / −9**. Panden die als verbouwing worden
aangemerkt: 10.031 / **9.535** / 9.222, dus +496 (+0,08%) en −313 (−0,05%) van
620.816. Op VBO-niveau raakt de regel 19.437 van 1.313.265 VBO's (1,5%).

DiD op `tr` (`fepois`, provincie×jaar + provincie×afstandsklasse, cluster
provincie×klasse, vergunningjaren 2012–2025):

| reno_delta | N | 250 m | 500 m | 1000 m | 2500 m |
|---|---|---|---|---|---|
| −4 | 1.005.596 | −0,2398 | −0,3032 | −0,2293 | −0,1664 |
| **−6** | **1.006.018** | **−0,2394** | **−0,3024** | **−0,2293** | **−0,1663** |
| −9 | 1.006.482 | −0,2399 | −0,3028 | −0,2290 | −0,1653 |
| regel helemaal uit | 1.010.570 | −0,2434 | −0,3070 | −0,2343 | −0,1633 |

Op 250 m bewegen −4, −6 en −9 binnen 0,0005 van elkaar bij een standaardfout van
0,088; de regel helemaal uitzetten scheelt 0,004. Voor de nieuwbouwtelling naar
mutatiejaar is het resultaat identiek over alle drempels, omdat `in_sample_nwb`
niet op `projecttype` filtert. De regel is verdedigbaar maar draagt de conclusies
niet.

### 8.4 Wat de regel niet vangt

- **14,6%** van wat δ ≤ −6 als verbouwing aanmerkt staat óók in de
  CBS-nieuwbouwreeks. Vlak over alle drempels (14,6–15,8%), dus verschuiven helpt
  niet. De pandstatus `Verbouwing_pand` draagt maar 1,9% bij binnen die groep.
- Het is vrijwel volledig een **2012–2013-verschijnsel**: aandeel δ ≤ −6 per
  vergunningjaar 2,79% (2012), 2,61% (2013), daarna 0,08–0,48%. Dezelfde
  BAG-opbouwperiode als de koppelgraad in CHECK 2.
- En in die jaren **correleert het met de behandeling**: δ ≤ −6 nabij (≤1000 m)
  vs ver is 1,91 vs 2,87 (2012) en 1,43 vs 2,75 (2013), daarna verwaarloosbaar en
  zonder richting. Asymmetrisch precies in de pre-periode. Bij 1,5% omvang niet
  genoeg om iets te doen, maar het hoort in de gevoeligheidsanalyse.

### 8.5 Openstaand defect: `bouwjaar_mismatch` is tweezijdig

De semantiek is eenzijdig — alleen een negatieve delta wijst op een bestaand
gebouw — maar de vlag is symmetrisch. Aan de positieve kant zitten **6.987**
panden met δ > +5, waarvan **86,9% gewoon nieuwbouw** is (`IsNieuwbouw`; 6.344
met eindstatus `Pand_in_gebruik`). Dat zijn lange bouwtijden en fasering, geen
datafouten. De vlag wordt alleen als kwaliteitsindicator geëxporteerd en niet om
te filteren, dus het is niet urgent, maar het opruimen waard.

### 8.6 Hoe dit te presenteren

- Houd −6 aan, maar **als werkafspraak met de robuustheidscheck −4 / −6 / −9**,
  nooit als vaststaande norm.
- **Verdedig de drempel in het paper niet inhoudelijk**; laat de
  gevoeligheidstabel uit §8.3 zien. Dat is een sterker argument dan welke
  motivering van het getal 6 ook.
- **Exporteer `delta` als continue kolom**, zodat de grens downstream
  verschuifbaar is.
- De grijze band −1..−5 blijft apart getagd (`vlag_delta_grijs`); §8.2 laat zien
  dat daar iets anders in zit dan renovatie.

## 9. IsCensored en IsNieuwbouw spreken elkaar tegen (issue #25)

Ja, en dat hoort zo — het zijn metingen op **verschillende niveaus**:

| vlag | niveau | definitie |
|---|---|---|
| `IsCensored` | **pand** | `IsNull(First_Datum_Voorraad)`: het pand heeft nooit een `pand_status` met `IsVoorraad` gehad (`PrepBAG.dms:42`) |
| `IsNieuwbouw` | **VBO** | minstens één VBO van dit pand staat in de CBS-mutatiereeks (mutatietype `N`) |

Op `Pand_VergunningDuur_20260730` (620.816 panden):

| | IsNieuwbouw = nee | IsNieuwbouw = ja |
|---|---|---|
| **IsCensored = nee** | 41.504 | 481.644 |
| **IsCensored = ja** | 95.761 | **1.907** |

Dus 1.907 panden (0,31%) zijn allebei. Hun laatste pandstatus:
`Niet_gerealiseerd_pand` 735, `Pand_gesloopt` 734,
`Pand_ten_onrechte_opgevoerd` 263, `Bouwvergunning_verleend` 96,
`Bouw_gestart` 79.

De VBO-kant weerspreekt dat: van de 5.270 VBO's op deze panden staat **71,8% in
de CBS-voorraaddefinitie** en heeft 78,3% een nieuwbouwmutatie. Uitgesplitst
naar pandstatus (VBO in voorraad ja/nee): `Niet_gerealiseerd_pand` 1.418/392,
`Pand_gesloopt` 1.323/97, `Pand_ten_onrechte_opgevoerd` 968/14. Met andere
woorden: **de objecten zijn opgeleverd, alleen de pandstatus zegt van niet.**

Dit is dus geen rekenfout in `Pand_VergunningDuur` maar exact dezelfde
pand-vs-VBO-inconsistentie als in §4 en in `03_checks.R` CHECK 4 (de 2.691 VBO's
met pandstatus `Niet_gerealiseerd_pand` die zelf in gebruik zijn). Bij
`Pand_gesloopt` is er niet eens een tegenspraak: gebouwd en later gesloopt is
een normale levensloop, alleen ontbreekt de tussenliggende voorraadstatus in de
bewaarde historie.

**Gevolgen en wat eraan gedaan is:**

- Voor de R-pipeline: geen. Realisatie wordt al op `vbo_status` bepaald (§4),
  niet op `IsCensored`. De 1.907 panden zitten via hun VBO's gewoon in
  `uitkomst == "gerealiseerd_nieuwbouw"`.
- Voor duur-/survival-analyse op **pandniveau** wél: `IsCensored` zou deze 1.907
  panden als "nog niet opgeleverd" meenemen terwijl ze aantoonbaar zijn
  opgeleverd, mét een opleverdatum die alleen op VBO-niveau bestaat. Gebruik
  `IsCensored` daarom niet los.
- `N2000_Analyse.dms` exporteert nu `cens_nwb_conflict := IsCensored && IsNieuwbouw`
  en beide vlaggen hebben een `Descr` die het niveau benoemt.
- `03_checks.R` CHECK 8 telt het per extract, zodat het zichtbaar blijft.

Ter contrast het spiegelbeeld — opgeleverd maar níet in de nieuwbouwreeks —
is met 41.504 panden (6,7%) veel groter, en zit vrijwel helemaal vóór 2012
(vergunningjaar 2009–2011: 24.210). Dat is de bekende BAG-opbouwperiode, geen
inconsistentie.

## Bronnen (extern geverifieerd)

- IPLO — buitenplanse omgevingsplanactiviteit; vergunningplicht Natura 2000-activiteit.
- VNG / Waarderingskamer — BAG & WOZ onder Omgevingswet en Wkb; "de knip".
- Wijzigingendocument BAG 2.0 (geobasisregistraties, 2018); IMBAG praktijkhandleiding.
- CBS — *Afleiding van Voorraad woningen* (StatLine 81955) en bijlage-1 transformaties.
- IMBAG praktijkhandleiding — verlenen bouwvergunning; statussen.
