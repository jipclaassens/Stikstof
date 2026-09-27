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

**Getoetst (september 2026, `R/13_binnenring.R`)**: (i) klopt niet voor het aantal. Woningen in
gevolgklasse 1 vallen niet uit de BVV-reeks: het aandeel opleveringen zonder vergunningrecord bleef
1,3–2,6% per halfjaar, ook voor eengezinswoningen die vanaf juli 2024 vergund werden, en het aandeel
eengezins in de vergunningen bleef ~36–38%. (ii) klopt wel: het brondocument voor BVV werd het
**akkoord op de bouwmelding** (gevolgklasse 1) of de **technische bouwvergunning** (2 en 3)
(factsheet VNG/Waarderingskamer, okt. 2023), en sinds **september 2025** de vergunning voor de
omgevingsplanactiviteit. De piek van juni 2026 (~30.000 BVV-objecten tegen ~8.000 per maand) past
bij een inhaalslag. Behandel 2024–2026 dus als breuk in het **moment**, niet in de dekking.

## 4. BAG-statushistorie en voorraaddefinitie

- **`Verbouwing pand`** is een **nieuwe status sinds BAG 2.0 (wet in werking 1-7-2018)**,
  samen met `ten onrechte opgevoerd`. Bedoeld voor **reeds opgeleverde** objecten.
  Het is een **transiënte** status: na afronding keert het pand terug naar
  `Pand in gebruik`. Vóór 2018 dook een verbouwing/renovatie op als een
  BVV-achtige statusovergang op een bestaand pand. → De status vangt renovatie maar
  deels; de bouwjaar-delta (§6) vult hem aan. Let op: die delta-regel is wel de
  beste discriminator die we hebben, maar hij is inert. Hij raakt 1,5% van het
  bestand en verandert de uitkomsten niet, zie §8.
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

Getoetst (september 2026, `R/17_registratie.R`). Met de koppeling van BAG-panden
aan zaken uit de gemeentelijke bekendmakingen (vergunningenspoor, binnen 100 meter):
de 2.104 panden van Meierijstad en Bernheze in juni 2026 zijn voor 30 procent aan een
zaak gekoppeld, en bij driekwart van de rest ligt binnen 100 meter geen verleend besluit
uit de drie jaar ervoor; wat die batch is, blijft open. De rest van juni (26.850
objecten in 7.717 panden, 277 gemeenten) is ruim drie keer een gewone maand. Van die
objecten zit 84 procent in gekoppelde panden (87 procent van september 2025 tot en met
mei 2026), met besluiten die mediaan op 18 juni 2026 werden gepubliceerd, en
ongekoppelde panden liggen niet vaker bij een ouder besluit dan in andere maanden (31
tegen 37 procent). Dus vooral een batch in twee gemeenten en daarnaast een maand met
veel nieuwe besluiten; maar er waren ook ruim vier keer zoveel ongekoppelde panden als
in een gewone maand, dus een kleinere inhaalslag daarbinnen is niet uit te sluiten. Let op: een bouwstart voor
de vergunningdatum komt in de BAG vrijwel niet voor, omdat de statussen van een pand
in volgorde worden geregistreerd; met die toets is een inhaalslag dus niet te zien.

## 8. Waarom de bouwjaar-delta op −6 staat (issue #26)

Er lopen drie dingen door elkaar die uit elkaar moeten. De 6 is een eigen
werkafspraak en geen norm uit CBS of BAG. Hij ligt wel ongeveer op de plek waar
de verdeling van gedaante verandert. En hij is inert: voor de uitkomsten maakt
de precieze waarde vrijwel niets uit.

### 8.1 Wat de delta is, en waarom hij iets zegt

De delta is `pand_bouwjaar − vergunningjaar`. Dat verschil zegt iets omdat
`oorspronkelijkBouwjaar` een verplicht pandattribuut is dat al bij
vergunningverlening wordt ingevuld (§5). Bij echte nieuwbouw is het dus het
geplande opleveringsjaar: de vergunning komt eerst, het bouwjaar ligt er kort
na. Het normale patroon is vergunning 2020, bouwjaar 2021, oplevering 2022, dus
een delta van +1.

Ligt het bouwjaar juist ver vóór de vergunning, dan bestond het gebouw al en
ging de vergunning over een verbouwing. Vandaar de regel dat een voldoende
negatieve delta op renovatie wijst. De vraag is waar "voldoende" ligt.

### 8.2 Herkomst: een eigen werkafspraak, geen externe bron

De grens komt niet uit een norm maar uit de geschiedenis van deze repo.
`bouwjaar_mismatch` (`|delta| > 5`) is op 18-6-2026 ingevoerd in `3ec8bd3`,
onder een blok met het commentaar `//adhoc tests`, als plausibiliteitscheck
tegen records als "vergunning 1950 met bouwjaar 2017". De renovatiegrens
`reno_delta = −6` is daar in de schoningssessie van 17-7-2026 (`1a2de37`) het
spiegelbeeld van: "> 5" is hetzelfde als "≥ 6", genomen aan de kant die op een
bestaand gebouw wijst. Geverifieerd op VBO-niveau: elke `bouwjaar_mismatch`
valt in precies een van beide helften, 19.437 met δ ≤ −6 en 17.022 met δ ≥ +6,
en nul daarbuiten.

Belangrijk voordat iemand er een bron bij zoekt: CBS hanteert zoiets niet. In de
mutatietypering (86098) speelt bouwjaar alleen mee in `IDEN_N_3`, de eis dat het
bouwjaar minstens het verslagjaar min één is, en een mutatie wordt daar nooit
verworpen op een bouwjaar-mismatch. Er is dus geen externe norm om naar te
verwijzen, en die suggestie moeten we niet wekken.

### 8.3 De grens ligt wel ongeveer op de knik

Dit beantwoordt de vraag waarom niet −3 of −10. Per delta-waarde, op pandniveau
(`Pand_VergunningDuur`, 620.816 panden), het aandeel panden met eindstatus "in
gebruik". Dat aandeel zegt of het gebouw bestaat en bewoond wordt, en dus of
"verbouwing" een plausibele lezing is.

| delta | −1 | −2 | −3 | −4 | −5 | −6 | −7 | −8 | −9 | −10 | −15 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| panden | 3.639 | 842 | 650 | 286 | 210 | 126 | 90 | 97 | 117 | 131 | 148 |
| % in gebruik | 37,4 | 36,0 | 15,1 | 24,1 | 34,3 | 73,8 | 60,0 | 72,2 | 87,2 | 89,3 | 98,0 |

Twee kantelpunten vallen rond −6 samen. De aantallen storten in van 3.639 bij
delta −1 naar 210 bij −5 en blijven daarna vlak, rond de 90 tot 150 per
jaarstap. Tegelijk verandert het karakter van de groep: bij −1 tot −5 is 15 tot
37% van de panden in gebruik, vanaf −6 is dat 60 tot 98%. Pas onder de −6 heb je
dus consistent te maken met bestaande, bewoonde panden, wat renovatie hoort te
betekenen. De band daarboven is grotendeels administratieve rommel en geen
verbouwing.

Dat is achteraf geconstateerd en niet de reden dat de grens er ooit kwam, maar
het betekent wel dat de werkafspraak niet toevallig verkeerd ligt.

### 8.4 En het maakt voor de uitkomsten nauwelijks uit

De robuustheidscheck loopt over −4, −6 en −9. Het aantal panden dat als
verbouwing wordt aangemerkt is dan 10.031, 9.535 en 9.222. Dat is +496 en −313
ten opzichte van −6, oftewel minder dan een tiende procent van de 620.816
panden. Op VBO-niveau raakt de regel 19.437 van de 1.313.265 VBO's, dus 1,5%.

De DiD-coëfficiënt op `tr` (`fepois`, provincie×jaar en
provincie×afstandsklasse, cluster provincie×klasse, vergunningjaren 2012-2025):

| reno_delta | N | 250 m | 500 m | 1000 m | 2500 m |
|---|---|---|---|---|---|
| −4 | 1.005.596 | −0,2398 | −0,3032 | −0,2293 | −0,1664 |
| −6 | 1.006.018 | −0,2394 | −0,3024 | −0,2293 | −0,1663 |
| −9 | 1.006.482 | −0,2399 | −0,3028 | −0,2290 | −0,1653 |
| regel helemaal uit | 1.010.570 | −0,2434 | −0,3070 | −0,2343 | −0,1633 |

Op 250 m bewegen de drie drempels binnen 0,0005 van elkaar, bij een
standaardfout van 0,088. Zelfs de regel volledig uitzetten verschuift de
coëfficiënt maar 0,004, ruim binnen de ruis. Voor de nieuwbouwtelling naar
mutatiejaar is de uitkomst identiek over alle drempels, omdat `in_sample_nwb`
niet op `projecttype` filtert. De regel is dus verdedigbaar, maar hij draagt de
conclusies niet.

### 8.5 Wat de regel niet vangt

Van wat δ ≤ −6 als verbouwing aanmerkt staat 14,6% óók in de CBS-nieuwbouwreeks,
wat een directe tegenspraak is. Dat aandeel is vlak over alle drempels, 14,6 tot
15,8%, dus verschuiven van de grens lost het niet op. De pandstatus
`Verbouwing_pand` draagt binnen die groep maar 1,9% bij.

Verder is het vrijwel volledig een verschijnsel van 2012 en 2013. Het aandeel
δ ≤ −6 per vergunningjaar is 2,79% in 2012 en 2,61% in 2013, en daarna 0,08 tot
0,48%. Dat is dezelfde BAG-opbouwperiode als de koppelgraad in CHECK 2.

En in juist die jaren correleert het met de behandeling. Nabij N2000 (≤1000 m)
tegen ver weg is het 1,91 tegen 2,87 in 2012 en 1,43 tegen 2,75 in 2013, daarna
verwaarloosbaar en zonder richting. De misclassificatie is dus asymmetrisch
precies in de pre-periode. Bij een omvang van 1,5% is dat niet genoeg om iets te
doen, maar het hoort in de gevoeligheidsanalyse thuis en niet in een voetnoot.

### 8.6 Openstaand defect: `bouwjaar_mismatch` is tweezijdig

De semantiek is eenzijdig, want alleen een negatieve delta wijst op een bestaand
gebouw, maar de vlag is symmetrisch. Aan de positieve kant zitten 6.987 panden
met δ > +5, en daarvan is 86,9% gewoon nieuwbouw volgens `IsNieuwbouw`, met
`Pand_in_gebruik` als eindstatus voor 6.344 stuks. Dat zijn lange bouwtijden en
gefaseerde projecten, geen datafouten, en ze worden nu ten onrechte gevlagd. De
vlag wordt alleen als kwaliteitsindicator geëxporteerd en niet om te filteren,
dus urgent is het niet, maar het opruimen waard.

### 8.7 Hoe dit te presenteren

Houd −6 aan, maar presenteer hem als werkafspraak met de robuustheidscheck −4,
−6 en −9 erbij, nooit als vaststaande norm, en zonder een bron te suggereren die
er niet is. Verdedig de drempel in het paper niet inhoudelijk maar laat de
gevoeligheidstabel uit §8.4 zien; dat is een sterker argument dan welke
motivering van het getal 6 ook. Exporteer `delta` bij voorkeur als continue
kolom, zodat de grens downstream verschuifbaar is en niemand vastzit aan onze
keuze. De grijze band −1 tot −5 blijft apart getagd via `vlag_delta_grijs`,
omdat §8.3 laat zien dat daar iets anders in zit dan renovatie.

## 9. IsCensored en IsNieuwbouw spreken elkaar tegen (issue #25)

Ja, en dat hoort zo. Het zijn metingen op verschillende niveaus:

| vlag | niveau | definitie |
|---|---|---|
| `IsCensored` | pand | `IsNull(First_Datum_Voorraad)`: het pand heeft nooit een `pand_status` met `IsVoorraad` gehad (`PrepBAG.dms:42`) |
| `IsNieuwbouw` | VBO | minstens één VBO van dit pand staat in de CBS-mutatiereeks (mutatietype `N`) |

Op `Pand_VergunningDuur_20260730` (620.816 panden):

| | IsNieuwbouw = nee | IsNieuwbouw = ja |
|---|---|---|
| IsCensored = nee | 41.504 | 481.644 |
| IsCensored = ja | 95.761 | 1.907 |

Dus 1.907 panden (0,31%) zijn allebei. Hun laatste pandstatus:
`Niet_gerealiseerd_pand` 735, `Pand_gesloopt` 734,
`Pand_ten_onrechte_opgevoerd` 263, `Bouwvergunning_verleend` 96,
`Bouw_gestart` 79.

De VBO-kant weerspreekt dat. Van de 5.270 VBO's op deze panden staat 71,8% in de
CBS-voorraaddefinitie en heeft 78,3% een nieuwbouwmutatie. Uitgesplitst naar
pandstatus, als VBO in voorraad tegen niet in voorraad:
`Niet_gerealiseerd_pand` 1.418 om 392, `Pand_gesloopt` 1.323 om 97,
`Pand_ten_onrechte_opgevoerd` 968 om 14. De objecten zijn dus opgeleverd, en
alleen de pandstatus zegt van niet.

Dit is geen rekenfout in `Pand_VergunningDuur` maar exact dezelfde
pand-vs-VBO-inconsistentie als in §4 en in `03_checks.R` CHECK 4, waar 2.691
VBO's met pandstatus `Niet_gerealiseerd_pand` zelf in gebruik blijken. Bij
`Pand_gesloopt` is er niet eens een tegenspraak: gebouwd en later gesloopt is
een normale levensloop, alleen ontbreekt de tussenliggende voorraadstatus in de
bewaarde historie.

Gevolgen en wat eraan gedaan is:

- Voor de R-pipeline: geen. Realisatie wordt al op `vbo_status` bepaald (§4) en
  niet op `IsCensored`. De 1.907 panden zitten via hun VBO's gewoon in
  `uitkomst == "gerealiseerd_nieuwbouw"`.
- Voor duur- en survivalanalyse op pandniveau wel. `IsCensored` zou deze 1.907
  panden als nog niet opgeleverd meenemen terwijl ze aantoonbaar zijn
  opgeleverd, met een opleverdatum die alleen op VBO-niveau bestaat. Gebruik
  `IsCensored` daarom niet los.
- `N2000_Analyse.dms` exporteert nu `cens_nwb_conflict := IsCensored && IsNieuwbouw`,
  en beide vlaggen hebben een `Descr` die het niveau benoemt.
- `03_checks.R` CHECK 8 telt het per extract, zodat het zichtbaar blijft.

Het spiegelbeeld, opgeleverd maar niet in de nieuwbouwreeks, is met 41.504
panden (6,7%) veel groter en zit vrijwel helemaal vóór 2012. In de
vergunningjaren 2009 tot 2011 gaat het om 24.210 panden. Dat is de bekende
BAG-opbouwperiode en geen inconsistentie.

## Bronnen (extern geverifieerd)

- IPLO — buitenplanse omgevingsplanactiviteit; vergunningplicht Natura 2000-activiteit.
- VNG / Waarderingskamer — BAG & WOZ onder Omgevingswet en Wkb; "de knip".
- Wijzigingendocument BAG 2.0 (geobasisregistraties, 2018); IMBAG praktijkhandleiding.
- CBS — *Afleiding van Voorraad woningen* (StatLine 81955) en bijlage-1 transformaties.
- IMBAG praktijkhandleiding — verlenen bouwvergunning; statussen.
