# R-pipeline: van GeoDMS-export naar analysebestand

Deze map bevat de statistische kant van het stikstof/woningbouw-onderzoek.
De GeoDMS-configuratie in `cfg/` produceert drie CSV-exports; de scripts hier
combineren die tot één bestand op VBO-niveau, classificeren de objecten en
draaien de aanbodanalyses.

Aanleiding: de analyses zaten verspreid over twee grote Stata-do-files
(`do_analysis_files_stikstof.do`, `do_analysis_stikstof_jaar.do`) waarin
inlezen, schonen, filteren en schatten door elkaar liepen. Zie ook
[issue #28](https://github.com/jipclaassens/Stikstof/issues/28).

## Draaien

```bash
"C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe" C:\ProjDir\Stikstof\R\run_all.R
```

Benodigde packages: `data.table`, `fixest`. Beide staan al op deze machine.
Doorlooptijd ongeveer twee minuten; het zwaarste is het inlezen van de CSV's.

## Structuur

| script | doet | schrijft |
|---|---|---|
| `00_config.R` | paden, extractdatum, parameters, hulpfuncties | — |
| `01_build_vbo.R` | drie exports → één bestand op VBO-niveau | `_derived/vbo_base.rds` |
| `02_classify.R` | projecttype, uitkomst, steekproefvlaggen | `_derived/vbo_analyse.rds` |
| `03_checks.R` | datakwaliteitscontroles (CHECK 1–9) | `_output/checks_<extract>.txt` |
| `04_analyse_aanbod.R` | DiD en event study, vergunningen en nieuwbouw | `_output/aanbod_<extract>.txt` |
| `05_eventstudy_kwartaal.R` | event study op kwartaalbasis, twee uitspraken | `_output/eventstudy_kw_<extract>*` |
| `06_rapport.R` | alle schattingen voor het rapport (hoofdstuk 2 tot en met 7, 10 en 11) | `_output/rapport_<extract>.json` |
| `07_plannen.R` | het plannenspoor (hoofdstuk 12) | `_output/plannen_<extract>.json` |
| `08_strikt.R` | nabij en ver binnen gemeente en kwartaal, trendbreuk, ringen (hoofdstuk 8) | `_output/strikt_<extract>.json` |
| `09_aanvragen.R` | de bekendmakingen: aanvragen, uitkomst, doorlooptijd (hoofdstuk 9) | `_output/aanvragen_<extract>.json` |
| `09a_titels.py` | strenge typering van nieuwbouw en stallen uit de titels, en de projectsoort volgens `analyse_export.projectsoort` (met de woordgrens van 28 september 2026) voor 09, 13, 24 en 27 (`vergunningen.sqlite`) | `_derived/aanvragen_titels.csv` |
| `10_landelijk.R` | landelijke reeksen: vergunningen, rente, kosten, belemmeringen (hoofdstuk 14) | `_output/landelijk_<extract>.json` |
| `11_ruimte.R` | contour, netcongestie en depositie op de objecten (hoofdstuk 10, 15, 16) | `_output/ruimte_<extract>.json` |
| `11a_rasters.py` | leest de rasters af op de BAG-objecten (Python van QGIS) | `_derived/vbo_raster.csv` |
| `11b_contour_vroeg.py` | verstedelijkingscontour van 2010 tot en met 2014 als raster (Python van QGIS) | `_derived/stedelijk_vroeg_25m.tif` |
| `12_beroep.R` | Raad van State en uitspraken over woningbouw (hoofdstuk 17) | `_output/beroep_<extract>.json` |
| `13_binnenring.R` | de binnenste kilometer sinds 2024 per woningtype, de toets op wegvallende registraties en de golf aanvragen rond de Omgevingswet (hoofdstuk 13) | `_output/binnenring_<extract>.json` |
| `14_netcongestie.R` | netcongestie in de tijd: vol op 3 maart 2024 binnen de gemeente, en een gespreide invoering per voedingsgebied (hoofdstuk 16) | `_output/netcongestie_<extract>.json` |
| `14a_pc6.py` | postcode (PC6) per BAG-punt uit de vlakken van de capaciteitskaart (Python van QGIS) | `_derived/vbo_pc6.csv` |
| `14b_congestiedatum.py` | eerste datum van afnamecongestie per PC6 uit ACM-register, TenneT, Enexis en kaartstanden | `_derived/pc6_congestie.csv` |
| `15_natuurbesluiten.R` | grote BAG-projecten voor de koppeling aan natuurbesluiten (hoofdstuk 13) | `_derived/projecten_20plus.csv` |
| `15a_natuurbesluiten.py` | provinciale natuurbesluiten over woningnieuwbouw: tellingen, koppeling met kansniveau | `_output/natuurbesluiten_<extract>.json` |
| `16_kernen.R` | nabij tegenover ver binnen de gemeente, gemeente x kerntype en kern (hoofdstuk 15) | `_output/kernen_<extract>.json` |
| `16a_kernen.py` | kernen uit de verstedelijkingscontour, inwoners en hoofdkern per gemeente (Python van QGIS) | `_derived/vbo_kern.csv` |
| `17_registratie.R` | het registratiemoment van de BAG-status Bouwvergunning verleend: tijd tot de bouwstart, datum tegenover het gepubliceerde besluit, binnenste kilometer, de piek van juni 2026 (hoofdstuk 13) | `_output/registratie_<extract>.json` |
| `17a_pand_zaak.py` | koppeling van BAG-panden aan zaken uit `vergunningen.sqlite` (alleen lezen) | `_derived/pand_zaak.csv` |
| `17b_besluit_nabij.py` | per pand een verleend besluit binnen 100 m, recent of ouder dan een half jaar | `_derived/besluit_nabij.csv` |
| `18_nnn.R` | het Natuurnetwerk Nederland en Natura 2000 zonder habitat als verklaring voor de achterstand nabij natuur (hoofdstuk 15) | `_output/nnn_<extract>.json` |
| `18a_nnn.py` | afstand van elk BAG-punt tot het NNN, met en zonder Natura 2000, en tot Natura 2000 (Python van QGIS) | `_derived/vbo_nnn.csv` |
| `19_ringen.R` | waar de ring van 2,5 tot 5 km groeide, en de ringen met en zonder de grootste steden (hoofdstuk 8 en 15) | `_output/ringen_<extract>.json` |
| `20_plannen_nh.R` | plancapaciteit nabij natuur tegenover de eerdere bouw, Noord-Holland met Almere en Lelystad (hoofdstuk 15) | `_output/plannen_nh_<extract>.json` |
| `20a_plannen_nh.py` | kleinste afstand van elk plan in de openbare Noord-Hollandse monitor (2023 en 2025) tot habitat (Python van QGIS) | `_derived/plannen_nh.csv` |
| `21a_econlogic.R` | slanke afgeleide van de objectexport van het densification-paper (EconLogicPaper, 8,7 miljoen woningen met mutatietype, bouwjaar, woningtype en buurtdichtheid; pad in `ECONLOGIC_PEROBJECT`) | `_derived/econlogic_vbo.rds` |
| `21_verdichting.R` | nabij tegenover ver binnen dezelfde buurtdichtheid en hetzelfde ontwikkeltype (hoofdstuk 15) | `_output/verdichting_<extract>.json` |
| `22a_habitat.py` | afstand tot habitat op het raster van 25 m en depositie 2018 voor punten, planvlakken en de oppervlakte per ring (Python van QGIS); bewaart het afstandsraster | `_derived/habitat_afstand_25m.tif`, `langereeks_habitat.csv`, `plannen_nl.csv`, `ringen_oppervlakte.csv` |
| `22_langereeks.R` | nieuwe woningen naar bouwjaar sinds 1995, nabij en ver en per depositiekwartiel, en de ringen naast elkaar (hoofdstuk 8 en 10) | `_output/langereeks_<extract>.json` |
| `23_plannen_nl.R` | plancapaciteit nabij natuur in de nationale woningbouwkaart van VRO tegenover de eerdere bouw, met een bootstrap over gemeenten (hoofdstuk 15) | `_output/plannen_nl_<extract>.json` |
| `24_uitgebreid.R` | waarover de uitgebreide aanvragen gingen die nabij natuur na de uitspraak wegvielen: natuur, brandveilig gebruik of afwijken van het plan, uit de vlaggen van `24a_teksten.py teksten` (prioriteit 1 van de tekstoogst is compleet sinds 28 september 2026); ook per gemeente en periode, met de dragende gemeenten en weglaten per gemeente | `_output/uitgebreid_<extract>.json` |
| `24a_teksten.py` | natuur en stikstof in de zaken met een uitgebreide procedure: `samenvatting` telt ze in titel en samenvatting (`vergunningen.sqlite`, alleen lezen), `teksten` maakt vlaggen per zaak uit de volledige teksten van `vergunningen/pipeline/teksten.py` | `_output/uitgebreid_samenvatting_<extract>.txt`, `_derived/teksten_vlaggen.csv` |
| `25_omvang.R` | het tweede kanaal: werden projecten nabij natuur na 2019 kleiner in plaats van minder? Omvang per project (log-OLS en Poisson), aantal projecten en woningen, verdeling over omvangsklassen, en het pseudoproject in Den Haag in de telling van hoofdstuk 8 | `_output/omvang_<extract>.json` |
| `26_samenstelling.R` | zijn de depositiedaling (hoofdstuk 10) en de stap bij de appartementen in de binnenste kilometer (hoofdstuk 13) een verschil tussen stad en land? Dezelfde vergelijkingen binnen gemeente en buurtdichtheid, binnen de vroege contour en naar afstand tot overbelaste natuur; roept `22a_habitat.py` aan | `_output/samenstelling_<extract>.json` |
| `27a_pijplijn_export.py` | export uit `vergunningen.sqlite` (alleen lezen) voor de koppeling van zaken aan de sites van het herontwikkelingspaper: de BAG-koppeling met alle statusdatums, en per gemeentelijke zaak sleutel, datums, uitkomst, plek, activiteiten, titels bij sloop of nieuwbouw en beroep | `_derived/pijplijn/pijplijn_pand_zaak.csv`, `pijplijn_zaak.csv` |
| `27_pijplijn_sites.R` | proef voor issue 30 in de repo Redevelopment (EconLogicPaper): de gerealiseerde sites aan hun zaak (procedure, doorlooptijd, vergunning tot start en oplevering per alternatief), de route over het documentnummer, pogingen op niet-ontwikkelde sites uit de bekendmakingen, en vergunde maar niet uitgevoerde vervanging uit de BAG zelf (geplande objecten uit `vbo_base.rds` naast bestaande opstal; `vergunningen/inzichten.md`, hoofdstuk 36) | `_output/pijplijn_sites_<extract>.json` |
| `28a_bestemming.py` | koppelt de punten van de bestaande opstal van de sites van het herontwikkelingspaper aan de vastgestelde Wro-plannen uit de PDOK-download (enkelbestemming, bouwaanduiding, maatvoering, bouwvlak; stand 1 juli 2026), met datum en status per vlak (Python van QGIS met shapely) | `_derived/bestemming/<laag>_paren.csv`, `<laag>_vlakken.csv` |
| `28_bestemming_sites.R` | de bestemming van die sites in 2012 en nu: hoofdgroep, woningtype volgens de bouwaanduiding, bouwhoogte en wooneenheden; het gerealiseerde alternatief tegenover het toegestane type, planwijzigingen rond het project, en de bestemming van 2012 tegenover herontwikkeling daarna (issue 30 in de repo Redevelopment) | `_output/bestemming_sites_<extract>.json` |
| `29a_plannen_dso.py` | per planversie uit de oogst van de API Ruimtelijke plannen v4 (`C:\LocalData\vergunningen\bron\dso\dso.sqlite`) de oppervlakte, de kleinste afstand van de contour tot de AERIUS relevante habitatkartering en de woonbestemmingen uit de PDOK-laag Enkelbestemming (Python van QGIS met shapely) | `_derived/plannen_dso.csv` |
| `29b_uitspraken_dso.py` | de gepubliceerde gerechtelijke uitspraken die aan een plan hangen, uit dezelfde database (alleen lezen) | `_derived/uitspraken_dso.csv` |
| `29_plannen_natuur.R` | bestemmingsplannen nabij natuur voor en na de PAS-uitspraak: aantal vastgesteld, tijd van ontwerp tot vaststelling en beroep met gevolgen (hoofdstuk 12) | `_output/plannen_natuur_<extract>.json` |
| `30a_wijzigingen_lv.py` | per ontwerp en besluit tot wijziging van een gemeentelijk omgevingsplan in de nieuwe standaard (oogst van de DSO-LV, `C:\LocalData\vergunningen\bron\dso\dso_lv.sqlite`) de vereniging van de gewijzigde locaties uit de renvooi, de oppervlakte en de kleinste afstand tot de AERIUS relevante habitatkartering (Python van QGIS met shapely) | `_derived/wijzigingen_lv.csv` |
| `30_planwijzigingen_natuur.R` | planwijzigingen nabij natuur rond de uitspraken over intern salderen: doorstroom van de bestemmingsplannen onder overgangsrecht en van de TAM-omgevingsplannen, en het aantal ontwerpen en vaststellingen langs de drie wegen (bestemmingsplan, TAM-omgevingsplan, nieuwe standaard) | `_output/planwijzigingen_natuur_<extract>.json` |
| `31a_kennisgevingen.py` | de kennisgevingen van ruimtelijke plannen uit de bekendmakingen (SRU-stroom ruimtelijk in `vergunningen/pipeline`, database `C:\LocalData\vergunningen\plannen_sru\plannen_sru.sqlite`) via het IMRO-nummer gekoppeld aan de plandossiers van de DSO-oogst: per dossier de datum van de kennisgeving van ontwerp, vaststelling en onherroepelijk worden (standaard-Python) | `_derived/kennisgevingen_plannen.csv`, `_derived/kennisgeving_dossier.csv` |
| `31_procedure.R` | de tijd van ontwerp tot vaststelling nabij natuur met drie maten voor de ontwerpdatum (API, kennisgeving, samen), event study per ontwerpjaar, ontwerpen die nooit werden vastgesteld, en de placebo voor de indeling naar vaststellingsdatum (hoofdstuk 12) | `_output/procedure_natuur_<extract>.json` |
| `32a_vergunningen_lv.py` | per omgevingsvergunning in de DSO-LV (tabel `vergunning` in `dso_lv.sqlite`) de afstand van het vergunningsgebied tot het habitat en de projectsoort uit de titel (Python van QGIS met shapely) | `_derived/vergunningen_lv.csv` |
| `32_vergunningen_lv.R` | vergunningen voor een buitenplanse activiteit nabij natuur rond de uitspraken over intern salderen, met klein verbouw als placebo (hoofdstuk 13) | `_output/vergunningen_lv_<extract>.json` |
| `34_tekstvlaggen.R` | natuur, stikstof, soortenbescherming en afwijken van het plan in de volledige tekst van aanvragen voor nieuwe panden (reguliere procedure), nabij tegenover ver rond de PAS-uitspraak en intern salderen, met event study, trendbreuk en weglaten per gemeente, en afwijken naar contour en buurtdichtheid (hoofdstuk 13; leest `_derived/contour_zaken.csv` en `_derived/econlogic_vbo.rds`) | `_output/tekstvlaggen_<extract>.json` |
| `34a_contour_zaken.py` | binnen of buiten de huidige verstedelijkingscontour (`bron\afgeleid\stedelijk_gebied_nl_25m.tif`) op de plek van de zaken met een volledige tekst, met het locatietype uit `vergunningen.sqlite` (alleen lezen; Python van QGIS) | `_derived/contour_zaken.csv` |
| `35a_verordeningen.py` | stedelijk en landelijk gebied in de provinciale omgevingsverordeningen op 1 januari 2024, 2025 en 2026 en actueel, uit de zips van de DSO-LV (`bron\dso\lv_download`): oppervlakte per aanwijzing, aandeel nabij natuur en wat er tussen twee peildatums bij kwam of af ging (Python van QGIS met shapely; hoofdstuk 15) | `_derived/verordening_aanwijzingen.csv`, `_derived/verordening_wijzigingen.csv`, `_derived/verordening_wijzigingen.gpkg` |
| `36a_voorbereiding.py` | voorbereidingsbesluiten en exploitatieplannen van gemeenten uit de kennisgevingen (SRU-stroom ruimtelijk), per plandossier, met de variant uit de titel (nieuw, herziening, geen exploitatieplan, intrekking), de plek uit het register van de API Ruimtelijke plannen of uit de kennisgeving, de afstand tot het habitat, de oppervlakte en de uitgestrektheid (Python van QGIS met shapely; hoofdstuk 12) | `_derived/voorbereiding.csv` |
| `36_voorbereiding.R` | aandeel nabij natuur van voorbereidingsbesluiten en exploitatieplannen voor en na de PAS-uitspraak, op kleine plekken en alle, binnen de gemeente en landelijk (hoofdstuk 12) | `_output/voorbereiding_<extract>.json` |
| `rapport/bouw_rapport.js` | voegt de JSON's in de HTML-bron en schrijft het rapport | `_output/rapport_stikstof_<extract>.html` |

Script 09 tot en met 12 lezen de datamap van het vergunningenspoor
(`VERGUNNINGEN_DATA`, standaard de OneDrive-projectmap; zie `00_config.R` en
`vergunningen/CLAUDE.md`). Voor 11a en 11b is de Python van QGIS nodig
(`QGIS_PY`, standaard `C:\Program Files\QGIS 3.40.7\bin\python-qgis-ltr.bat`);
`11_ruimte.R` roept ze zelf aan als hun uitvoer ontbreekt of verouderd is.

`30_planwijzigingen_natuur.R` staat niet in `run_all.R`: het vraagt `29a_plannen_dso.py` en, voor de wijzigingen in de
nieuwe standaard, de oogst van de DSO-LV (`bron\dso\werk\dso_lv_omgevingsplan.py`) en `30a_wijzigingen_lv.py` vooraf.
`29_plannen_natuur.R` en `31_procedure.R` staan niet in `run_all.R`: ze vragen de oogst van de DSO-API, `31a_kennisgevingen.py` (met de SRU-stroom ruimtelijk) en
`29a_plannen_dso.py` (Python van QGIS) en `29b_uitspraken_dso.py` vooraf.
`24_uitgebreid.R` (wacht op de tekstoogst), `27_pijplijn_sites.R` en `28_bestemming_sites.R` staan niet in `run_all.R`: 27 en 28 zijn
proeven voor het herontwikkelingspaper, lezen de EconLogic-bestanden in
`C:/LocalData/Redevelopment/R_werk` (pad in `ECONLOGIC_WERK`); 27 draait na
`27a_pijplijn_export.py _derived/pijplijn`, 28 na `28a_bestemming.py` (Python van QGIS).

`_derived/` en `_output/` staan in `.gitignore`: het zijn afgeleide bestanden.

## Data

De ruwe exports staan **niet** in deze repo. Ze worden gegenereerd door
`cfg/main/Analyse/N2000_Analyse.dms` en weggeschreven naar de Teams-map:

```
D:\Radboud Universiteit\Woningbouw RU-VU - General\data\data_natuur\input_woningen\
```

Overrulen kan met de omgevingsvariabele `STIKSTOF_DATA`. Het extract wordt
gekozen met `EXTRACT` in `00_config.R` (nu `20260730`; de bijbehorende
`BAG_file_date` staat in de `.xml` naast elke CSV, hier `20260710`).

| bestand | eenheid | rijen | wat |
|---|---|---|---|
| `Object_BouwvergunningVerleend_*.csv` | VBO | 1.313.420 | elk VBO waarvoor een bouwvergunning is verleend |
| `Object_Nieuwbouw_*.csv` | VBO | 927.382 | elk VBO dat CBS als nieuwbouw telt, met mutatiemaand |
| `Pand_VergunningDuur_*.csv` | pand | 620.816 | tijdlijn per pand: vergunning → bouwstart → oplevering |

De koppeling levert 1.356.420 VBO's op. Panden uit het vergunningenbestand
zitten allemaal in `Pand_VergunningDuur` (0 ontbrekend); andersom hebben
43.000 nieuwbouw-VBO's helemaal geen vergunningrecord.

## Drie ontwerpkeuzes

### 1. De ruggengraat is de vereniging, niet de doorsnede

Van de CBS-nieuwbouw-VBO's heeft in 2012 **24,5%** geen vergunningrecord,
in 2013 13,4%, vanaf 2015 rond de 1,5–3%. De BAG-vergunninghistorie loopt
gewoon niet ver genoeg terug. Wie die records via een `merge` laat wegvallen,
maakt van de koppelgraad een verborgen selectiecriterium dat sterk met het
jaar samenhangt. Ze blijven dus staan, met de vlag `bron_vergunning`.

### 2. Project-id = gemeente + documentnummer

De Stata-pipeline gebruikt `vbo_documentnummer_rel`. Dat veld bestaat alleen
in het vergunningenbestand, dus is het leeg voor precies de VBO's uit punt 1.
En het is te grof: 1.308 `_rel`-waarden lopen over meerdere gemeenten heen,
omdat documentnummers als `RB 040/07` in verschillende gemeenten voorkomen.

De sleutel `gemeentecode + vbo_documentnummer` staat in **beide**
objectbestanden, splitst geen enkel bestaand `_rel`-project (0 gevallen), en
herstelt wel die 1.308. Geverifieerd in `03_checks.R`, CHECK 1.

Dit is niet cosmetisch. In Stata staat:

```stata
collapse (sum) nieuwbouw (min) n2000_afstandsklasse_250, ///
    by(projectid_doc projectomvang_doc gemeentecode jaar provinciecode provincienaam)
```

`collapse` behandelt `projectid_doc == .` als een gewone groepswaarde. Alle
42.429 niet-gekoppelde VBO's binnen een gemeente-jaar worden dus één
pseudo-project (grootste: 1.308 VBO's) en krijgen via `(min)` allemaal de
**kleinste** afstandsklasse van die hoop. Omdat dat vooral in 2012–2013
gebeurt, ontstaat er kunstmatig extra nieuwbouw vlak bij N2000 in de
pre-periode. Effect op de DiD-coëfficiënt (nieuwbouw):

| drempel | eigen project-id | NA gepoold |
|---|---|---|
| 250 m | −0,099 | −0,325 |
| 500 m | −0,160 | −0,327 |
| 1000 m | −0,123 | −0,260 |
| 2500 m | −0,187 | −0,258 |

En in de event study wordt 2012 op ≤1000 m een IRR van 2,08 [1,50–2,87] in
plaats van 1,35 [0,95–1,92] — een gefabriceerde pre-trend.

### 3. Drie assen in plaats van één `status`

De Stata-versie perst alles in één variabele met zeven niveaus en selecteert
daarop (`keep if inlist(status,2,4,7)`). Dat mengt drie dingen:

| as | variabele | waarden |
|---|---|---|
| wat voor project was dit? (ex ante) | `projecttype` | `nieuwbouw`, `verbouwing`, `admin_fout` |
| wat is ermee gebeurd? (ex post) | `uitkomst` | `gerealiseerd_nieuwbouw`, `gerealiseerd_overig`, `uitval`, `pipeline`, `onbepaald` |
| wat weten we nog niet? | `opvolgtijd_jaar`, `gerealiseerd_binnen_Njr` | — |

**Filteren doe je op `projecttype`, meten op `uitkomst`.** Selecteren op de
uitkomst is hier niet onschuldig: niet-realisatie is juist het veronderstelde
effect van stikstof.

Het oude filter is bovendien intern asymmetrisch. Status 6 (gerealiseerd,
geen nieuwbouw) valt weg, maar status 7 (nog niet opgeleverd) blijft staan —
terwijl daar de toekomstige 6'en in zitten. Naarmate een vergunningcohort
ouder wordt lekt 7 naar 6 en valt weg. Dat is per constructie een
jaarafhankelijke uitval.

Realisatie wordt bepaald op `vbo_status`, niet op de pandstatus, conform de
CBS-voorraaddefinitie (`docs/BAG-dataschoning-inzichten.md` §4). Dat lost
meteen de inconsistentie op waar dit onderzoek mee begon: 2.950 VBO's met
pandstatus `Niet_gerealiseerd_pand` staan zelf op "in gebruik". Dat is geen
aggregatie-artefact — van de 23.972 betrokken panden zijn er maar 54 gemengd —
maar een echte tegenspraak in de BAG.

## Wat de filterkeuze doet

Coëfficiënt op `tr` in de vergunningenanalyse:

| variant | N | 250 m | 500 m | 1000 m | 2500 m |
|---|---|---|---|---|---|
| ex ante (`in_sample`) | 1.006.018 | −0,239 | −0,302 | −0,229 | −0,166 |
| idem + verbouwingen | 1.012.180 | −0,242 | −0,307 | −0,235 | −0,164 |
| Stata-equivalent (2,4,7) | 988.162 | −0,249 | −0,328 | −0,234 | −0,164 |
| alleen gerealiseerd | 797.053 | −0,143 | −0,188 | −0,156 | −0,146 |

De eerste drie zijn niet van elkaar te onderscheiden. Alleen "alleen
gerealiseerd" wijkt af, en dat is precies de variant die niet mag: die
conditioneert op de uitkomst. Zie de volgende paragraaf.

## Censurering

Aandeel vergunningen dat nog in de pijplijn zit, per vergunningjaar:

| vergunningjaar | ≤1000 m | >1000 m |
|---|---|---|
| 2021 | 7,1% | 4,7% |
| 2022 | 13,5% | 17,6% |
| 2023 | 24,4% | 31,2% |
| 2024 | 49,1% | 60,9% |
| 2025 | 84,3% | 91,2% |

Het extract is een momentopname; "niet opgeleverd" betekent iets heel anders
voor een cohort uit 2015 dan voor een uit 2024, en het verschil loopt niet
gelijk op tussen nabij en ver. Voor uitval- en duuranalyses gebruik je
daarom `gerealiseerd_binnen_3jr` / `_4jr` / `_5jr`: die zijn `NA` zolang het
venster niet vol is, zodat cohorten onderling vergelijkbaar blijven.

## Jaargrenzen

- **2026 valt altijd af**: het nieuwbouwbestand heeft maar 7 mutatiemaanden
  in 2026 (t/m juli).
- **2024 is een breukjaar**: Omgevingswet en Wkb "de knip" per 1-1-2024,
  zie `docs/BAG-dataschoning-inzichten.md` §3. `04_analyse_aanbod.R`
  rapporteert de reeks t/m 2023 als gevoeligheidsanalyse; dat scheelt bij
  250 m −0,239 tegen −0,156.
- **2012–2013** zijn bruikbaar voor nieuwbouwtellingen, maar niet voor
  analyses die op de vergunningkoppeling leunen.

## Verhouding tot de Stata-scripts

De do-files staan in de Teams-map onder `analyses/analysis_stikstof/`. Deze
pipeline vervangt:

| Stata | R |
|---|---|
| `do_analysis_files_stikstof.do`, blok "BESTANDEN STANDAARDISEREN" | `01_build_vbo.R` |
| idem, `gen status` + `recode` | `02_classify.R` |
| idem, blok "BEPALEN BLOOTSTELLING" | `00_config.R` (`afstandsklasse()`, `mol_per_woning()`) + `01_build_vbo.R` |
| `do_analysis_stikstof_jaar.do`, blokken NIEUWBOUW en VERGUNNINGEN | `04_analyse_aanbod.R` |

Nog niet geport: het plannenblok (`afstand_n2000_plannen.dta`, provinciale
plancapaciteitsbestanden) en de duur/`cloglog`-analyses.

Fouten in de Stata-versie die hier niet zijn overgenomen, naast de
project-id-pooling hierboven:

1. `do_analysis_stikstof_jaar.do:81` — `drop if status_invalide == 1` staat
   vóór de regel waar `status_invalide` wordt aangemaakt (r. 140).
2. `:304` en `:309` — `keep if jaar >= 2012` en `tab jaar_kwartaal` op
   `afstand_n2000_vergunningen.dta`; dat bestand heeft geen `jaar` en geen
   `jaar_kwartaal`.
3. `:239` en `:399` — `recode event (-7=1) ... (6=14)` laat 2026 op
   `event == 7` staan, wat samenvalt met de referentiecategorie `ib7.jtr`
   (= 2018). 2026 wordt daardoor met het basisjaar gepoold.
4. `:1091` — verwijst naar `projectomvang_doc_doc`.
5. `:109-110` — legenda zegt "5.000m" in alle vier de panelen van de lus
   over `500 1000 2500 5000`.

## Aandachtspunten

- `pand_status` in het nieuwbouwbestand is een **snapshot** op de
  mutatiemaand, niet de eindstatus: in 697.094 van de 884.382 gekoppelde
  rijen wijkt hij af van `last_pand_status`. Beide zitten in het
  analysebestand, onder verschillende namen (`pand_status_mutatie` en
  `last_pand_status`). De Stata-versie gebruikt ze door elkaar.
- Voor de 43.000 VBO's zonder vergunningrecord was het gebruiksdoel onbekend.
  Sinds [issue #27](https://github.com/jipclaassens/Stikstof/issues/27) levert
  `Object_Nieuwbouw` ook `last_vbo_status` en `last_vbo_gebruiksdoel` mee, uit
  dezelfde `uq_vbo` als het vergunningenbestand, zodat die VBO's een echte
  waarde krijgen. `01_build_vbo.R` vult ze aan waar het vergunningenbestand
  niets zegt; `03_checks.R` CHECK 9 controleert dat beide bronnen het eens zijn
  waar ze overlappen. Dit vergt wel een nieuwe GeoDMS-export: op extracts t/m
  `20260730` ontbreken de kolommen en valt de pipeline terug op de oude
  afleiding (`Object_Nieuwbouw` is de CBS-woningreeks, 99,9% van de gekoppelde
  VBO's heeft `woon`, dus die krijgen `woon` met de vlag
  `gebruiksdoel_afgeleid`).
- `IsCensored` uit `Pand_VergunningDuur` is een vlag op pandniveau en
  `IsNieuwbouw` een vlag op VBO-niveau; voor 1.907 panden (0,31%) spreken ze
  elkaar tegen. Zie `docs/BAG-dataschoning-inzichten.md` §9 en CHECK 8. Gebruik
  `IsCensored` niet los als survival-indicator.
- De renovatiedrempel `reno_delta = -6` is een eigen werkafspraak en bovendien
  inert: de robuustheidscheck −4, −6 en −9 geeft op 250 m −0,2398, −0,2394 en
  −0,2399, bij een standaardfout van 0,088. Zie docs §8 voor de onderbouwing.
- De EIB-depositiecurve (`PARAMS$eib_a`, `eib_b`) is overgenomen uit de
  Stata-versie en hier niet opnieuw geschat.
