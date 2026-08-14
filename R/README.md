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
| `03_checks.R` | datakwaliteitscontroles | `_output/checks_<extract>.txt` |
| `04_analyse_aanbod.R` | DiD en event study, vergunningen en nieuwbouw | `_output/aanbod_<extract>.txt` |

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
- Voor de 43.000 VBO's zonder vergunningrecord is het gebruiksdoel onbekend.
  `Object_Nieuwbouw` is de CBS-woningreeks (99,9% van de gekoppelde VBO's
  heeft gebruiksdoel `woon`), dus die krijgen `woon` toegewezen met de vlag
  `gebruiksdoel_afgeleid`.
- De EIB-depositiecurve (`PARAMS$eib_a`, `eib_b`) is overgenomen uit de
  Stata-versie en hier niet opnieuw geschat.
