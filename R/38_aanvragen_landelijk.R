## =====================================================================
## 38_aanvragen_landelijk.R -- hoeveel wordt er landelijk aangevraagd,
##                             verleend en geweigerd, per jaar?
##
## Input : analyse/zaken.csv in de datamap van de bekendmakingen
##         (VERGUNNINGEN_DATA); _derived/aanvragen_titels.csv (09a_titels.py)
## Output: _output/aanvragen_landelijk_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 9 vergelijkt nabij en ver. Hier het landelijke beeld: het
## aantal aanvragen per jaar en wat ermee gebeurde. De totalen van alle
## gemeenten stijgen tot 2021 vooral doordat steeds meer gemeenten hun
## bekendmakingen in het elektronische blad zetten (sinds juli 2021 elke
## gemeente). Een tijdreeks kan daarom alleen op een vaste groep
## gemeenten: die met minstens veertig aanvragen in elk jaar van 2016 tot
## en met 2025. Daarnaast het aantal per gemeente in de bekendmakingen.
## Weigeringen als aandeel van de gepubliceerde besluiten (verleend plus
## geweigerd); zaken zonder gepubliceerd besluit tellen daar niet in mee.
## Nieuwbouw van woningen streng getypeerd, zoals 09_aanvragen.R.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)
sink(file.path(PATHS$output, sprintf("aanvragen_landelijk_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()

z <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
           select = c("zaak_id", "gemeente", "datum_aanvraag", "uitkomst", "verleend", "geweigerd", "ingetrokken", "vergunningvrij",
                      "besluit_onbekend", "geen", "uitgebreid"))
tt <- fread(file.path(PATHS$derived, "aanvragen_titels.csv"), select = c("zaak_id", "streng", "tijdelijk", "soort"))
z[tt, on = "zaak_id", nb := as.integer(i.soort == "nieuwbouw_woning" & i.streng %in% 1L & !(i.tijdelijk %in% 1L))]
z[is.na(nb), nb := 0L]
d <- as.IDate(substr(as.character(z$datum_aanvraag), 1, 10))
z[, `:=`(jaar = year(d), kwartaal = sprintf("%dQ%d", year(d), (month(d) - 1L) %/% 3L + 1L))]
LAATSTE <- max(d, na.rm = TRUE)
stopifnot(!anyNA(z$jaar))

tel <- function(x) x[, .(aanvragen = .N, verleend = sum(verleend), geweigerd = sum(geweigerd), ingetrokken = sum(ingetrokken),
                         vergunningvrij = sum(vergunningvrij), besluit_onbekend = sum(besluit_onbekend), geen_besluit = sum(geen),
                         nieuwbouw = sum(nb), nieuwbouw_geweigerd = sum(nb * geweigerd), nieuwbouw_verleend = sum(nb * verleend))]
aandelen <- function(x) x[, `:=`(pct_geweigerd = 100 * geweigerd / (verleend + geweigerd),
                                 pct_nieuwbouw_geweigerd = 100 * nieuwbouw_geweigerd / (nieuwbouw_verleend + nieuwbouw_geweigerd))]

## ---- alle gemeenten -----------------------------------------------------
R$jaar_alle <- z[, c(list(gemeenten = uniqueN(gemeente)), tel(.SD)), by = jaar][order(jaar)]
R$jaar_alle[, per_gemeente := aanvragen / gemeenten]
aandelen(R$jaar_alle)
kop("Alle gemeenten in de bekendmakingen, per jaar van de aanvraag (%s tot en met %s)", min(d, na.rm = TRUE), LAATSTE)
print(R$jaar_alle[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])

## ---- een vaste groep gemeenten ------------------------------------------
pj <- z[, .N, by = .(gemeente, jaar)]
VAST <- sort(pj[jaar %in% 2016:2025 & N >= 40L, .N, by = gemeente][N == 10L, gemeente])
v <- z[gemeente %chin% VAST]
R$jaar_vast <- v[, tel(.SD), by = jaar][order(jaar)]
aandelen(R$jaar_vast)
R$jaar_vast[, `:=`(index = 100 * aanvragen / aanvragen[jaar == 2016], index_nieuwbouw = 100 * nieuwbouw / nieuwbouw[jaar == 2016])]
kop("Vaste groep: %s gemeenten met minstens veertig aanvragen in elk jaar van 2016 tot en met 2025 (%s van de %s aanvragen in die jaren)",
    length(VAST), nfmt(v[jaar %in% 2016:2025, .N]), nfmt(z[jaar %in% 2016:2025, .N]))
print(R$jaar_vast[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
R$kwartaal_vast <- v[jaar >= 2016L, .(aanvragen = .N, nieuwbouw = sum(nb)), by = kwartaal][order(kwartaal)]
kop("Vaste groep per kwartaal, 2022 tot en met %s", LAATSTE)
print(R$kwartaal_vast[kwartaal >= "2022Q1"])

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), laatste_aanvraag = format(LAATSTE),
               zaken = nrow(z), vast_gemeenten = length(VAST), vast_regel = "minstens 40 aanvragen in elk jaar van 2016 tot en met 2025")
writeLines(to_json(R), file.path(PATHS$output, sprintf("aanvragen_landelijk_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
