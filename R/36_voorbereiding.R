## =====================================================================
## 36_voorbereiding.R -- kwamen er na de PAS-uitspraak nabij natuur minder
##                       voorbereidingsbesluiten en exploitatieplannen?
##
## Input : _derived/voorbereiding.csv (36a_voorbereiding.py: een rij per
##         voorbereidingsbesluit of exploitatieplan uit de kennisgevingen
##         van gemeenten, met de afstand tot het habitat)
## Output: _output/voorbereiding_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Een voorbereidingsbesluit bevriest een gebied in afwachting van een
## nieuw plan; een exploitatieplan regelt het kostenverhaal van een nieuw
## bouwgebied als de gemeente daarover geen overeenkomst sloot. Minder
## nieuwe exploitatieplannen nabij natuur na de uitspraak zou wijzen op
## minder nieuwe bouwgebieden daar. Beide zijn schaars: tientallen per
## jaar met een plek, dus eerst tellen en dan kijken of een vergelijking
## nabij tegenover ver iets kan zeggen. Venster tot en met 2023: onder de
## Omgevingswet bestaat het exploitatieplan niet meer. Voorbereidingsbe-
## sluiten pas vanaf 2016, daarvoor hebben de kennisgevingen zelden een
## plek.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
sink(file.path(PATHS$output, sprintf("voorbereiding_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
PAS <- as.Date("2019-05-29")

v <- fread(file.path(PATHS$derived, "voorbereiding.csv"), sep = ";", encoding = "UTF-8")
v[, datum := as.IDate(datum)]
v[, groep := fcase(soort == "voorbereidingsbesluit" & variant == "besluit", "voorbereidingsbesluit",
                   soort == "exploitatieplan" & variant == "nieuw", "exploitatieplan, nieuw",
                   soort == "exploitatieplan" & variant == "herziening", "exploitatieplan, herziening")]
R$dekking <- v[, .(gebeurtenissen = .N, met_plek = sum(!is.na(afstand_m)), plek_plan = sum(plek == "plan"), plek_kennisgeving = sum(plek == "kennisgeving")),
               by = .(soort, variant)][order(soort, variant)]
kop("Voorbereidingsbesluiten en exploitatieplannen in de kennisgevingen van gemeenten (2013 tot en met september 2026)")
print(R$dekking)
e <- v[!is.na(groep) & !is.na(afstand_m)]
e[, `:=`(jaar = year(datum), D = as.integer(afstand_m <= 2500), na = as.integer(datum >= PAS))]
e <- e[jaar <= 2023 & (jaar >= 2016 | groep != "voorbereidingsbesluit")]
## Een besluit voor de hele gemeente of het hele buitengebied raakt bijna altijd de 2.500 meter; zulke brede besluiten (vaak
## thematisch: kamerverhuur, geitenhouderijen, recreatiewoningen) werden na 2019 talrijker. Vergelijk daarom op kleine plekken:
## hoogstens 10 hectare en een omhullende rechthoek met een diagonaal van hoogstens 2 kilometer.
e[, maat := fifelse(opp_ha <= 10 & uitgestrekt_m <= 2000, "klein", "breed")]
R$per_jaar <- dcast(e[, .N, by = .(groep, maat, jaar, kant = fifelse(D == 1L, "nabij", "ver"))], groep + maat + jaar ~ kant, value.var = "N", fill = 0)
R$per_jaar[, aandeel_nabij := 100 * nabij / (nabij + ver)]
print(R$per_jaar[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
R$maat <- e[, .(gebeurtenissen = .N, nabij = 100 * mean(D)), by = .(groep, maat, periode = fifelse(na == 1L, "na", "voor"))][order(groep, maat, -periode)]
kop("Aandeel nabij naar maat van de plek, voor en na de uitspraak")
print(R$maat[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])

## ---- aandeel nabij voor en na de uitspraak --------------------------------------
## Lineair binnen de gemeente: ligt een gebeurtenis na de uitspraak minder vaak nabij dan de gebeurtenissen van dezelfde gemeente
## ervoor? En landelijk Poisson op de aantallen per kant en jaar met kant en jaar als vaste effecten (de verhouding nabij tegenover
## ver na tegenover voor; 2019 weggelaten, na vanaf 2020). Een Poisson per gemeente met gemeente x jaar kan hier niet: de meeste
## gemeenten hebben een of twee gebeurtenissen.
res <- list()
for (g in unique(e$groep)) for (mt in c("klein", "alle")) {
  d <- if (mt == "alle") e[groep == g] else e[groep == g & maat == mt]
  d[, nabij := 100 * D]
  m1 <- feols(nabij ~ na | gemeente, data = d, cluster = ~gemeente, notes = FALSE)
  p <- CJ(D = 0:1, jaar = setdiff(min(d$jaar):2023, 2019L))
  p[d[, .N, by = .(D, jaar)], on = c("D", "jaar"), n := i.N]
  p[is.na(n), n := 0L]
  p[, na := D * as.integer(jaar >= 2020L)]
  m2 <- fepois(n ~ na | D + jaar, data = p, vcov = "hetero", notes = FALSE)
  x1 <- coeftable(m1); x2 <- coeftable(m2)
  res[[paste(g, mt)]] <- data.table(groep = g, maat = mt, gebeurtenissen = nrow(d), gemeenten = uniqueN(d$gemeente),
                                    nabij_voor = d[na == 0L, mean(nabij)], nabij_na = d[na == 1L, mean(nabij)],
                                    aandeel_b = x1["na", 1], aandeel_se = x1["na", 2], aandeel_p = x1["na", 4], aandeel_n = nobs(m1),
                                    verhouding_pct = 100 * (exp(x2["na", 1]) - 1), verhouding_laag = 100 * (exp(x2["na", 1] - 1.96 * x2["na", 2]) - 1),
                                    verhouding_hoog = 100 * (exp(x2["na", 1] + 1.96 * x2["na", 2]) - 1), verhouding_p = x2["na", 4])
}
R$pas <- rbindlist(res)
kop("Na de PAS-uitspraak: aandeel nabij binnen de gemeente (procentpunt) en de landelijke verhouding nabij tegenover ver (procent, 95%%-interval)")
print(R$pas[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("voorbereiding_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
