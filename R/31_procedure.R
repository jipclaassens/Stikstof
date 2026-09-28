## =====================================================================
## 31_procedure.R -- de tijd van ontwerp tot vaststelling nabij natuur voor
##                   en na de PAS-uitspraak, met de ontwerpdatums uit de
##                   kennisgevingen
##
## Input : _derived/plannen_dso.csv (29a_plannen_dso.py);
##         _derived/kennisgeving_dossier.csv (31a_kennisgevingen.py: per
##         dossier de eerste kennisgeving van ontwerp, vaststelling en
##         onherroepelijk worden uit de officiele bekendmakingen)
## Output: _output/procedure_natuur_<extract>.json en .txt
##
## Waarom: deel B van 29_plannen_natuur.R rust op de ontwerpversies in de
## historie van de API, die voor plannen van 2014 tot en met 2018 maar bij 8
## tot 13 procent bestaan. De kennisgevingen geven een ontwerpdatum voor 61
## tot 87 procent (inzichten.md, hoofdstuk 42). Dezelfde modellen als in 29,
## met drie maten voor de ontwerpdatum: de API (zoals in 29), de
## kennisgeving, en de kennisgeving met de API als aanvulling (hoofdmaat).
## Daarnaast een event study naar het jaar van het ontwerp en het aandeel
## ontwerpen dat nooit werd vastgesteld.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
t0 <- Sys.time()
sink(file.path(PATHS$output, sprintf("procedure_natuur_%s.txt", EXTRACT)), split = TRUE)
R <- list()
pct <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)
ROND <- function(x, d = 3) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]
PAS <- as.IDate("2019-05-29")
ct <- function(m, termen, naam) {
  x <- coeftable(m)
  data.table(groep = naam, term = termen, b = x[termen, 1], se = x[termen, 2], p = x[termen, 4], n = nobs(m))
}

## ---- dossiers, zoals in 29_plannen_natuur.R ------------------------------
p <- fread(file.path(PATHS$derived, "plannen_dso.csv"), sep = ";", encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "dossier", "overheidscode", "overheidsnaam", "naam", "planstatus",
                                           "beroep_en_bezwaar", "dossierstatus")))
p[, datum := as.IDate(substr(datum, 1, 10))]
p <- p[grepl("^[0-9]{4}$", overheidscode) & !startsWith(overheidscode, "99") & overheidscode != "0000" &
       !grepl("^(provincie|ministerie|rijk)", tolower(overheidsnaam)) & !is.na(dossier) & dossier != ""]
VAST <- c("vastgesteld", "onherroepelijk", "vigerend", "goedgekeurd")
v <- p[planstatus %chin% VAST & !is.na(datum)]
setorder(v, dossier, datum, identificatie)
ds <- v[!duplicated(dossier), .(dossier, type, gemeente = overheidscode, vaststelling = datum, opp_ha, afstand_m)]
ont <- p[planstatus == "ontwerp" & !is.na(datum), .(ontwerp_api = min(datum)), by = dossier]
ds[ont, on = "dossier", ontwerp_api := i.ontwerp_api]
kg <- fread(file.path(PATHS$derived, "kennisgeving_dossier.csv"), sep = ";", encoding = "UTF-8", colClasses = list(character = "dossier"))
kg[, ontwerp_kg := as.IDate(ontwerp_kg)]
ds[kg, on = "dossier", ontwerp_kg := i.ontwerp_kg]
ds <- ds[type == "bestemmingsplan" & !is.na(afstand_m)]
ds[, `:=`(D = as.integer(afstand_m <= 2500), klein = !is.na(opp_ha) & opp_ha <= 10)]
## een ontwerpdatum na de vaststelling of meer dan 2.000 dagen ervoor telt niet (een herhaald ontwerp of een ander dossier)
geldig <- function(o, vs) fifelse(!is.na(o) & o <= vs & as.integer(vs - o) < 2000L, o, as.IDate(NA))
ds[, `:=`(ontwerp_api = geldig(ontwerp_api, vaststelling), ontwerp_kg = geldig(ontwerp_kg, vaststelling))]
ds[, ontwerp_comb := fifelse(!is.na(ontwerp_kg), ontwerp_kg, ontwerp_api)]
kop("Bestemmingsplannen met een vastgestelde versie: %s; ontwerpdatum uit de API %s, uit een kennisgeving %s, samen %s",
    nfmt(nrow(ds)), nfmt(ds[!is.na(ontwerp_api), .N]), nfmt(ds[!is.na(ontwerp_kg), .N]), nfmt(ds[!is.na(ontwerp_comb), .N]))
R$dekking <- ds[year(vaststelling) %between% c(2014L, 2023L),
                .(dossiers = .N, pct_api = pct(!is.na(ontwerp_api)), pct_kg = pct(!is.na(ontwerp_kg)), pct_samen = pct(!is.na(ontwerp_comb))),
                by = .(jaar = year(vaststelling), D)][order(jaar, D)]
print(ROND(R$dekking, 1))
vgl <- ds[!is.na(ontwerp_api) & !is.na(ontwerp_kg), as.integer(ontwerp_kg - ontwerp_api)]
R$vergelijking <- list(dossiers = length(vgl), mediaan = median(vgl), p05 = unname(quantile(vgl, 0.05)), p95 = unname(quantile(vgl, 0.95)))
cat(sprintf("\nKennisgeving min API-ontwerpdatum bij %s dossiers: mediaan %d dagen (5 procent %d, 95 procent %d)\n",
            nfmt(length(vgl)), as.integer(median(vgl)), as.integer(quantile(vgl, 0.05)), as.integer(quantile(vgl, 0.95))))

## ---- tijd van ontwerp tot vaststelling ----------------------------------
duur <- function(maat, naam) {
  b <- ds[!is.na(get(maat))]
  b[, `:=`(ontwerp = get(maat))]
  b[, `:=`(dagen = as.integer(vaststelling - ontwerp), jaar_o = year(ontwerp), jaar = year(vaststelling))]
  b <- b[dagen > 0]
  b[, `:=`(log_d = log(dagen), na_o = as.integer(ontwerp >= PAS), na_v = as.integer(vaststelling >= PAS))]
  B1 <- b[jaar_o %between% c(2014L, 2021L)]
  m <- function(d, na, fe, lab) {
    ## het jaar als vast effect is het jaar van het ontwerp bij de indeling naar ontwerp, en het jaar van vaststelling bij de indeling
    ## naar vaststelling: met het jaar van het ontwerp selecteert die laatste op de duur zelf (zie de placebo hieronder)
    d <- copy(d)[, na_x := D * get(na)]
    d[, jr := if (na == "na_v") jaar else jaar_o]
    f <- if (fe == "gemeente, jaar, nabij") log_d ~ na_x | gemeente + jr + D else log_d ~ na_x | gemeente^D + jr
    cbind(ct(feols(f, data = d, cluster = ~gemeente, notes = FALSE), "na_x", lab), fe = fe, maat = naam)
  }
  uitk <- rbind(m(B1, "na_o", "gemeente, jaar, nabij", "alle, naar ontwerpdatum 2014 tot en met 2021"),
                m(B1, "na_o", "gemeente x nabij, jaar", "alle, naar ontwerpdatum 2014 tot en met 2021"),
                m(B1[klein == TRUE], "na_o", "gemeente, jaar, nabij", "tot 10 ha, naar ontwerpdatum"),
                m(b[jaar %between% c(2014L, 2023L)], "na_v", "gemeente, jaar, nabij", "alle, naar vaststellingsdatum 2014 tot en met 2023"))
  B1[, cohort := fifelse(ontwerp >= PAS, "na", fifelse(ontwerp >= PAS - 365L, "jaar ervoor", "eerder"))]
  B1[, `:=`(D_jaar = D * (cohort == "jaar ervoor"), D_na = D * (cohort == "na"))]
  mc <- feols(log_d ~ D_jaar + D_na | gemeente + jaar_o + D, data = B1, cluster = ~gemeente, notes = FALSE)
  uitk <- rbind(uitk, cbind(ct(mc, c("D_jaar", "D_na"), "alle, ontwerp in het jaar voor de uitspraak en erna, tegenover eerder"),
                            fe = "gemeente, jaar, nabij", maat = naam))
  es <- feols(log_d ~ i(jaar_o, D, ref = 2018) | gemeente + jaar_o + D, data = B1, cluster = ~gemeente, notes = FALSE)
  x <- coeftable(es)
  list(did = uitk[, pct := 100 * (exp(b) - 1)],
       es = data.table(maat = naam, jaar = as.integer(sub(".*::([0-9]{4}):D", "\\1", rownames(x))), b = x[, 1], se = x[, 2], p = x[, 4]),
       mediaan = B1[, .(plannen = .N, mediaan_dagen = as.numeric(median(dagen))), by = .(D, cohort)][order(D, cohort)][, maat := naam][])
}
U <- list(duur("ontwerp_comb", "kennisgeving, anders API"), duur("ontwerp_kg", "alleen kennisgeving"), duur("ontwerp_api", "alleen API (zoals 29)"))
R$duur <- rbindlist(lapply(U, `[[`, "did"))
R$duur_jaar <- rbindlist(lapply(U, `[[`, "es"))[, pct := 100 * (exp(b) - 1)]
R$duur_mediaan <- rbindlist(lapply(U, `[[`, "mediaan"))
kop("Tijd van ontwerp tot vaststelling, nabij tegenover ver na de PAS-uitspraak (log dagen)")
print(ROND(R$duur))
cat("\nPer jaar van het ontwerp tegenover 2018 (procent):\n"); print(dcast(ROND(R$duur_jaar, 1), jaar ~ maat, value.var = "pct"))
cat("\nMediaan dagen per cohort:\n"); print(R$duur_mediaan)

## ---- placebo: de indeling naar vaststellingsdatum op nepdatums ---------------
## Met het jaar van het ontwerp als vast effect (zoals 29_plannen_natuur.R tot versie 3.6) vergelijkt de schatting binnen een
## ontwerpjaar de plannen die na de datum werden vastgesteld met die ervoor, dus de lange met de korte procedures; spreiden de duren
## nabij meer, dan geeft dat op elke datum een positief verschil. Met het jaar van vaststelling als vast effect niet.
bp <- ds[!is.na(ontwerp_comb)]
bp[, `:=`(dagen = as.integer(vaststelling - ontwerp_comb), jaar_o = year(ontwerp_comb), jaar = year(vaststelling))]
bp <- bp[dagen > 0 & jaar %between% c(2014L, 2023L)]
bp[, log_d := log(dagen)]
R$spreiding <- bp[, .(plannen = .N, sd_log_dagen = sd(log_d), p90_p10 = as.numeric(quantile(dagen, 0.9) / quantile(dagen, 0.1))), by = D][order(D)]
R$placebo_vaststelling <- rbindlist(lapply(c("2015-05-29", "2016-05-29", "2017-05-29", "2018-05-29", "2019-05-29", "2020-05-29",
                                             "2021-05-29", "2022-05-29"), function(d) {
  x <- copy(bp)[, na_x := D * as.integer(vaststelling >= as.IDate(d))]
  m1 <- feols(log_d ~ na_x | gemeente + jaar_o + D, data = x, cluster = ~gemeente, notes = FALSE)
  m2 <- feols(log_d ~ na_x | gemeente + jaar + D, data = x, cluster = ~gemeente, notes = FALSE)
  data.table(datum = d, b_ontwerpjaar = coef(m1)[["na_x"]], p_ontwerpjaar = pvalue(m1)[["na_x"]],
             b_vaststellingsjaar = coef(m2)[["na_x"]], p_vaststellingsjaar = pvalue(m2)[["na_x"]])
}))
kop("Placebo: indeling naar vaststellingsdatum op nepdatums, met het jaar van ontwerp of van vaststelling als vast effect")
print(ROND(R$spreiding)); print(ROND(R$placebo_vaststelling))

## ---- ontwerpen die nooit werden vastgesteld --------------------------------
## het heelal: dossiers in de oogst met een ontwerpversie of een kennisgeving van een ontwerp, gemeentelijk bestemmingsplan
alle <- unique(p[type == "bestemmingsplan", .(dossier, gemeente = overheidscode)])
afst <- p[type == "bestemmingsplan" & !is.na(afstand_m)][order(dossier, datum)][!duplicated(dossier), .(dossier, afstand_m, opp_ha)]
o1 <- merge(alle, afst, by = "dossier")
o1[ont, on = "dossier", ontwerp_api := i.ontwerp_api]
o1[kg, on = "dossier", ontwerp_kg := i.ontwerp_kg]
o1[, ontwerp := pmin(ontwerp_api, ontwerp_kg, na.rm = TRUE)]
o1 <- o1[!is.na(ontwerp)]
o1[, `:=`(vastgesteld = dossier %chin% v$dossier, D = as.integer(afstand_m <= 2500), na = as.integer(ontwerp >= PAS), jaar_o = year(ontwerp))]
O <- o1[jaar_o %between% c(2014L, 2021L)]
O[, gestrand := 100 * (!vastgesteld)]
mo <- feols(gestrand ~ I(D * na) | gemeente + jaar_o + D, data = O, cluster = ~gemeente, notes = FALSE)
R$gestrand <- cbind(ct(mo, "I(D * na)", "ontwerp nooit vastgesteld, procentpunt"),
                    O[, .(pct_nabij_voor = pct(!vastgesteld[D == 1L & na == 0L]), pct_nabij_na = pct(!vastgesteld[D == 1L & na == 1L]),
                          pct_ver_voor = pct(!vastgesteld[D == 0L & na == 0L]), pct_ver_na = pct(!vastgesteld[D == 0L & na == 1L]), ontwerpen = .N)])
kop("Ontwerpen van 2014 tot en met 2021 die nooit werden vastgesteld (tot september 2026)")
print(ROND(R$gestrand, 2))

R$meta <- list(bron = "API Ruimtelijke plannen v4 (oogst 28 september 2026) en de kennisgevingen van ruimtelijke plannen (SRU, 2013 tot en met 27 september 2026)",
               pas = format(PAS), nabij_m = 2500, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("procedure_natuur_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("procedure_natuur_%s.json", EXTRACT)))
sink()
