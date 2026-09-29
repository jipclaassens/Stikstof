## =====================================================================
## 32_vergunningen_lv.R -- vergunningen voor een buitenplanse activiteit
##                         nabij natuur rond de uitspraken over intern
##                         salderen
##
## Input : _derived/vergunningen_lv.csv (32a_vergunningen_lv.py: de
##         omgevingsvergunningen die gemeenten sinds 2024 in de DSO-LV
##         registreren, vooral buitenplanse omgevingsplanactiviteiten, met
##         afstand tot het habitat en projectsoort uit de titel)
## Output: _output/vergunningen_lv_<extract>.json en .txt
##
## Waarom: hoofdstuk 13 vond na de uitspraken geen daling van planwijzigingen
## nabij natuur, met de kanttekening dat een project ook via een vergunning
## voor een buitenplanse activiteit kan gaan in plaats van via een
## planwijziging. Die vergunningen staan met hun gebied in de DSO-LV. Als de
## uitspraak projecten nabij natuur remde, zouden ook deze vergunningen daar
## minder worden; als projecten van planwijziging naar vergunning
## verschoven, juist meer.
##
## Model: Poisson per gemeente, kant en kwartaal, 2024Q1 tot en met 2026Q2,
## met gemeente x nabij, gemeente x kwartaal en nabij x seizoen; na = vanaf
## 2025Q1 (variant vanaf 2024Q4). Nabij = omhullende rechthoek van het
## vergunningsgebied binnen 2.500 meter van het habitat. Groepen: alle,
## nieuwbouw van woningen en klein verbouw (placebo, een dakkapel heeft geen
## natuurvergunning nodig). De registratie begon in 2024 (49 vergunningen in
## het eerste kwartaal), dus de voorperiode is kort en groeit; gemeente x
## kwartaal vangt het tempo waarin gemeenten gingen registreren.
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
sink(file.path(PATHS$output, sprintf("vergunningen_lv_%s.txt", EXTRACT)), split = TRUE)
R <- list()
ROND <- function(x, d = 3) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]
KW_START <- 2012L
kw <- function(d) (as.integer(format(d, "%Y")) - KW_START) * 4L + (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L
kw_lab <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
Q0 <- kw(as.IDate("2024-01-01")); Q_SAL <- kw(as.IDate("2024-12-18")); Q_NA <- kw(as.IDate("2025-01-01")); Q_EIND <- kw(as.IDate("2026-06-30"))
ct <- function(m, termen, naam) {
  x <- coeftable(m)
  data.table(groep = naam, term = termen, b = x[termen, 1], se = x[termen, 2], p = x[termen, 4], n = nobs(m))
}

v <- fread(file.path(PATHS$derived, "vergunningen_lv.csv"), sep = ";", encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "gemeente", "titel", "soort")))
v[, datum := as.IDate(substr(begin_inwerking, 1, 10))]
v <- v[!is.na(datum) & !is.na(afstand_m)]
v[, `:=`(t = kw(datum), D = as.integer(afstand_m <= 2500))]
v <- v[t >= Q0 & t <= Q_EIND]
kop("Omgevingsvergunningen in de DSO-LV, 2024Q1 tot en met 2026Q2: %s (%s nabij) in %s gemeenten",
    nfmt(nrow(v)), nfmt(v[, sum(D)]), nfmt(uniqueN(v$gemeente)))
R$per_kwartaal <- v[, .(vergunningen = .N, nabij = sum(D), nieuwbouw_woning = sum(soort == "nieuwbouw_woning"),
                        klein_verbouw = sum(soort == "klein_verbouw")), by = .(kwartaal = kw_lab(t))][order(kwartaal)]
print(R$per_kwartaal)

GROEP <- list(`alle` = quote(rep(TRUE, .N)), `nieuwbouw van woningen` = quote(soort == "nieuwbouw_woning"),
              `klein verbouw (placebo)` = quote(soort == "klein_verbouw"))
did <- list(); es <- list(); ruw <- list()
for (g in names(GROEP)) {
  e <- v[eval(GROEP[[g]])]
  cel <- e[, .(y = .N), by = .(gemeente, D, t)]
  pa <- merge(CJ(gemeente = unique(e$gemeente), D = 0:1, t = Q0:Q_EIND), cel, by = c("gemeente", "D", "t"), all.x = TRUE)
  pa[is.na(y), y := 0L]
  pa[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L),
            na = D * as.integer(t >= Q_NA), na4 = D * as.integer(t >= Q_SAL), kw = kw_lab(t))]
  m1 <- fepois(y ~ na | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
  m2 <- fepois(y ~ na4 | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
  did[[g]] <- rbind(cbind(ct(m1, "na", g), na_vanaf = "2025Q1"), cbind(ct(m2, "na4", g), na_vanaf = "2024Q4"))
  mk <- fepois(y ~ i(kw, D, ref = "2024Q4") | gem_D + gem_t, data = pa, cluster = ~gemeente, notes = FALSE)
  x <- coeftable(mk)
  es[[g]] <- data.table(groep = g, kwartaal = sub("^kw::([0-9Q]+):D$", "\\1", rownames(x)), b = x[, 1], se = x[, 2], p = x[, 4])
  ruw[[g]] <- pa[, .(gemeenten = uniqueN(gemeente), per_kwartaal = sum(y) / uniqueN(t), aandeel_nabij = sum(y * D) / sum(y)),
                 by = .(periode = fifelse(t >= Q_NA, "na", "voor"))][order(periode)][, groep := g]
}
R$did <- rbindlist(did)[, pct := 100 * (exp(b) - 1)]
R$es <- rbindlist(es)[, pct := 100 * (exp(b) - 1)]
R$ruw <- rbindlist(ruw)
kop("Nabij tegenover ver na de uitspraken over intern salderen (Poisson, gemeente x nabij, gemeente x kwartaal, nabij x seizoen)")
print(ROND(R$did)); print(ROND(R$ruw))
cat("\nPer kwartaal tegenover 2024Q4 (procent):\n"); print(dcast(ROND(R$es, 1), kwartaal ~ groep, value.var = "pct"))

R$meta <- list(bron = "DSO-LV, omgevingsvergunningen, oogst 28 september 2026", intern_salderen = "2024-12-18", na_vanaf = kw_lab(Q_NA),
               eind = kw_lab(Q_EIND), nabij_m = 2500, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("vergunningen_lv_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("vergunningen_lv_%s.json", EXTRACT)))
sink()
