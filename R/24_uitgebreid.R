## =====================================================================
## 24_uitgebreid.R -- waarover gingen de uitgebreide aanvragen die nabij
##                    natuur na de PAS-uitspraak wegvielen?
##
## Input : analyse/zaken.csv in de datamap van de bekendmakingen
##         (VERGUNNINGEN_DATA), _derived/teksten_vlaggen.csv
##         (24a_teksten.py teksten: vlaggen per zaak uit de volledige
##         tekst van aanvraag en ontwerpbesluit)
## Output: _output/uitgebreid_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 9: het aantal aanvragen met een uitgebreide procedure daalde
## nabij natuur na de uitspraak met 29 procent, als enige soort aanvraag.
## Onder de Wabo gold die procedure onder meer bij afwijken van het
## bestemmingsplan, bij een aangehaakte natuurtoets (met een verklaring van
## geen bedenkingen van de provincie) en in sommige gemeenten bij
## brandveilig gebruik. Als de daling een stikstofrem is, zit ze in de
## zaken over natuur; zit ze in brandveilig gebruik of afwijken, dan niet.
## Zelfde paneel en schatter als 09_aanvragen.R: vast paneel A (minstens
## 40 aanvragen in elk jaar 2016 tot en met 2020), 2016Q1 tot en met
## 2021Q2, Poisson met gemeente x nabij, gemeente x kwartaal en nabij x
## seizoen, standaardfouten geclusterd op gemeente.
## Alleen geldig als de oogst van prioriteit 1 (teksten.py) compleet is:
## een deel op zaaknummer is geen steekproef.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("uitgebreid_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
KW_START <- 2012L
kw_index <- function(datum) (as.integer(format(datum, "%Y")) - KW_START) * 4L + (as.integer(format(datum, "%m")) - 1L) %/% 3L + 1L
kw_van_label <- function(k) (as.integer(substr(k, 1, 4)) - KW_START) * 4L + as.integer(substr(k, 6, 6))
Q_PAS <- kw_index(as.Date("2019-05-29")); Q_BVR <- kw_index(as.Date("2021-07-01"))
Q_A0 <- kw_index(as.Date("2016-01-01")); Q_EIND <- kw_index(as.Date("2026-06-30"))

z <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
           select = c("zaak_id", "gemeente", "kwartaal", "dist_m", "uitgebreid", "verleend", "geweigerd", "soort", "woningen"))
z[, `:=`(t = kw_van_label(kwartaal), jaar = as.integer(substr(kwartaal, 1, 4)))]
z <- z[!is.na(dist_m) & t <= Q_EIND]
z[, D := as.integer(dist_m <= 2500)]
per_jaar <- z[, .N, by = .(gemeente, jaar)]
GEM_A <- sort(per_jaar[jaar %in% 2016:2020 & N >= 40, .N, by = gemeente][N == 5L, gemeente])
actief <- z[, .(t_eerst = min(t), t_laatst = max(t)), by = gemeente]

f_vl <- file.path(PATHS$derived, "teksten_vlaggen.csv")
if (!file.exists(f_vl)) stop("Ontbreekt: ", f_vl, "\nDraai eerst: py -3.13 R/24a_teksten.py teksten ", f_vl)
vl <- fread(f_vl, sep = ";")
setnames(vl, "woningen", "woningen_tekst")
u <- merge(z[uitgebreid == 1L & gemeente %in% GEM_A & t >= Q_A0 & t < Q_BVR], vl, by = "zaak_id", all.x = TRUE)
u[, met_tekst := !is.na(teksten)]
for (v in c("natura", "soorten", "vvgb_prov", "vvgb_raad", "afwijken", "brandveilig", "milieu")) u[is.na(get(v)), (v) := 0L]
## woningbouw: een titel van het soort nieuwbouw van woningen (09a_titels.py) of een woningaantal in de tekst
u[, woningbouw := soort %in% "nieuwbouw_woning" | (!is.na(woningen_tekst) & woningen_tekst > 0) | (!is.na(woningen) & woningen > 0)]
u[, soort := fcase(natura == 1L | soorten == 1L | vvgb_prov == 1L, "natuur",
                   brandveilig == 1L, "brandveilig gebruik",
                   afwijken == 1L | vvgb_raad == 1L, "afwijken van het plan",
                   woningbouw, "woningbouw",
                   milieu == 1L, "milieu",
                   met_tekst, "overig met tekst",
                   default = "zonder tekst")]
u[, na := t >= Q_PAS]

kop("Dekking en samenstelling, vast paneel A, 2016Q1 tot en met 2021Q2")
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), gemeenten = length(GEM_A),
               zaken = nrow(u), met_tekst = u[, mean(met_tekst)])
R$dekking <- u[, .(zaken = .N, met_tekst = mean(met_tekst)), by = .(nabij = D, na)][order(nabij, na)]
R$samenstelling <- u[, .N, by = .(nabij = D, na, soort)][, aandeel := N / sum(N), by = .(nabij, na)][order(nabij, na, soort)]
print(R$meta); print(R$dekking); print(R$samenstelling)

kop("Nabij tegenover ver na de uitspraak, per soort uitgebreide zaak")
paneel <- function(dt) {
  cel <- dt[, .(y = .N), by = .(gemeente, D, t)]
  vol <- CJ(gemeente = GEM_A, D = 0:1, t = Q_A0:(Q_BVR - 1L))
  vol <- merge(vol, actief, by = "gemeente")[t >= t_eerst & t <= t_laatst][, c("t_eerst", "t_laatst") := NULL]
  p <- merge(vol, cel, by = c("gemeente", "D", "t"), all.x = TRUE); p[is.na(y), y := 0L]
  p[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L), na_pas = D * as.integer(t >= Q_PAS))]
  p[]
}
did <- function(dt) {
  m <- tryCatch(fepois(y ~ na_pas | gem_D + gem_t + D_szn, data = paneel(dt), cluster = ~gemeente, notes = FALSE, glm.iter = 200),
                error = function(e) NULL)
  if (is.null(m)) return(data.table(b = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_, zaken = nrow(dt)))
  ct <- coeftable(m)
  data.table(b = ct["na_pas", 1], se = ct["na_pas", 2], p = ct["na_pas", 4], n = nobs(m), zaken = nrow(dt))
}
SOORTEN <- c("alle", "natuur", "brandveilig gebruik", "afwijken van het plan", "woningbouw", "milieu", "overig met tekst", "zonder tekst")
R$did <- rbindlist(lapply(SOORTEN, function(s) cbind(soort = s, did(if (s == "alle") u else u[soort == s]))))
R$did[, pct := 100 * (exp(b) - 1)]
## overlappende groepen: elke vlag op zichzelf, ongeacht de volgorde hierboven
VLAG <- list("woningbouw (titel of woningaantal)" = quote(woningbouw == TRUE), "afwijken van het plan (vlag)" = quote(afwijken == 1L | vvgb_raad == 1L),
             "milieu (vlag)" = quote(milieu == 1L), "brandveilig gebruik (vlag)" = quote(brandveilig == 1L),
             "verklaring van geen bedenkingen van de provincie" = quote(vvgb_prov == 1L),
             "geen van de vlaggen, met tekst" = quote(met_tekst & natura == 0L & soorten == 0L & vvgb_prov == 0L & brandveilig == 0L &
                                                         afwijken == 0L & vvgb_raad == 0L & milieu == 0L & !woningbouw))
R$vlaggen <- rbindlist(lapply(names(VLAG), function(s) cbind(groep = s, did(u[eval(VLAG[[s]])]))))
R$vlaggen[, pct := 100 * (exp(b) - 1)]
print(R$did[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
kop("Overlappende groepen")
print(R$vlaggen[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

## ---------------------------------------------------------------------
## De woningbouwgroep nader: aantallen, gemeenten, trendbreuk en zonder de
## gemeenten met de meeste zaken
## ---------------------------------------------------------------------
kop("Woningbouw met een uitgebreide procedure: aantallen, trendbreuk en robuustheid")
w <- u[woningbouw == TRUE]
R$woningbouw_tel <- w[, .(zaken = .N, gemeenten = uniqueN(gemeente), kwartalen = uniqueN(t)), by = .(nabij = D, na)][order(nabij, na)]
R$woningbouw_tel[, per_kwartaal := zaken / fifelse(na, Q_BVR - Q_PAS, Q_PAS - Q_A0)]
print(R$woningbouw_tel)
top <- w[D == 1L, .N, by = gemeente][order(-N)]
R$woningbouw_top <- head(top, 10)
print(R$woningbouw_top)
tb <- function(dt) {
  p <- paneel(dt)
  p[, `:=`(tj = (t - Q_PAS) / 4, post = as.integer(t >= Q_PAS))]
  p[, `:=`(voortrend = D * tj, sprong = D * post, natrend = D * post * tj)]
  m <- tryCatch(fepois(y ~ voortrend + sprong + natrend | gem_D + gem_t + D_szn, data = p, cluster = ~gemeente, notes = FALSE, glm.iter = 200),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  data.table(term = names(coef(m)), b = coef(m), se = se(m), p = pvalue(m))
}
R$woningbouw_trendbreuk <- tb(w)
print(R$woningbouw_trendbreuk)
R$woningbouw_zonder <- rbindlist(lapply(1:3, function(k) cbind(zonder = paste(head(top$gemeente, k), collapse = ", "), did(w[!gemeente %in% head(top$gemeente, k)]))))
R$woningbouw_zonder[, pct := 100 * (exp(b) - 1)]
print(R$woningbouw_zonder)
## gemeente x periode (voor en na) in plaats van gemeente x kwartaal: minder
## lege cellen; de ruwe verhouding over alle gemeenten; welke gemeenten de
## schatting dragen (nabij en ver, voor en na allebei zaken); en de woning-
## bouwgroep telkens zonder een van die gemeenten
u[, post := as.integer(t >= Q_PAS)]
GROEP <- list("alle" = quote(TRUE), "woningbouw" = quote(woningbouw == TRUE), "milieu" = quote(milieu == 1L),
              "brandveilig gebruik" = quote(brandveilig == 1L), "afwijken van het plan" = quote(afwijken == 1L | vvgb_raad == 1L))
gp_paneel <- function(x) {
  cel <- x[, .(y = .N), by = .(gemeente, D, post)]
  p <- merge(CJ(gemeente = unique(x$gemeente), D = 0:1, post = 0:1), cel, by = c("gemeente", "D", "post"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(gD = paste(gemeente, D), gP = paste(gemeente, post), tr = D * post)]
  p[]
}
gp_did <- function(p) { m <- fepois(y ~ tr | gD + gP, data = p, cluster = ~gemeente, notes = FALSE)
  data.table(b = coef(m)[["tr"]], se = se(m)[["tr"]], p = pvalue(m)[["tr"]]) }
dragend <- function(p) p[, .(ok = all(tapply(y, D, sum) > 0) & all(tapply(y, post, sum) > 0)), by = gemeente][ok == TRUE, gemeente]
R$periode <- rbindlist(lapply(names(GROEP), function(g) {
  x <- u[eval(GROEP[[g]])]
  r <- x[, .N, by = .(D, post)]
  ruw <- (r[D == 1 & post == 1, N] / (Q_BVR - Q_PAS)) / (r[D == 1 & post == 0, N] / (Q_PAS - Q_A0)) /
         ((r[D == 0 & post == 1, N] / (Q_BVR - Q_PAS)) / (r[D == 0 & post == 0, N] / (Q_PAS - Q_A0)))
  p <- gp_paneel(x)
  cbind(groep = g, zaken = nrow(x), ruw_pct = 100 * (ruw - 1), gp_did(p), gemeenten_dragend = length(dragend(p)))
}))
R$periode[, `:=`(pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1))]
print(R$periode[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
pw <- gp_paneel(u[woningbouw == TRUE]); dw <- dragend(pw)
R$woningbouw_dragend <- pw[gemeente %in% dw, .(zaken = sum(y)), by = .(nabij = D, post)][order(nabij, post)]
R$woningbouw_weglaten <- rbindlist(lapply(dw, function(g) cbind(zonder = g, gp_did(pw[gemeente != g]))))[, pct := 100 * (exp(b) - 1)]
R$woningbouw_weglaten_bereik <- R$woningbouw_weglaten[, .(min_pct = min(pct), max_pct = max(pct), max_p = max(p))]
print(R$woningbouw_dragend); print(R$woningbouw_weglaten_bereik)

## ter vergelijking: dezelfde woningbouwgroep met een reguliere procedure (uitgebreid = 0), in hetzelfde panel en venster
zr <- merge(z[uitgebreid == 0L & gemeente %in% GEM_A & t >= Q_A0 & t < Q_BVR & soort %in% "nieuwbouw_woning"], vl[, .(zaak_id)], by = "zaak_id")
R$woningbouw_regulier_met_tekst <- nrow(zr)
R$woningbouw_regulier <- cbind(groep = "nieuwbouw van woningen, reguliere procedure (titel)",
                               did(z[uitgebreid == 0L & gemeente %in% GEM_A & t >= Q_A0 & t < Q_BVR & soort %in% "nieuwbouw_woning"]))
R$woningbouw_regulier[, pct := 100 * (exp(b) - 1)]
print(R$woningbouw_regulier)

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("uitgebreid_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
