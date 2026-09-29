## =====================================================================
## 34_tekstvlaggen.R -- noemen aanvragen voor nieuwe panden nabij natuur
##                      vaker de natuurtoets of een afwijking van het plan?
##
## Input : analyse/zaken.csv in de datamap van de bekendmakingen
##         (VERGUNNINGEN_DATA); _derived/aanvragen_titels.csv (09a_titels.py,
##         de projectsoort); _derived/teksten_vlaggen.csv (24a_teksten.py
##         teksten: vlaggen per zaak uit de volledige tekst)
## Output: _output/tekstvlaggen_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 9 en 13 vonden in het aantal aanvragen nabij natuur geen
## eenduidige rem. Een rem kan ook in extra werk zitten: een aanvraag die
## een natuurtoets meeneemt (Natura 2000-activiteit, een aangehaakte
## natuurvergunning, stikstofberekening) of die van het plan afwijkt. De
## tekstoogst (vergunningen/pipeline/teksten.py, prioriteit 2 en 3) heeft
## de volledige tekst van de aanvragen voor nieuwe woningen zonder aantal in
## de titel en van de aanvragen met een gekoppeld nieuw BAG-pand. Hier het
## aandeel dat natuur, stikstof, soortenbescherming of afwijken noemt,
## nabij tegenover ver, voor en na de PAS-uitspraak (vast paneel A,
## 2016Q1 tot en met 2021Q2, zoals 09_aanvragen.R) en rond de
## Omgevingswet en intern salderen (alle gemeenten, 2021Q3 tot en met
## 2026Q2). Lineair in procentpunten met gemeente x nabij en gemeente x
## kwartaal als vaste effecten: een tekst bevat ook de standaardzinnen van
## een gemeente, dus alleen binnen de gemeente vergelijken.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
sink(file.path(PATHS$output, sprintf("tekstvlaggen_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
pct <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)
ROND <- function(x, d = 3) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]
ct <- function(m, termen, naam) {
  x <- coeftable(m)
  data.table(groep = naam, term = termen, b = x[termen, 1], se = x[termen, 2], p = x[termen, 4], n = nobs(m))
}
KW_START <- 2012L
kw_index <- function(datum) (as.integer(format(datum, "%Y")) - KW_START) * 4L + (as.integer(format(datum, "%m")) - 1L) %/% 3L + 1L
kw_van_label <- function(k) (as.integer(substr(k, 1, 4)) - KW_START) * 4L + as.integer(substr(k, 6, 6))
Q_PAS <- kw_index(as.Date("2019-05-29")); Q_BVR <- kw_index(as.Date("2021-07-01")); Q_OW <- kw_index(as.Date("2024-01-01"))
Q_SAL <- kw_index(as.Date("2024-12-18")); Q_A0 <- kw_index(as.Date("2016-01-01")); Q_EIND <- kw_index(as.Date("2026-06-30"))

z <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
           select = c("zaak_id", "gemeente", "kwartaal", "dist_m", "uitgebreid"))
z <- merge(z, fread(file.path(PATHS$derived, "aanvragen_titels.csv"), select = c("zaak_id", "soort")), by = "zaak_id", all.x = TRUE)
z[, `:=`(t = kw_van_label(kwartaal), jaar = as.integer(substr(kwartaal, 1, 4)))]
z <- z[!is.na(dist_m) & t <= Q_EIND]
z[, D := as.integer(dist_m <= 2500)]
per_jaar <- z[, .N, by = .(gemeente, jaar)]
GEM_A <- sort(per_jaar[jaar %in% 2016:2020 & N >= 40, .N, by = gemeente][N == 5L, gemeente])
vl <- fread(file.path(PATHS$derived, "teksten_vlaggen.csv"), sep = ";")
u <- merge(z[uitgebreid == 0L], vl, by = "zaak_id")
VLAGGEN <- c("natura", "stikstof", "soorten", "afwijken", "vvgb_prov")
for (v in VLAGGEN) u[, (v) := 100 * get(v)]
u[, groep := fifelse(soort %in% "nieuwbouw_woning", "nieuwbouw van woningen", "overige aanvragen met een nieuw pand")]
kop("Aanvragen met een reguliere procedure en een volledige tekst: %s zaken in %s gemeenten (%s nieuwbouw van woningen)",
    nfmt(nrow(u)), nfmt(uniqueN(u$gemeente)), nfmt(u[groep == "nieuwbouw van woningen", .N]))
R$per_jaar <- u[jaar >= 2014, c(list(zaken = .N), lapply(.SD, function(x) mean(x))), by = .(jaar, D), .SDcols = VLAGGEN][order(jaar, D)]
print(ROND(R$per_jaar, 2))

## ---- A. PAS-uitspraak, vast paneel A --------------------------------------
a <- u[gemeente %in% GEM_A & t >= Q_A0 & t < Q_BVR]
a[, na := D * as.integer(t >= Q_PAS)]
did_a <- list()
for (v in VLAGGEN) for (g in c("alle", unique(a$groep))) {
  d <- if (g == "alle") a else a[groep == g]
  m <- tryCatch(feols(as.formula(sprintf("%s ~ na | gemeente^D + gemeente^t", v)), data = d, cluster = ~gemeente, notes = FALSE), error = function(e) NULL)
  if (!is.null(m)) did_a[[paste(v, g)]] <- cbind(ct(m, "na", g), vlag = v,
                                                   nabij_voor = d[D == 1 & t < Q_PAS, mean(get(v))], nabij_na = d[D == 1 & t >= Q_PAS, mean(get(v))],
                                                   ver_voor = d[D == 0 & t < Q_PAS, mean(get(v))], ver_na = d[D == 0 & t >= Q_PAS, mean(get(v))])
}
R$pas <- rbindlist(did_a)
kop("A. Na de PAS-uitspraak, vast paneel A (%s gemeenten, %s zaken), procentpunt nabij tegenover ver", length(GEM_A), nfmt(nrow(a)))
print(ROND(R$pas, 2))

## ---- B. Omgevingswet en intern salderen, alle gemeenten ----------------------
b <- u[t >= Q_BVR & t <= Q_EIND]
b[, `:=`(ow = D * as.integer(t >= Q_OW), sal = D * as.integer(t >= Q_SAL))]
did_b <- list()
for (v in VLAGGEN) for (g in c("alle", unique(b$groep))) {
  d <- if (g == "alle") b else b[groep == g]
  m <- tryCatch(feols(as.formula(sprintf("%s ~ ow + sal | gemeente^D + gemeente^t", v)), data = d, cluster = ~gemeente, notes = FALSE), error = function(e) NULL)
  if (!is.null(m)) did_b[[paste(v, g)]] <- cbind(ct(m, c("ow", "sal"), g), vlag = v)
}
R$salderen <- rbindlist(did_b)
kop("B. Omgevingswet (2024Q1) en intern salderen (2024Q4), alle gemeenten 2021Q3 tot en met 2026Q2 (%s zaken), procentpunt, stappen incrementeel", nfmt(nrow(b)))
print(ROND(R$salderen, 2))
R$salderen_ruw <- b[, c(list(zaken = .N), lapply(.SD, function(x) mean(x))), by = .(periode = fcase(t >= Q_SAL, "na intern salderen", t >= Q_OW, "Omgevingswet",
                                                                                                  default = "2021Q3-2023Q4"), D), .SDcols = VLAGGEN][order(periode, D)]
print(ROND(R$salderen_ruw, 2))

## ---- C. afwijken bij nieuwbouw van woningen: trend of sprong? ----------------
## Nabij natuur ligt vaker buitengebied, waar een woning vaker buiten het plan valt; sinds de Omgevingswet moet een buitenplanse
## omgevingsplanactiviteit in de kennisgeving staan. Een verschil dat geleidelijk groeit terwijl gemeenten de nieuwe terminologie
## overnemen, is dan geen effect van de uitspraak. Event study per halfjaar (tegenover 2024H1), trendbreuk bij 2024Q4 binnen de
## Omgevingswet, na vanaf 2025Q1, en weglaten per gemeente.
hj <- function(t) sprintf("%dH%d", KW_START + (t - 1L) %/% 4L, ((t - 1L) %% 4L) %/% 2L + 1L)
nb <- b[groep == "nieuwbouw van woningen"]
nb[, half := hj(t)]
es <- feols(afwijken ~ i(half, D, ref = "2024H1") | gemeente^D + gemeente^t, data = nb, cluster = ~gemeente, notes = FALSE)
x <- coeftable(es)
R$afwijken_es <- data.table(half = sub("^half::([0-9H]+):D$", "\\1", rownames(x)), b = x[, 1], se = x[, 2], p = x[, 4])
ow_t <- nb[t >= Q_OW]
ow_t[, `:=`(tj = (t - Q_SAL) / 4, post = as.integer(t >= Q_SAL))]
ow_t[, `:=`(voortrend = D * tj, sprong = D * post, natrend = D * post * tj)]
mt <- feols(afwijken ~ voortrend + sprong + natrend | gemeente^D + gemeente^t, data = ow_t, cluster = ~gemeente, notes = FALSE)
R$afwijken_trendbreuk <- ct(mt, c("voortrend", "sprong", "natrend"), "nieuwbouw van woningen, 2024Q1 tot en met 2026Q2")
nb[, sal25 := D * as.integer(t >= kw_index(as.Date("2025-01-01")))]
m25 <- feols(afwijken ~ ow + sal25 | gemeente^D + gemeente^t, data = nb, cluster = ~gemeente, notes = FALSE)
R$afwijken_2025 <- ct(m25, c("ow", "sal25"), "nieuwbouw van woningen, na vanaf 2025Q1")
## gemeenten die het verschil dragen: met aanvragen nabij en ver, voor en na intern salderen, binnen de Omgevingswet
dr <- nb[t >= Q_OW, .(n = .N), by = .(gemeente, D, post = t >= Q_SAL)][, .N, by = gemeente][N == 4L, gemeente]
m0 <- feols(afwijken ~ sal | gemeente^D + gemeente^t, data = nb[t >= Q_OW], cluster = ~gemeente, notes = FALSE)
R$afwijken_binnen_ow <- cbind(ct(m0, "sal", "nieuwbouw van woningen, alleen 2024Q1 tot en met 2026Q2"), gemeenten_dragend = length(dr))
wl <- rbindlist(lapply(dr, function(g) {
  m <- feols(afwijken ~ sal | gemeente^D + gemeente^t, data = nb[t >= Q_OW & gemeente != g], cluster = ~gemeente, notes = FALSE)
  data.table(zonder = g, b = coef(m)[["sal"]], p = pvalue(m)[["sal"]])
}))
R$afwijken_weglaten <- wl[, .(min_b = min(b), max_b = max(b), max_p = max(p), gemeenten = .N)]
R$afwijken_ruw <- nb[, .(zaken = .N, afwijken = mean(afwijken)), by = .(half, D)][order(half, D)]
kop("C. Afwijken bij nieuwbouw van woningen: event study, trendbreuk en weglaten")
print(dcast(ROND(R$afwijken_ruw, 1), half ~ D, value.var = c("zaken", "afwijken")))
print(ROND(R$afwijken_es, 2)); print(ROND(R$afwijken_trendbreuk, 2)); print(ROND(R$afwijken_2025, 2))
print(ROND(R$afwijken_binnen_ow, 2)); print(ROND(R$afwijken_weglaten, 2))

## ---- D. afwijken naar contour en buurtdichtheid -------------------------------
## Nabij natuur ligt vaker buiten de stad. Is de extra vermelding van afwijken nabij natuur een verschil tussen stad en land, of
## schoven de aanvragen nabij natuur naar buiten de contour? Binnen of buiten de huidige verstedelijkingscontour op de plek van de
## zaak (34a_contour_zaken.py; in Gelderland de bebouwde kom als vervanger) en de dichtheid van de buurt (OAD van de CBS-buurt van
## 2012 in drie klassen, zoals 21_verdichting.R en 26_samenstelling.R: de mediaan van de VBO's in de cel van 100, 500 of 2000 meter
## rond de zaak). Alleen zaken met een precieze plek, dus niet met een woonplaats, buurt, wijk of gemeente als locatie.
cz <- fread(file.path(PATHS$derived, "contour_zaken.csv"), sep = ";")
setnames(cz, c("x", "y"), c("x_rd", "y_rd"))
nb[cz, on = "zaak_id", `:=`(x_rd = i.x_rd, y_rd = i.y_rd, lt = i.locatietype, binnen = i.binnen_contour, gld = i.contourbron == 125L)]
nb[lt %in% c("Woonplaats", "Buurt", "Wijk", "Gemeente", "GeometrieRef"), binnen := NA_integer_]
klasse <- function(oad) fifelse(is.na(oad), NA_character_, fifelse(oad >= 1500, "hoog", fifelse(oad >= 500, "midden", "laag")))
vbo <- readRDS(file.path(PATHS$derived, "econlogic_vbo.rds"))[!is.na(oad) & !is.na(x), .(x_rd = x, y_rd = y, oad)]
sleutel <- function(x, y, m) (x %/% m) * 1e5 + (y %/% m)
for (m in c(100, 500, 2000)) {
  cel <- vbo[, .(oad_cel = as.numeric(median(oad))), by = .(k = sleutel(x_rd, y_rd, m))]
  nb[, k := sleutel(x_rd, y_rd, m)]
  nb[cel, on = "k", (paste0("oad_", m)) := i.oad_cel]
}
nb[, dichtheid := fifelse(is.na(binnen), NA_character_, klasse(fcoalesce(oad_100, oad_500, oad_2000)))]
nb[, c("k", "oad_100", "oad_500", "oad_2000") := NULL]
rm(vbo, cel); invisible(gc())
nb[, `:=`(buiten = 100 * (1L - binnen), periode = fcase(t >= Q_SAL, "3 na intern salderen", t >= Q_OW, "2 Omgevingswet", default = "1 2021Q3-2023Q4"))]
R$contour_dekking <- nb[, .(zaken = .N, met_contour = sum(!is.na(binnen)), met_dichtheid = sum(!is.na(dichtheid)), gelderland = sum(gld %in% TRUE))]
R$contour_ruw <- nb[!is.na(binnen), .(zaken = .N, afwijken = mean(afwijken)), by = .(periode, D, binnen)][order(periode, binnen, D)]
R$dichtheid_ruw <- nb[!is.na(dichtheid), .(zaken = .N, afwijken = mean(afwijken)), by = .(periode, D, dichtheid)][order(periode, dichtheid, D)]
R$samenstelling_ruw <- nb[!is.na(binnen), .(zaken = .N, buiten = mean(buiten), laag = 100 * mean(dichtheid %in% "laag")), by = .(periode, D)][order(periode, D)]
kop("D. Afwijken bij nieuwbouw van woningen naar contour en buurtdichtheid (%s zaken, %s met een precieze plek)",
    nfmt(R$contour_dekking$zaken), nfmt(R$contour_dekking$met_contour))
print(R$contour_dekking); print(ROND(R$samenstelling_ruw, 1)); print(ROND(R$contour_ruw, 1)); print(ROND(R$dichtheid_ruw, 1))
## samenstelling: lagen de aanvragen nabij natuur na intern salderen vaker buiten de contour of in een dunne buurt?
nc <- nb[!is.na(binnen)]
nc[, laag := 100 * (dichtheid %in% "laag")]
did_d <- list()
for (v in c("buiten", "laag")) {
  m <- feols(as.formula(sprintf("%s ~ ow + sal | gemeente^D + gemeente^t", v)), data = nc, cluster = ~gemeente, notes = FALSE)
  did_d[[v]] <- cbind(ct(m, c("ow", "sal"), "samenstelling, 2021Q3 tot en met 2026Q2"), uitkomst = v)
}
## afwijken per contour en dichtheid, en binnen gemeente x contour (of dichtheid) x kwartaal
nc[, `:=`(ow_b = ow * (binnen == 0L), sal_b = sal * (binnen == 0L))]
nc[, `:=`(ow_l = ow * (dichtheid %in% "laag"), sal_l = sal * (dichtheid %in% "laag"))]
spec <- list(
  list("alleen binnen de contour", nc[binnen == 1L], "afwijken ~ ow + sal | gemeente^D + gemeente^t", c("ow", "sal")),
  list("alleen buiten de contour", nc[binnen == 0L], "afwijken ~ ow + sal | gemeente^D + gemeente^t", c("ow", "sal")),
  list("binnen gemeente, contour en kwartaal", nc, "afwijken ~ ow + sal | gemeente^D^binnen + gemeente^t^binnen", c("ow", "sal")),
  list("idem, verschil buiten tegenover binnen", nc, "afwijken ~ ow + sal + ow_b + sal_b | gemeente^D^binnen + gemeente^t^binnen", c("ow", "sal", "ow_b", "sal_b")),
  list("idem, zonder Gelderland", nc[gld %in% FALSE], "afwijken ~ ow + sal | gemeente^D^binnen + gemeente^t^binnen", c("ow", "sal")),
  list("alleen buurten met lage dichtheid", nc[dichtheid %in% "laag"], "afwijken ~ ow + sal | gemeente^D + gemeente^t", c("ow", "sal")),
  list("alleen buurten met middelhoge of hoge dichtheid", nc[dichtheid %in% c("midden", "hoog")], "afwijken ~ ow + sal | gemeente^D + gemeente^t", c("ow", "sal")),
  list("binnen gemeente, dichtheid en kwartaal", nc[!is.na(dichtheid)], "afwijken ~ ow + sal | gemeente^D^dichtheid + gemeente^t^dichtheid", c("ow", "sal")),
  list("idem, verschil lage tegenover hogere dichtheid", nc[!is.na(dichtheid)],
       "afwijken ~ ow + sal + ow_l + sal_l | gemeente^D^dichtheid + gemeente^t^dichtheid", c("ow", "sal", "ow_l", "sal_l")),
  list("binnen gemeente, contour, dichtheid en kwartaal", nc[!is.na(dichtheid)],
       "afwijken ~ ow + sal | gemeente^D^binnen^dichtheid + gemeente^t^binnen^dichtheid", c("ow", "sal")),
  list("binnen gemeente, contour en kwartaal, alleen 2024Q1 tot en met 2026Q2", nc[t >= Q_OW], "afwijken ~ sal | gemeente^D^binnen + gemeente^t^binnen", "sal"),
  list("idem, alleen buiten de contour", nc[t >= Q_OW & binnen == 0L], "afwijken ~ sal | gemeente^D + gemeente^t", "sal"),
  list("idem, alleen binnen de contour", nc[t >= Q_OW & binnen == 1L], "afwijken ~ sal | gemeente^D + gemeente^t", "sal"))
for (s in spec) {
  m <- tryCatch(feols(as.formula(s[[3]]), data = s[[2]], cluster = ~gemeente, notes = FALSE), error = function(e) NULL)
  if (!is.null(m)) did_d[[s[[1]]]] <- cbind(ct(m, s[[4]], s[[1]]), uitkomst = "afwijken")
}
R$contour_did <- rbindlist(did_d)
print(ROND(R$contour_did, 2))
## event study per halfjaar (tegenover 2024H1) binnen gemeente, contour en kwartaal, en alleen binnen de contour
es_d <- list(list("binnen gemeente, contour en kwartaal", nc, "gemeente^D^binnen + gemeente^t^binnen"),
             list("alleen binnen de contour", nc[binnen == 1L], "gemeente^D + gemeente^t"),
             list("alleen buiten de contour", nc[binnen == 0L], "gemeente^D + gemeente^t"))
R$contour_es <- rbindlist(lapply(es_d, function(s) {
  m <- feols(as.formula(sprintf('afwijken ~ i(half, D, ref = "2024H1") | %s', s[[3]])), data = s[[2]], cluster = ~gemeente, notes = FALSE)
  x <- coeftable(m)
  data.table(groep = s[[1]], half = sub("^half::([0-9H]+):D$", "\\1", rownames(x)), b = x[, 1], se = x[, 2], p = x[, 4])
}))
print(dcast(ROND(R$contour_es, 2), half ~ groep, value.var = c("b", "p")))

R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), zaken = nrow(u), gemeenten_A = length(GEM_A))
writeLines(to_json(R), file.path(PATHS$output, sprintf("tekstvlaggen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
