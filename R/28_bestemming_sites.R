## =====================================================================
## 28_bestemming_sites.R -- de bestemming van de sites van het
##                          herontwikkelingspaper (issue 30, stap 3)
##
## Input : _derived/bestemming/ (28a_bestemming.py: per laag de vlakken
##         die een punt van de bestaande opstal raken, met datum en status);
##         _derived/bestemming_site_punt.rds en bestemming_punten.csv (dit
##         script maakt ze als ze ontbreken, uit econlogic_sites_obj.rds van
##         27_pijplijn_sites.R); C:/LocalData/Redevelopment/R_werk/ (sites_,
##         clusters_sn_ en stage1_sn_Nederland_20260710.rds);
##         bron/plannen/ruimtelijkeplannen_plannen_20260701.csv (oppervlakte
##         per plan)
## Output: _output/bestemming_sites_<extract>.json en .txt
##
## Het plan dat op een datum gold, per punt: het vastgestelde vlak van de
## enkelbestemming met de laatste plandatum op of voor die datum (ontwerpen
## tellen niet; voor de stand van nu ook geen vervallen of historische
## versies). Bouwaanduiding en maatvoering tellen alleen als ze bij datzelfde
## plan horen. De stand van 1 januari 2012 zou de bestemming voor het venster
## van het paper zijn, maar de download bevat vooral de huidige plannen: de
## plannen die in de actualiseringsgolf van 2012 en 2013 zijn vervangen,
## ontbreken meestal (dekking in 2012 14 tot 23 procent van de sites). B en D
## gebruiken daarom de stand van 31 december 2013, en voor de gerealiseerde
## projecten alleen opleveringen van 2018 tot en met 2026, zodat het plan
## minstens vier jaar voor de oplevering ligt.
##
## A. Dekking: sites met een bestemming in 2012 en nu.
## B. De gerealiseerde sloop-nieuwbouw (SN, oplevering 2018 tot en met
##    2026): de bestemming, het toegestane woningtype en de bouwhoogte van
##    eind 2013, tegenover de onveranderde sites; het gerealiseerde
##    alternatief K tegenover het woningtype van eind 2013 (dichter dan
##    toegestaan).
## C. Planwijziging rond het project: een klein plan (onder 10 ha) dat de
##    site raakt, per jaar voor en na de oplevering en per site in de vijf
##    jaar ervoor, tegenover de kans per jaar bij onveranderde sites.
## D. De bestemming van eind 2013 en herontwikkeling daarna: het aandeel SN
##    (oplevering 2018 tot en met 2026) per woningtype en hoofdgroep,
##    stedelijk, ruw en binnen de gemeente met de sitegrootte als controle.
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
sink(file.path(PATHS$output, sprintf("bestemming_sites_%s.txt", EXTRACT)), split = TRUE)

W <- Sys.getenv("ECONLOGIC_WERK", "C:/LocalData/Redevelopment/R_werk")
B <- file.path(PATHS$derived, "bestemming")
R <- list()
pct <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)
ROND <- function(x, d = 2) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]

## ---- 0. punten per site ---------------------------------------------
f_sp <- file.path(PATHS$derived, "bestemming_site_punt.rds")
if (!file.exists(f_sp)) {
  o <- readRDS(file.path(PATHS$derived, "econlogic_sites_obj.rds"))
  p <- o[is_incumbent == TRUE & !is.na(x), .(site_id, x, y)]
  u <- unique(p[, .(x, y)])[, pid := .I]
  fwrite(u[, .(pid, x, y)], file.path(PATHS$derived, "bestemming_punten.csv"), sep = ";")
  p[u, on = .(x, y), pid := i.pid]
  saveRDS(p[, .(site_id, pid)], f_sp)
  rm(o, p, u)
  stop("Punten geschreven; draai nu 28a_bestemming.py en daarna dit script opnieuw")
}
sp <- readRDS(f_sp)
kop("Punten van bestaande opstal: %s in %s sites", format(nrow(sp), big.mark = "."), format(uniqueN(sp$site_id), big.mark = "."))

## ---- 1. de lagen ------------------------------------------------------
lees <- function(laag) {
  v <- fread(file.path(B, paste0(laag, "_vlakken.csv")), sep = ";", encoding = "UTF-8",
             colClasses = list(character = c("datum", "planstatus", "dossierstatus", "typeplan", "dossier", "plan")))
  v[, datum := as.IDate(substr(datum, 1, 10))]
  ## historisch staat als True of False in de uitvoer van 28a (Python)
  v[, historisch := as.integer(as.logical(historisch))]
  v[, rang := fcase(planstatus == "onherroepelijk", 1L, planstatus %chin% c("vastgesteld", "vigerend"), 2L,
                    planstatus %like% "goedgekeurd|uitspraak", 3L, default = 4L)]
  list(paren = fread(file.path(B, paste0(laag, "_paren.csv")), sep = ";"), vlakken = v)
}
EB <- lees("Enkelbestemming"); BA <- lees("Bouwaanduiding"); MV <- lees("Maatvoering"); BV <- lees("Bouwvlak")
for (nm in c("EB", "BA", "MV", "BV")) {
  L <- get(nm)
  cat(sprintf("%s: %s vlakken, %s paren, %s punten\n", nm, format(nrow(L$vlakken), big.mark = "."),
              format(nrow(L$paren), big.mark = "."), format(uniqueN(L$paren$pid), big.mark = ".")))
}
plannen <- fread(file.path(PATHS$verg, "bron", "plannen", "ruimtelijkeplannen_plannen_20260701.csv"), sep = ";", encoding = "UTF-8",
                 select = c("identificatie", "opp_ha", "typePlan", "naam"))
plannen <- unique(plannen, by = "identificatie")

## het plan per punt op een datum: het vlak van de enkelbestemming met de laatste datum
plan_op <- function(tot, nu = FALSE) {
  x <- EB$paren[EB$vlakken, on = "fid", nomatch = 0L]
  x <- x[!is.na(datum) & datum <= tot]
  if (nu) x <- x[!(dossierstatus %chin% "vervallen") & !(historisch %in% 1L)]
  setorder(x, pid, -datum, rang, -fid)
  x[!duplicated(pid), .(pid, plan, datum, hoofdgroep, typeplan)]
}
## bouwaanduiding en maatvoering van hetzelfde plan op dat punt
ORDE <- c(vrijstaand = 1L, `twee-aaneen` = 2L, aaneengebouwd = 3L, gestapeld = 4L)
van_plan <- function(L, pp) {
  x <- L$paren[L$vlakken, on = "fid", nomatch = 0L]
  x[pp, on = .(pid, plan), nomatch = 0L]
}
stand <- function(tot, nu = FALSE) {
  pp <- plan_op(tot, nu)
  ba <- van_plan(BA, pp)[, .(orde = max(ORDE[woningtype])), by = pid]
  mv <- van_plan(MV, pp)
  bv <- unique(van_plan(BV, pp)[, .(pid)])
  pp[ba, on = "pid", orde := i.orde]
  pp[mv[, .(hoogte = suppressWarnings(max(bouwhoogte, na.rm = TRUE))), by = pid][is.finite(hoogte)], on = "pid", hoogte := i.hoogte]
  pp[, in_bouwvlak := pid %in% bv$pid]
  list(punt = pp, mv = mv)
}
S12 <- stand(as.IDate("2011-12-31"))
S14 <- stand(as.IDate("2013-12-31"))
SNU <- stand(as.IDate("2026-07-01"), nu = TRUE)

## per site: de meest voorkomende hoofdgroep, het ruimste woningtype, de hoogste bouwhoogte, de som van het maximum aantal
## wooneenheden over de vlakken van de maatvoering, en de datum van het plan
per_site <- function(S) {
  x <- sp[S$punt, on = "pid", nomatch = 0L]
  hg <- x[!is.na(hoofdgroep), .N, by = .(site_id, hoofdgroep)][order(site_id, -N, hoofdgroep)][!duplicated(site_id), .(site_id, hoofdgroep)]
  mx <- function(v) if (all(is.na(v))) NA_real_ else as.numeric(max(v, na.rm = TRUE))
  rest <- x[, .(orde = mx(orde), hoogte = mx(hoogte), bouwvlak = mean(in_bouwvlak), plandatum = max(datum), n_punt = .N), by = site_id]
  rest[, orde := as.integer(orde)]
  we <- unique(sp[S$mv[!is.na(wooneenheden), .(pid, fid, wooneenheden)], on = "pid", nomatch = 0L, allow.cartesian = TRUE][, .(site_id, fid, wooneenheden)])
  we <- we[, .(wooneenheden = sum(wooneenheden)), by = site_id]
  out <- merge(hg, rest, by = "site_id", all = TRUE)
  out[we, on = "site_id", wooneenheden := i.wooneenheden]
  out
}
B12 <- per_site(S12); B14 <- per_site(S14); BNU <- per_site(SNU)

## ---- 2. de sites van het paper ---------------------------------------
s <- readRDS(file.path(W, "sites_Nederland_20260710.rds"))
st <- s$attrs[!is.na(site_id), .(site_id, oad, gemeente_code, site_size)]
st[oad >= 4294967295, oad := NA]
st[, groep := sub("_.*$", "", site_id)][groep == "OnvS", groep := "Onv"]
st[, heeft_inc := site_id %chin% s$incumbent$site_id]
st[s$new, on = "site_id", redev_ym := i.redev_yearmonth]
st[s$incumbent, on = "site_id", `:=`(n_obj = i.n_obj, n_units_res = i.n_units_res)]
rm(s); invisible(gc())
st[readRDS(file.path(W, "clusters_sn_Nederland_20260710.rds"))$sites, on = "site_id", K := i.cluster]
st[readRDS(file.path(W, "stage1_sn_Nederland_20260710.rds"))$iv, on = "site_id", iv := i.iv]
st[, stedelijk := !is.na(oad) & oad >= 1000]
U <- st[heeft_inc == TRUE & groep %chin% c("SN", "Onv", "S", "O")]
U[, redev_jaar := redev_ym %/% 100L]
U[B12, on = "site_id", `:=`(hg12 = i.hoofdgroep, orde12 = i.orde, hoogte12 = i.hoogte, bv12 = i.bouwvlak, datum12 = i.plandatum, we12 = i.wooneenheden)]
U[B14, on = "site_id", `:=`(hg14 = i.hoofdgroep, orde14 = i.orde, hoogte14 = i.hoogte, bv14 = i.bouwvlak, datum14 = i.plandatum, we14 = i.wooneenheden)]
U[BNU, on = "site_id", `:=`(hgnu = i.hoofdgroep, ordenu = i.orde, hoogtenu = i.hoogte, bvnu = i.bouwvlak, datumnu = i.plandatum, wenu = i.wooneenheden)]
TYPE <- c("vrijstaand", "twee-aaneen", "aaneengebouwd", "gestapeld")
U[, type12 := fifelse(is.na(hg12), NA_character_, fifelse(is.na(orde12), "geen aanduiding", TYPE[pmax(orde12, 1L)]))]
U[, type14 := fifelse(is.na(hg14), NA_character_, fifelse(is.na(orde14), "geen aanduiding", TYPE[pmax(orde14, 1L)]))]
U[, typenu := fifelse(is.na(hgnu), NA_character_, fifelse(is.na(ordenu), "geen aanduiding", TYPE[pmax(ordenu, 1L)]))]

## ---- A. dekking ------------------------------------------------------
R$A_dekking <- U[, .(sites = .N, pct_bestemming_2012 = pct(!is.na(hg12)), pct_bestemming_2014 = pct(!is.na(hg14)), pct_bestemming_nu = pct(!is.na(hgnu)),
                     pct_aanduiding_2014 = pct(!is.na(orde14)), pct_hoogte_2014 = pct(!is.na(hoogte14)),
                     pct_aanduiding_2012 = pct(!is.na(orde12)), pct_hoogte_2012 = pct(!is.na(hoogte12)),
                     pct_aanduiding_nu = pct(!is.na(ordenu)), pct_hoogte_nu = pct(!is.na(hoogtenu)), pct_wooneenheden_nu = pct(!is.na(wenu))),
                 by = .(groep, stedelijk)][order(groep, stedelijk)]
cat("\nA. Dekking (procent van de sites):\n"); print(ROND(R$A_dekking, 1))

## ---- B. de gerealiseerde projecten tegenover de onveranderde sites ------
U[, g := fcase(groep == "SN" & redev_jaar >= 2018L, "SN, oplevering 2018 tot en met 2026", groep == "Onv", "onveranderd", default = NA_character_)]
R$B_dekking <- U[!is.na(g), .(sites = .N, pct_met_stand_2014 = pct(!is.na(hg14))), by = .(stedelijk, g)][order(stedelijk, g)]
print(ROND(R$B_dekking, 1))
V <- U[!is.na(g) & !is.na(hg14)]
R$B_hoofdgroep <- dcast(V[, .N, by = .(stedelijk, g, hg14)][, pct := 100 * N / sum(N), by = .(stedelijk, g)],
                        stedelijk + hg14 ~ g, value.var = "pct", fill = 0)[order(stedelijk, -onveranderd)]
cat("\nB. Hoofdgroep van de bestemming eind 2013 (procent), gerealiseerd tegenover onveranderd:\n"); print(ROND(R$B_hoofdgroep, 1))
R$B_type <- dcast(V[, .N, by = .(stedelijk, g, type14)][, pct := 100 * N / sum(N), by = .(stedelijk, g)],
                  stedelijk + type14 ~ g, value.var = "pct", fill = 0)
cat("\nWoningtype volgens de bouwaanduiding eind 2013 (procent):\n"); print(ROND(R$B_type, 1))
R$B_hoogte <- V[, .(sites = .N, met_hoogte = sum(!is.na(hoogte14)), hoogte_med = as.numeric(median(hoogte14, na.rm = TRUE)),
                    hoogte_p75 = as.numeric(quantile(hoogte14, 0.75, na.rm = TRUE)), pct_in_bouwvlak = pct(bv14 > 0)), by = .(stedelijk, g)][order(stedelijk, g)]
print(ROND(R$B_hoogte, 1))
## het gerealiseerde alternatief tegenover het toegestane woningtype van eind 2013
KORDE <- c(1L, 4L, 2L, 4L, 3L, 1L)   # K1 vrijstaand, K2 appartementen hoog, K3 twee-onder-een-kap, K4 appartementen midden, K5 rijtjes, K6 vrijstaand groot
KNAAM <- c("1 vrijstaand", "2 appartementen hoogdicht", "3 twee-onder-een-kap", "4 appartementen middeldicht", "5 rijtjeswoningen", "6 vrijstaand groot")
sk <- V[groep == "SN" & !is.na(K)]
sk[, `:=`(Knaam = KNAAM[K], dichter = !is.na(orde14) & KORDE[K] > orde14)]
R$B_K_type <- dcast(sk[, .N, by = .(Knaam, type14)], Knaam ~ type14, value.var = "N", fill = 0)
R$B_K_dichter <- sk[, .(sites = .N, met_aanduiding = sum(!is.na(orde14)), pct_dichter_dan_toegestaan = pct(dichter[!is.na(orde14)]),
                        pct_wonen_2014 = pct(hg14 %chin% c("wonen", "woongebied"))), by = Knaam][order(Knaam)]
cat("\nGerealiseerd alternatief K tegenover de bouwaanduiding eind 2013 (aantal sites):\n"); print(R$B_K_type)
print(ROND(R$B_K_dichter, 1))

## ---- C. planwijziging rond het project ---------------------------------
## alle vastgestelde vlakken van de enkelbestemming die een punt van de site raken, met de oppervlakte van hun plan
alle <- EB$paren[EB$vlakken[!is.na(datum), .(fid, plan, datum, typeplan)], on = "fid", nomatch = 0L]
alle[plannen, on = .(plan = identificatie), opp_ha := i.opp_ha]
alle <- unique(sp[alle, on = "pid", nomatch = 0L, allow.cartesian = TRUE][, .(site_id, plan, datum, opp_ha, typeplan)])
alle[, klein := !is.na(opp_ha) & opp_ha < 10]
cat(sprintf("\nC. plannen per site: %s koppelingen, klein (onder 10 ha) %.1f%%, plan zonder oppervlakte %.1f%%\n",
            format(nrow(alle), big.mark = "."), pct(alle$klein), pct(is.na(alle$opp_ha))))
sn <- U[groep == "SN" & redev_jaar >= 2014L, .(site_id, redev = as.IDate(sprintf("%d-%02d-15", redev_ym %/% 100L, redev_ym %% 100L)), stedelijk)]
x <- alle[klein == TRUE][sn, on = "site_id", nomatch = 0L]
x[, jaren := as.numeric(datum - redev) / 365.25]
x <- x[jaren >= -8 & jaren < 3]
x[, jaar_rel := floor(jaren)]
R$C_rond_project <- x[, .(sites = uniqueN(site_id)), by = .(stedelijk, jaar_rel)]
R$C_rond_project[sn[, .N, by = stedelijk], on = "stedelijk", pct := 100 * sites / i.N]
setorder(R$C_rond_project, stedelijk, jaar_rel)
## de kans per jaar bij onveranderde sites: een klein plan met datum in dat jaar, 2014 tot en met 2025
onv <- U[groep == "Onv", .(site_id, stedelijk)]
y <- alle[klein == TRUE & year(datum) %between% c(2014L, 2025L)][onv, on = "site_id", nomatch = 0L]
oj <- y[, .(sites = uniqueN(site_id)), by = .(stedelijk, jaar = year(datum))]
oj[onv[, .N, by = stedelijk], on = "stedelijk", pct := 100 * sites / i.N]
R$C_onveranderd_per_jaar <- oj[, .(pct_per_jaar = mean(pct)), by = stedelijk]
cat("\nKlein plan dat de site raakt, per jaar ten opzichte van de oplevering (procent van de SN-sites):\n")
print(dcast(ROND(R$C_rond_project, 2), jaar_rel ~ stedelijk, value.var = "pct"))
cat("Ter vergelijking, onveranderde sites, gemiddeld per kalenderjaar 2014 tot en met 2025:\n"); print(ROND(R$C_onveranderd_per_jaar, 3))
## per site: een klein plan in de vijf jaar voor de oplevering (SN vanaf 2018), naar stedelijkheid, alternatief en bestemming eind 2013
x5 <- unique(x[jaren >= -5 & jaren < 0, .(site_id)])
sn5 <- U[groep == "SN" & redev_jaar >= 2018L, .(site_id, stedelijk, K, hg14)]
sn5[, klein_plan_5j := site_id %chin% x5$site_id]
sn5[, bestemming := fifelse(is.na(hg14), "geen stand 2014", fifelse(hg14 %chin% c("wonen", "woongebied"), "wonen", "niet wonen"))]
R$C_vijf_jaar <- rbind(sn5[, .(indeling = "stedelijk", waarde = as.character(stedelijk), sites = .N, pct = pct(klein_plan_5j)), by = stedelijk][, !"stedelijk"],
                       sn5[!is.na(K), .(indeling = "alternatief", sites = .N, pct = pct(klein_plan_5j)), by = .(waarde = KNAAM[K])],
                       sn5[, .(indeling = "bestemming eind 2013", sites = .N, pct = pct(klein_plan_5j)), by = .(waarde = bestemming)])
R$C_onveranderd_vijf_jaar <- R$C_onveranderd_per_jaar[, .(stedelijk, pct_vijf_jaar_benadering = 5 * pct_per_jaar)]
cat("\nKlein plan in de vijf jaar voor de oplevering, per site (SN, oplevering 2018 tot en met 2026):\n"); print(ROND(R$C_vijf_jaar, 1))

## ---- D. de bestemming van 2012 en herontwikkeling daarna -----------------
D <- U[stedelijk == TRUE & groep %chin% c("SN", "Onv") & !is.na(hg14) & (groep == "Onv" | redev_jaar >= 2018L)]
D[, `:=`(y = 100 * (groep == "SN"), l_opp = log(pmax(site_size, 1)), l_obj = log(pmax(n_obj, 1)),
         typ = factor(type14, levels = c("geen aanduiding", TYPE)),
         hg = factor(fifelse(hg14 %chin% c("wonen", "woongebied", "gemengd", "centrum", "bedrijf", "maatschappelijk", "tuin"), hg14, "overig")))]
D[, hg := relevel(hg, "wonen")]
R$D_type <- D[, .(sites = .N, pct_sn = pct(y / 100)), by = typ][order(typ)]
R$D_hoofdgroep <- D[, .(sites = .N, pct_sn = pct(y / 100)), by = hg][order(-sites)]
cat("\nD. Stedelijk: aandeel met sloop-nieuwbouw (oplevering 2018 tot en met 2026) naar de bestemming van eind 2013:\n")
print(ROND(R$D_type, 3)); print(ROND(R$D_hoofdgroep, 3))
m1 <- feols(y ~ i(typ, ref = "geen aanduiding") + l_opp + l_obj | gemeente_code, data = D, cluster = ~gemeente_code, notes = FALSE)
m2 <- feols(y ~ i(hg, ref = "wonen") + l_opp + l_obj | gemeente_code, data = D, cluster = ~gemeente_code, notes = FALSE)
m3 <- feols(y ~ i(typ, ref = "geen aanduiding") + i(hg, ref = "wonen") + l_opp + l_obj + iv | gemeente_code, data = D[!is.na(iv)], cluster = ~gemeente_code, notes = FALSE)
tab <- function(m, naam) { ct <- coeftable(m); data.table(model = naam, term = rownames(ct), b = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m)) }
R$D_modellen <- rbind(tab(m1, "woningtype"), tab(m2, "hoofdgroep"), tab(m3, "beide, met inclusive value"))
cat("\nBinnen de gemeente, met log oppervlakte en log objecten (procentpunt SN):\n"); print(ROND(R$D_modellen, 3))
## dezelfde vergelijking als Poisson: bij een gemiddelde van ongeveer 2 procent is een relatieve kans beter te lezen dan
## procentpunten uit een lineair model, dat onder nul kan voorspellen
D[, y01 := as.integer(groep == "SN")]
p3 <- fepois(y01 ~ i(typ, ref = "geen aanduiding") + i(hg, ref = "wonen") + l_opp + l_obj + iv | gemeente_code, data = D[!is.na(iv)], cluster = ~gemeente_code, notes = FALSE)
R$D_poisson <- tab(p3, "beide, met inclusive value, Poisson")[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
cat("\nPoisson, relatieve kans op SN binnen de gemeente:\n"); print(ROND(R$D_poisson, 3))
R$D_gemiddeld <- D[, pct(y / 100)]

R$meta <- list(stand_2012 = "2011-12-31", stand_2014 = "2013-12-31", stand_nu = "2026-07-01", bron = "PDOK ATOM Ruimtelijke plannen, stand 1 juli 2026",
               gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("bestemming_sites_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("bestemming_sites_%s.json", EXTRACT)))
sink()
