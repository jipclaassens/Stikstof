## =====================================================================
## 27_pijplijn_sites.R -- proef voor issue 30 in de repo Redevelopment:
##                        de bekendmakingen tussen rendabel en gerealiseerd
##
## Input : C:/LocalData/Redevelopment/R_werk/ (EconLogicPaper/R, 02 tot 06):
##         perobject_, sites_, clusters_sn_ en stage1_sn_Nederland_20260710.rds;
##         _derived/pijplijn/ (27a_pijplijn_export.py); _derived/vbo_base.rds
##         (01_build_vbo.R, de BAG-datums per nieuw object); analyse/zaken.csv
##         van het vergunningenspoor (soort, uitgebreide procedure)
## Output: _output/pijplijn_sites_<extract>.json en .txt;
##         _derived/econlogic_sites_obj.rds (slanke objecttabel, cache)
##
## A. De sites met nieuwbouw, en daarbinnen de sloop-nieuwbouwsites (SN):
##    hun nieuwe panden via zaak_bag aan de zaak. Alleen panden met de BAG-
##    status Bouwvergunning verleend vanaf juni 2021 zijn gekoppeld, dus de
##    dekking telt binnen dat venster. Per groep: soort, procedure, uitkomst,
##    doorlooptijd, beroep, en de tijd van vergunning tot bouwstart en
##    oplevering; per alternatief K van stage 1 en naar OAD.
## B. De route over het documentnummer: het BAG-documentnummer van het
##    nieuwe pand of object tegen het zaaknummer van de gemeente, alleen als
##    de zaak binnen 2 kilometer ligt. Die route zou ook voor 2021 werken,
##    waar een gemeente toen al publiceerde; ze vindt weinig.
## C. Pogingen op alle sites van stage 2: zaken voor nieuwbouw van woningen
##    of sloop van een woning met een precieze plek (punt, adres, perceel:
##    binnen 25 meter van een object; een vlak tot 100 meter breed: binnen
##    50 meter), niet op een punt dat de gemeente voor meer dan vijf zaken
##    gebruikt, met de eerste publicatie vanaf juli 2021 (landelijke
##    dekking) tot en met juni 2026 (de export). Strikt: niet gevolgd door
##    nieuwbouw van een andere site binnen 30 meter, binnen 5 meter van het
##    object, en bouwen of oprichten als los woord zonder verbouw, splitsing,
##    mantelzorg- of tijdelijke woning, of sloop van een woning; vervanging:
##    slopen en bouwen in dezelfde zaak. Per groep (SN, sloop zonder
##    vervolg, onttrekking, onveranderd), met de vangst bij echte projecten,
##    de uitkomst, het vervolg in de BAG (zaak_bag) en de inclusive value
##    van stage 1, ruw en binnen de gemeente met de sitegrootte als controle.
## Lees C als verkenning: een zaak aan het dichtstbijzijnde object is niet
## altijd een zaak over die site, en de titels vangen niet elke nieuwbouw
## (vergunningen/inzichten.md, hoofdstuk 36).
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
stopifnot(requireNamespace("bit64", quietly = TRUE))
options(width = 200)
t0 <- Sys.time()
sink(file.path(PATHS$output, sprintf("pijplijn_sites_%s.txt", EXTRACT)), split = TRUE)

W  <- Sys.getenv("ECONLOGIC_WERK", "C:/LocalData/Redevelopment/R_werk")
PL <- file.path(PATHS$derived, "pijplijn")
VENSTER_BAG <- as.IDate("2021-06-01")   # zaak_bag: panden met de vergunningstatus vanaf juni 2021
LANDELIJK   <- as.IDate("2021-07-01")   # sindsdien publiceren alle gemeenten in de bekendmakingen
EIND        <- as.IDate("2026-06-30")   # de EconLogic-export is van juli 2026
R <- list()
datum <- function(v) if (inherits(v, "IDate")) v else as.IDate(substr(as.character(v), 1, 10))
eerst <- function(v) if (all(is.na(v))) as.IDate(NA) else min(v, na.rm = TRUE)
med   <- function(v) if (all(is.na(v))) NA_real_ else as.numeric(median(v, na.rm = TRUE))
pct   <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)

## ---- 0. objecten en sites van EconLogic --------------------------------
slank <- file.path(PATHS$derived, "econlogic_sites_obj.rds")
if (!file.exists(slank)) {
  x <- readRDS(file.path(W, "perobject_Nederland_20260710.rds"))
  stopifnot(max(x$vbo_bag_nr, na.rm = TRUE) < 2^53, max(x$pand_bag_nr, na.rm = TRUE) < 2^53)
  o <- x[, .(site_id, vbo_id = sprintf("%016.0f", vbo_bag_nr), pand_id = sprintf("%016.0f", pand_bag_nr), pand_docnum,
             is_plus, is_min, is_incumbent, redev_type = as.character(redev_type_lbl), redev_yearmonth,
             x = x_coord, y = y_coord)]
  rm(x); invisible(gc())
  ## de uint32-nulwaarde van GeoDMS als coördinaat (zie 21a_econlogic.R)
  o[x >= 4294967295 | y >= 4294967295, `:=`(x = NA, y = NA)]
  saveRDS(o, slank)
} else o <- readRDS(slank)
kop("Objecten: %s in %s sites", format(nrow(o), big.mark = "."), format(uniqueN(o$site_id), big.mark = "."))

s  <- readRDS(file.path(W, "sites_Nederland_20260710.rds"))
st <- s$attrs[!is.na(site_id), .(site_id, oad, gemeente_code, site_size)]
st[oad >= 4294967295, oad := NA]
st[, groep := sub("_.*$", "", site_id)][groep == "OnvS", groep := "Onv"]
st[, heeft_inc := site_id %chin% s$incumbent$site_id]
st[s$new, on = "site_id", `:=`(redev_ym = i.redev_yearmonth, n_units_new = i.n_units_new)]
st[s$incumbent, on = "site_id", `:=`(n_units_res = i.n_units_res, n_obj = i.n_obj)]
rm(s); invisible(gc())
st[readRDS(file.path(W, "clusters_sn_Nederland_20260710.rds"))$sites, on = "site_id", K := i.cluster]
st[readRDS(file.path(W, "stage1_sn_Nederland_20260710.rds"))$iv, on = "site_id", iv := i.iv]
st[, stedelijk := !is.na(oad) & oad >= 1000]   # de scope van stage 2 (cfg$oad_min in EconLogicPaper)
cat("\nSites naar voorvoegsel, met en zonder bestaande opstal:\n")
print(dcast(st[, .N, by = .(groep, heeft_inc)], groep ~ heeft_inc, value.var = "N", fill = 0)[order(-`TRUE`)])
R$sites <- st[, .(sites = .N, met_opstal = sum(heeft_inc), met_nieuwbouw = sum(!is.na(redev_ym)), met_K = sum(!is.na(K)),
                  stedelijk = sum(stedelijk)), by = groep][order(-sites)]

## ---- 1. de zaken ------------------------------------------------------
zk <- fread(file.path(PL, "pijplijn_zaak.csv"), sep = ";", encoding = "UTF-8",
            colClasses = list(character = c("creator", "sleutelsoort", "sleutel", "uitkomst", "locatietype", "eerste_identifier", "titels")))
for (v in c("eerste_datum", "datum_aanvraag", "datum_uitkomst")) set(zk, j = v, value = datum(zk[[v]]))
za <- fread(file.path(PATHS$verg, "analyse", "zaken.csv"), sep = ";", encoding = "UTF-8",
            select = c("zaak_id", "gemeente", "datum_aanvraag", "uitgebreid", "soort", "woningen"))
za[, datum_aanvraag := datum(datum_aanvraag)]
## controle: zaken.csv hoort bij dezelfde zaakvorming als de database
chk <- merge(zk[, .(zaak_id, a1 = datum_aanvraag, g1 = creator)], za[, .(zaak_id, a2 = datum_aanvraag, g2 = gemeente)], by = "zaak_id")
gelijk <- chk[, mean(a1 == a2 & g1 == g2, na.rm = TRUE)]
kop("zaken.csv: %s zaken, %s in de zaaktabel, %.3f%% met dezelfde gemeente en aanvraagdatum",
    format(nrow(za), big.mark = "."), format(nrow(chk), big.mark = "."), 100 * gelijk)
stopifnot(gelijk > 0.999)
zk[za, on = "zaak_id", `:=`(uitgebreid = i.uitgebreid, soort = i.soort, woningen = i.woningen)]
rm(za, chk); invisible(gc())
## nieuwbouw van woningen: de soort uit zaken.csv (analyse_export.py, over alle titels van de zaak); voor zaken zonder
## aanvraag, die daar niet in staan, dezelfde regel op de bewaarde titels. Die regel heeft geen woordgrens, zodat ook
## "verbouwen van een schuur tot woning" meetelt; daarom daarnaast nieuwbouw_echt: bouwen, oprichten of nieuwbouw als
## los woord, zonder realiseren (vaak in een bestaand gebouw), zonder verbouw, splitsing of transformatie, en zonder
## mantelzorgwoning of tijdelijke woning. Vervanging: slopen en nieuwbouw_echt in de titels van dezelfde zaak, of
## vervangende nieuwbouw of herbouw. Sloop van een woning: een strengere regel dan alleen het woord slopen, want slopen van
## een schuur of aanbouw bij een woning telt niet.
RX_NIEUWBOUW <- paste0("(bouwen|oprichten|realiseren|nieuwbouw) (van )?(een |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?",
                       "(nieuwe |vrijstaande |levensloopbestendige |sociale |tijdelijke )?(woning|woningen|woonhuis|appartement|appartementen|",
                       "studentenwoning|wooneenhe|woonzorg|woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|",
                       "eengezinswoning|meergezins|flexwoning|mantelzorgwoning)|nieuwbouwwoning|woningbouw|nieuwbouw van|nieuwbouwproject|",
                       "[0-9]+ (woningen|appartementen|studentenwoningen|wooneenheden|studio's)")
RX_SLOOPWONING <- paste0("(slopen|sloop) (van )?(een |de |het |twee |drie |[0-9]+ )?(bestaande |voormalige |vrijstaande |oude )?",
                         "(woning|woningen|woonhuis|woonhuizen|appartement|appartementen|flat|flats|bedrijfswoning|boerderij|",
                         "woonboerderij|woongebouw|woonblok|portiekflat|galerijflat|rijtjeswoning|eengezinswoning|bungalow|villa)")
zk[, nieuwbouw := fifelse(!is.na(soort), soort == "nieuwbouw_woning", grepl(RX_NIEUWBOUW, titels, ignore.case = TRUE))]
zk[, sloop_woning := grepl(RX_SLOOPWONING, titels, ignore.case = TRUE)]
RX_ECHT <- paste0("(^|[^a-z])(bouwen|oprichten|nieuwbouw) (van )?(een |de |het |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?",
                  "(nieuwe |vrijstaande |levensloopbestendige |sociale |grondgebonden )?(woning|woningen|woonhuis|appartement|appartementen|",
                  "woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|rijwoningen|eengezinswoning|meergezins)|",
                  "nieuwbouwwoning|nieuwbouw van|vervangende nieuwbouw|herbouw")
RX_NIET <- "verbouw|splits|transformatie|omzetten|mantelzorg|tijdelijk|recreatiewoning|vakantiewoning|chalet|woonwagen|woonark|woonboot"
zk[, nieuwbouw_echt := grepl(RX_ECHT, titels, ignore.case = TRUE) & !grepl(RX_NIET, titels, ignore.case = TRUE)]
zk[, vervanging := !grepl(RX_NIET, titels, ignore.case = TRUE) &
     ((t_slopen == 1L & nieuwbouw_echt) | grepl("vervangende nieuwbouw|herbouw", titels, ignore.case = TRUE))]
cat(sprintf("nieuwbouw_echt %s zaken, vervanging %s\n", format(zk[, sum(nieuwbouw_echt)], big.mark = "."), format(zk[, sum(vervanging)], big.mark = ".")))
R$zaken <- list(gemeentelijk = nrow(zk), nieuwbouw = zk[, sum(nieuwbouw)], sloop_woning = zk[, sum(sloop_woning)],
                zaaknummer = zk[, sum(sleutelsoort == "zaaknummer")])
cat(sprintf("nieuwbouw van woningen %s zaken, sloop van een woning %s\n", format(R$zaken$nieuwbouw, big.mark = "."),
            format(R$zaken$sloop_woning, big.mark = ".")))

## ---- A. sites met nieuwbouw aan hun zaak --------------------------------
pz <- fread(file.path(PL, "pijplijn_pand_zaak.csv"), sep = ";", colClasses = list(character = "pand_id"))
for (v in c("verg_begin", "start_begin", "gebruik_begin", "niet_gerealiseerd_begin")) set(pz, j = v, value = datum(pz[[v]]))
stopifnot(!anyDuplicated(pz$pand_id))
vb <- readRDS(file.path(PATHS$derived, "vbo_base.rds"))
vb <- vb[, .(vbo_id = sprintf("%016.0f", as.double(vbo_bag_nr)), verg = as.IDate(vergunning_datum),
             start = as.IDate(bouwstart_eerste_datum), opl = as.IDate(oplevering_eerste_datum), docnr = documentnummer)]
stopifnot(!anyDuplicated(vb$vbo_id))
pl <- o[is_plus == TRUE & site_id %chin% st[!is.na(redev_ym), site_id],
        .(site_id, vbo_id, pand_id, pand_docnum, x, y)]
pl[vb, on = "vbo_id", `:=`(verg = i.verg, start = i.start, opl = i.opl, docnr = i.docnr)]
rm(vb); invisible(gc())
kop("Nieuwe objecten op sites met nieuwbouw: %s, met een BAG-vergunningdatum %.1f%%, met een pand in zaak_bag %.1f%%",
    format(nrow(pl), big.mark = "."), pct(!is.na(pl$verg)), pct(pl$pand_id %chin% pz$pand_id))

## per site: de vroegste vergunning, bouwstart en oplevering over de nieuwe objecten
ps <- pl[, .(verg = eerst(verg), start = eerst(start), opl = eerst(opl)), by = site_id]
## per site de zaak die de meeste nieuwe panden kreeg, bij gelijkspel de vroegste vergunning
pp <- unique(pl[, .(site_id, pand_id)])[pz, on = "pand_id", nomatch = 0L]
sz <- pp[, .(n_panden = .N, verg_z = min(verg_begin), start_z = eerst(start_begin), gebruik_z = eerst(gebruik_begin),
             niet_ger = any(!is.na(niet_gerealiseerd_begin))), by = .(site_id, zaak_id)]
setorder(sz, site_id, -n_panden, verg_z)
sz[, n_zaken := .N, by = site_id]
sz <- sz[!duplicated(site_id)]
A <- st[!is.na(redev_ym), .(site_id, groep, K, stedelijk, redev_ym, n_units_new, gemeente_code)]
A[ps, on = "site_id", `:=`(verg = i.verg, start = i.start, opl = i.opl)]
A[sz, on = "site_id", `:=`(zaak_id = i.zaak_id, n_zaken = i.n_zaken, niet_ger = i.niet_ger)]
A[zk, on = "zaak_id", `:=`(z_nieuwbouw = i.nieuwbouw, z_soort = i.soort, z_uitgebreid = i.uitgebreid, z_uitkomst = i.uitkomst,
                           z_duur = i.doorlooptijd_dagen, z_verl = i.n_verlengingen, z_beroep = i.n_uitspraken, z_ro = i.act_ro,
                           z_aanvraag = i.datum_aanvraag, z_uitkomst_datum = i.datum_uitkomst)]
A[, venster := !is.na(verg) & verg >= VENSTER_BAG]
A[, sn := groep == "SN"]
maat <- function(d) {
  g <- d[venster == TRUE]
  k <- g[!is.na(zaak_id)]
  data.table(sites = nrow(d), in_venster = nrow(g), gekoppeld = nrow(k), pct_gekoppeld = pct(!is.na(g$zaak_id)),
             pct_nieuwbouwzaak = pct(k$z_nieuwbouw), pct_sloop_kap = pct(k$z_soort %chin% "sloop_kap"),
             pct_uitgebreid = pct(k$z_uitgebreid == 1L), pct_verleend = pct(k$z_uitkomst %chin% "verleend"),
             duur_med = med(k$z_duur), pct_verlengd = pct(k$z_verl > 0), pct_beroep = pct(k$z_beroep > 0), pct_ro = pct(k$z_ro == 1L),
             pct_meer_zaken = pct(k$n_zaken > 1),
             nb_zaken = sum(k$z_nieuwbouw %in% TRUE), nb_pct_uitgebreid = pct(k[z_nieuwbouw %in% TRUE, z_uitgebreid == 1L]),
             nb_duur_med = med(k[z_nieuwbouw %in% TRUE, z_duur]),
             aanvraag_tot_bagverg_med = med(as.integer(g$verg - g$z_aanvraag)),
             verg_tot_start_med = med(as.integer(g$start - g$verg)), start_tot_opl_med = med(as.integer(g$opl - g$start)),
             verg_tot_opl_med = med(as.integer(g$opl - g$verg)))
}
R$A_groep <- rbind(cbind(groep = "SN, alle", maat(A[sn == TRUE])), cbind(groep = "SN, stedelijk (OAD vanaf 1.000)", maat(A[sn == TRUE & stedelijk == TRUE])),
                   cbind(groep = "SN, niet stedelijk", maat(A[sn == TRUE & stedelijk == FALSE])),
                   cbind(groep = "overige nieuwbouw", maat(A[sn == FALSE])), cbind(groep = "overige nieuwbouw, stedelijk", maat(A[sn == FALSE & stedelijk == TRUE])))
R$A_K <- rbindlist(lapply(1:6, function(k) cbind(K = k, maat(A[sn == TRUE & K == k]))))
R$A_K_stedelijk <- rbindlist(lapply(1:6, function(k) cbind(K = k, maat(A[sn == TRUE & K == k & stedelijk == TRUE]))))
R$A_jaar <- A[sn == TRUE, .(sites = .N, in_venster = sum(venster), gekoppeld = sum(venster & !is.na(zaak_id))), by = .(jaar = redev_ym %/% 100L)][order(jaar)]
cat("\nA. Sites met nieuwbouw, gekoppeld via zaak_bag (binnen het venster: BAG-vergunning vanaf juni 2021):\n")
print(R$A_groep[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("\nSN naar alternatief K:\n"); print(R$A_K[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("\nSN, stedelijk, naar alternatief K:\n"); print(R$A_K_stedelijk[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
cat("\nSN naar opleverjaar:\n"); print(R$A_jaar)
## welke soort zaak de nieuwe panden kregen
R$A_soort <- A[venster == TRUE & !is.na(zaak_id), .N, by = .(sn, soort = fcoalesce(z_soort, "geen aanvraag in zaken.csv"))][, pct := 100 * N / sum(N), by = sn][order(sn, -N)]
cat("\nSoort van de gekoppelde zaak:\n"); print(R$A_soort)
## verschillen tussen de alternatieven binnen de gemeente: lineair, standaardfouten geclusterd op gemeente
tk <- A[sn == TRUE & venster == TRUE & !is.na(zaak_id)]
tk[, `:=`(uitgebr = 100 * (z_uitgebreid %in% 1L), log_duur = log(pmax(z_duur, 1)),
          log_verg_start = log(pmax(as.integer(start - verg), 1)), log_start_opl = log(pmax(as.integer(opl - start), 1)))]
toets_K <- function(y, d) {
  m <- feols(as.formula(sprintf("%s ~ i(K, ref = 1) | gemeente_code", y)), data = d, cluster = ~gemeente_code, notes = FALSE)
  ct <- coeftable(m)
  data.table(uitkomst = y, K = as.integer(sub(".*::", "", rownames(ct))), b = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m))
}
R$A_toets_K <- rbind(toets_K("uitgebr", tk[z_nieuwbouw %in% TRUE]), toets_K("log_duur", tk[z_nieuwbouw %in% TRUE & !is.na(z_duur)]),
                     toets_K("log_verg_start", tk[!is.na(start) & start >= verg]), toets_K("log_start_opl", tk[!is.na(opl) & !is.na(start) & opl >= start]))
cat("\nAlternatief K tegen K = 1 (vrijstaand), binnen de gemeente (uitgebreid in procentpunt, de rest in log):\n")
print(R$A_toets_K[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])

## ---- B. de route over het documentnummer --------------------------------
norm <- function(v) gsub("[^A-Z0-9]", "", toupper(v))
zn <- zk[sleutelsoort == "zaaknummer" & !is.na(sleutel) & !is.na(x), .(zaak_id, k = norm(sleutel), zx = x, zy = y, z_eerste = eerste_datum)]
zn <- zn[nchar(k) >= 5]
koppel_doc <- function(kol) {
  dn <- unique(pl[!is.na(get(kol)) & get(kol) != "" & !is.na(x), .(site_id, k = norm(get(kol)), x, y)], by = c("site_id", "k"))
  dn <- dn[nchar(k) >= 5]
  m <- zn[dn, on = "k", nomatch = 0L, allow.cartesian = TRUE]
  m[, d := sqrt((x - zx)^2 + (y - zy)^2)]
  m <- m[d <= 2000]
  setorder(m, site_id, d)
  m[!duplicated(site_id), .(site_id, zaak_id_doc = zaak_id, d, z_eerste)]
}
for (kol in c("pand_docnum", "docnr")) {
  m <- koppel_doc(kol)
  A[m, on = "site_id", (paste0("doc_", kol)) := i.zaak_id_doc]
  cat(sprintf("\nB. documentnummer (%s): %s sites met een zaak met hetzelfde zaaknummer binnen 2 km, mediaan %.0f m\n",
              kol, format(nrow(m), big.mark = "."), med(m$d)))
}
A[, doc := fcoalesce(doc_docnr, doc_pand_docnum)]
R$B_jaar <- A[sn == TRUE, .(sites = .N, doc_pand = sum(!is.na(doc_pand_docnum)), doc_object = sum(!is.na(doc_docnr)),
                            doc_een_van_beide = sum(!is.na(doc)), bag_route = sum(!is.na(zaak_id)),
                            een_van_de_routes = sum(!is.na(doc) | !is.na(zaak_id)),
                            beide = sum(!is.na(doc) & !is.na(zaak_id)), zelfde_zaak = sum(!is.na(doc) & !is.na(zaak_id) & doc == zaak_id)),
                by = .(jaar = redev_ym %/% 100L)][order(jaar)]
R$B_jaar[, `:=`(pct_doc = 100 * doc_een_van_beide / sites, pct_routes = 100 * een_van_de_routes / sites)]
cat("\nSN naar opleverjaar, per route:\n"); print(R$B_jaar[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
## de zaak via het documentnummer: wat voor zaak en wanneer
A[zk, on = .(doc = zaak_id), `:=`(d_nieuwbouw = i.nieuwbouw, d_soort = i.soort, d_uitgebreid = i.uitgebreid, d_duur = i.doorlooptijd_dagen,
                                  d_eerste = i.eerste_datum, d_uitkomst = i.uitkomst)]
R$B_soort <- A[sn == TRUE & !is.na(doc), .(sites = .N, pct_nieuwbouwzaak = pct(d_nieuwbouw), pct_uitgebreid = pct(d_uitgebreid == 1L),
                                           duur_med = med(d_duur), pct_verleend = pct(d_uitkomst %chin% "verleend"),
                                           eerste_publicatie_vroegst = format(eerst(d_eerste))),
               by = .(periode = fifelse(d_eerste < LANDELIJK, "voor juli 2021", "vanaf juli 2021"))]
print(R$B_soort)

## ---- C. pogingen op alle sites ------------------------------------------
kand <- zk[(nieuwbouw | sloop_woning) & !is.na(x) & !is.na(eerste_datum) & eerste_datum >= LANDELIJK & eerste_datum <= EIND &
           n_zelfde_punt <= 5 &
           (locatietype %chin% c("Punt", "Adres", "Perceel") | (locatietype %chin% "Vlak" & !is.na(breedte_m) & breedte_m <= 100))]
kand[, r := fifelse(locatietype == "Vlak", 50, 25)]
cat(sprintf("\nC. %s zaken voor nieuwbouw van woningen of sloop van een woning met een precieze plek, juli 2021 tot en met juni 2026\n",
            format(nrow(kand), big.mark = ".")))
## het dichtstbijzijnde object binnen r meter, via een rooster met cellen van r meter
obj <- o[!is.na(x), .(site_id, ox = x, oy = y)]
dichtst <- function(p, straal) {
  ob <- obj[, .(site_id, ox, oy, cx = floor(ox / straal), cy = floor(oy / straal))]
  setkey(ob, cx, cy)
  res <- rbindlist(lapply(-1:1, function(dx) rbindlist(lapply(-1:1, function(dy) {
    q <- p[, .(zaak_id, px = x, py = y, cx = floor(x / straal) + dx, cy = floor(y / straal) + dy)]
    ob[q, on = .(cx, cy), nomatch = 0L, .(zaak_id, site_id, d = sqrt((ox - px)^2 + (oy - py)^2))]
  }))))
  res <- res[d <= straal]
  setorder(res, zaak_id, d)
  res[!duplicated(zaak_id)]
}
nn <- rbind(dichtst(kand[r == 25], 25), dichtst(kand[r == 50], 50))
rm(obj); invisible(gc())
kop("Aan een object gekoppeld: %s van %s zaken (%.1f%%), mediane afstand %.1f m",
    format(nrow(nn), big.mark = "."), format(nrow(kand), big.mark = "."), 100 * nrow(nn) / nrow(kand), med(nn$d))
pog <- merge(kand[, .(zaak_id, x, y, eerste_datum, uitkomst, uitgebreid, nieuwbouw, nieuwbouw_echt, vervanging, sloop_woning, n_uitspraken)], nn, by = "zaak_id")
pog[, zym := as.integer(format(eerste_datum, "%Y")) * 100L + as.integer(format(eerste_datum, "%m"))]
## verscheen er na de zaak binnen 30 meter van het zaakpunt een nieuw object van een andere site? Dan ging de zaak
## waarschijnlijk over die nieuwbouw, bijvoorbeeld op een leeg kavel naast het bestaande object, en niet over deze site
nieuw <- o[is_plus == TRUE & !is.na(x), .(site_n = site_id, nx = x, ny = y, ym = redev_yearmonth, cx = floor(x / 30), cy = floor(y / 30))]
setkey(nieuw, cx, cy)
eld <- rbindlist(lapply(-1:1, function(dx) rbindlist(lapply(-1:1, function(dy) {
  q <- pog[, .(zaak_id, site_id, px = x, py = y, zym, cx = floor(x / 30) + dx, cy = floor(y / 30) + dy)]
  nieuw[q, on = .(cx, cy), nomatch = 0L, .(zaak_id, site_id, site_n, ym, zym, d = sqrt((nx - px)^2 + (ny - py)^2))]
}))))
pog[, elders := zaak_id %in% eld[d <= 30 & ym >= zym & site_n != site_id, zaak_id]]
rm(nieuw, eld); invisible(gc())
pog[, strikt := !elders & d <= 5 & (nieuwbouw_echt | vervanging | sloop_woning)]
pog[, verv := !elders & d <= 5 & vervanging]
cat(sprintf("Gekoppelde zaken: %s; daarna nieuwbouw van een andere site binnen 30 m: %.1f%%; strikt (niet elders, binnen 5 m, nieuwbouw_echt, vervanging of sloop van een woning): %s; vervanging: %s\n",
            format(nrow(pog), big.mark = "."), pct(pog$elders), format(pog[, sum(strikt)], big.mark = "."), format(pog[, sum(verv)], big.mark = ".")))
## toets van de ruimtelijke koppeling: de nieuwbouwzaken van SN-sites uit zaak_bag, vindt de plek dezelfde site?
tst <- merge(A[sn == TRUE & !is.na(zaak_id), .(zaak_id, site_bag = site_id)], pog[, .(zaak_id, site_plek = site_id)], by = "zaak_id")
tst_n <- A[sn == TRUE & !is.na(zaak_id) & zaak_id %in% kand$zaak_id, .N]
R$C_toets <- list(zaken_bag_in_kandidaten = tst_n, ruimtelijk_gekoppeld = nrow(tst), zelfde_site = tst[, sum(site_bag == site_plek)])
cat(sprintf("Toets: %d SN-zaken uit zaak_bag staan bij de kandidaten; %d ruimtelijk gekoppeld, waarvan %d aan dezelfde site\n",
            R$C_toets$zaken_bag_in_kandidaten, R$C_toets$ruimtelijk_gekoppeld, R$C_toets$zelfde_site))
per_site <- pog[, .(n_zaken = .N, nieuwbouw = any(nieuwbouw), sloop = any(sloop_woning), uitgebreid = any(uitgebreid %in% 1L),
                    verleend = any(uitkomst %chin% "verleend"), geweigerd = any(uitkomst %chin% "geweigerd"),
                    ingetrokken = any(uitkomst %chin% "ingetrokken"), beroep = any(n_uitspraken > 0), eerste = min(eerste_datum),
                    strikt = any(strikt), s_verleend = any(strikt & uitkomst %chin% "verleend"),
                    s_geweigerd = any(strikt & uitkomst %chin% "geweigerd"), s_ingetrokken = any(strikt & uitkomst %chin% "ingetrokken"),
                    s_uitgebreid = any(strikt & uitgebreid %in% 1L), s_eerste = eerst(eerste_datum[strikt]),
                    verv = any(verv), v_verleend = any(verv & uitkomst %chin% "verleend"), v_geweigerd = any(verv & uitkomst %chin% "geweigerd"),
                    v_ingetrokken = any(verv & uitkomst %chin% "ingetrokken")),
                by = site_id]
## het universum van stage 2: sites met bestaande opstal, plus SN (07_stage2_logit.R); SN naar oplevering voor of na juli 2021
U <- st[(heeft_inc == TRUE | groep == "SN") & groep %chin% c("SN", "Onv", "S", "O"), .(site_id, groep, stedelijk, redev_ym, iv, n_units_res, n_obj, site_size, gemeente_code)]
U[, groep2 := fcase(groep == "SN" & redev_ym >= 202107L, "SN, opgeleverd vanaf juli 2021", groep == "SN", "SN, opgeleverd voor juli 2021",
                    groep == "S", "sloop zonder vervolg", groep == "O", "onttrekking", default = "onveranderd")]
U[per_site, on = "site_id", `:=`(poging = TRUE, p_nieuwbouw = i.nieuwbouw, p_sloop = i.sloop, p_verleend = i.verleend,
                                 p_geweigerd = i.geweigerd, p_ingetrokken = i.ingetrokken, p_uitgebreid = i.uitgebreid, p_beroep = i.beroep,
                                 p_eerste = i.eerste, strikt = i.strikt, s_verleend = i.s_verleend, s_geweigerd = i.s_geweigerd,
                                 s_ingetrokken = i.s_ingetrokken, s_uitgebreid = i.s_uitgebreid, s_eerste = i.s_eerste,
                                 verv = i.verv, v_verleend = i.v_verleend, v_geweigerd = i.v_geweigerd, v_ingetrokken = i.v_ingetrokken)]
U[is.na(poging), poging := FALSE]
U[is.na(strikt), strikt := FALSE]
U[is.na(verv), verv := FALSE]
pmaat <- function(d) d[, .(sites = .N, pct_poging = pct(poging), pct_nieuwbouw = pct(poging & p_nieuwbouw %in% TRUE),
                           pct_sloop = pct(poging & p_sloop %in% TRUE),
                           bij_poging_pct_verleend = pct(p_verleend[poging]), bij_poging_pct_geweigerd = pct(p_geweigerd[poging]),
                           bij_poging_pct_ingetrokken = pct(p_ingetrokken[poging]), bij_poging_pct_uitgebreid = pct(p_uitgebreid[poging]),
                           bij_poging_pct_beroep = pct(p_beroep[poging]),
                           pct_strikt = pct(strikt), bij_strikt_pct_verleend = pct(s_verleend[strikt]), bij_strikt_pct_geweigerd = pct(s_geweigerd[strikt]),
                           bij_strikt_pct_ingetrokken = pct(s_ingetrokken[strikt]), bij_strikt_pct_uitgebreid = pct(s_uitgebreid[strikt]),
                           n_vervanging = sum(verv), bij_verv_pct_verleend = pct(v_verleend[verv]), bij_verv_pct_geweigerd = pct(v_geweigerd[verv]),
                           bij_verv_pct_ingetrokken = pct(v_ingetrokken[verv]))]
R$C_groep <- rbind(cbind(scope = "alle", U[, pmaat(.SD), by = groep2]), cbind(scope = "stedelijk", U[stedelijk == TRUE, pmaat(.SD), by = groep2]))
cat("\nC. Pogingen (zaak vanaf juli 2021) per groep van stage 2:\n")
print(R$C_groep[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])
## de strikte pogingen met een vergunning: staat er voor de zaak een nieuw pand in de BAG (zaak_bag), en is dat gestart,
## in gebruik of niet gerealiseerd? Zonder nieuw pand in de BAG is er na de vergunning niets geregistreerd.
zb <- pz[, .(bag_pand = TRUE, bag_start = any(!is.na(start_begin)), bag_gebruik = any(!is.na(gebruik_begin)),
             bag_niet_ger = any(!is.na(niet_gerealiseerd_begin))), by = zaak_id]
sv <- pog[strikt == TRUE & uitkomst %chin% "verleend", .(zaak_id, site_id, zym, verv)]
sv[zb, on = "zaak_id", `:=`(bag_pand = i.bag_pand, bag_start = i.bag_start, bag_gebruik = i.bag_gebruik, bag_niet_ger = i.bag_niet_ger)]
sv[is.na(bag_pand), `:=`(bag_pand = FALSE, bag_start = FALSE, bag_gebruik = FALSE, bag_niet_ger = FALSE)]
sv[U, on = "site_id", groep2 := i.groep2]
R$C_bag <- sv[, .(zaken = .N, pct_bag_pand = pct(bag_pand), pct_gestart = pct(bag_start), pct_in_gebruik = pct(bag_gebruik),
                  pct_niet_gerealiseerd = pct(bag_niet_ger), pct_pand_zonder_start = pct(bag_pand & !bag_start & !bag_gebruik & !bag_niet_ger)),
              by = .(groep2, maat = fifelse(verv, "vervanging", "strikt, geen vervanging"),
                     vergund = fifelse(zym < 202401L, "vergunningzaak 2021 tot en met 2023", "vergunningzaak 2024 tot en met juni 2026"))][order(groep2, maat, vergund)]
cat("
Strikte pogingen met een verleende vergunning: een nieuw pand in de BAG voor de zaak, en de status daarvan:
")
print(R$C_bag[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])
## hoeveel van de echte projecten vangt de maat: de SN-sites naar opleverjaar
R$C_vangst <- U[groep == "SN" & redev_ym >= 202107L, .(sites = .N, pct_poging = pct(poging), pct_strikt = pct(strikt), pct_vervanging = pct(verv)), by = .(jaar = redev_ym %/% 100L)][order(jaar)]
cat("\nSN-sites naar opleverjaar, met een poging in de zaken vanaf juli 2021 (de vangst van de maat):\n"); print(R$C_vangst)
## de onveranderde sites en de SN-sites met oplevering vanaf juli 2021, stedelijk, naar decielen van de inclusive value
D <- U[stedelijk == TRUE & groep2 %chin% c("onveranderd", "SN, opgeleverd vanaf juli 2021") & !is.na(iv)]
D[, deciel := cut(iv, quantile(iv, 0:10 / 10), include.lowest = TRUE, labels = FALSE)]
R$C_iv <- D[, .(sites = .N, iv_min = min(iv), iv_max = max(iv),
                pct_sn = pct(groep2 == "SN, opgeleverd vanaf juli 2021"),
                onveranderd = sum(groep2 == "onveranderd"),
                onv_pct_poging = pct(poging[groep2 == "onveranderd"]),
                onv_pct_verleend_niet_gebouwd = pct((poging & p_verleend %in% TRUE)[groep2 == "onveranderd"]),
                onv_pct_geweigerd_of_ingetrokken = pct((poging & (p_geweigerd %in% TRUE | p_ingetrokken %in% TRUE))[groep2 == "onveranderd"]),
                onv_pct_strikt = pct(strikt[groep2 == "onveranderd"]), onv_pct_strikt_verleend = pct((strikt & s_verleend %in% TRUE)[groep2 == "onveranderd"]),
                een_object = sum(n_obj %in% 1L), onv1_pct_strikt = pct(strikt[groep2 == "onveranderd" & n_obj %in% 1L]),
                sn1_pct = pct((groep2 == "SN, opgeleverd vanaf juli 2021")[n_obj %in% 1L])),
            by = deciel][order(deciel)]
cat("\nStedelijk, naar deciel van de inclusive value (onveranderd en SN met oplevering vanaf juli 2021):\n")
print(R$C_iv[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
## binnen de gemeente en met de sitegrootte als controle: poging (strikt) en realisatie naar deciel, tegen deciel 1
D[, `:=`(y_poging = 100 * strikt, y_sn = 100 * (groep2 == "SN, opgeleverd vanaf juli 2021"), l_opp = log(pmax(site_size, 1)), l_obj = log(pmax(n_obj, 1)))]
dec <- function(y, d) {
  m <- feols(as.formula(sprintf("%s ~ i(deciel, ref = 1) + l_opp + l_obj | gemeente_code", y)), data = d, cluster = ~gemeente_code, notes = FALSE)
  ct <- coeftable(m); ct <- ct[grepl("deciel", rownames(ct)), , drop = FALSE]
  data.table(uitkomst = y, deciel = as.integer(sub(".*::", "", rownames(ct))), b = ct[, 1], se = ct[, 2], p = ct[, 4], n = nobs(m), gemiddeld = mean(d[[y]]))
}
R$C_iv_gemeente <- rbind(dec("y_poging", D[groep2 == "onveranderd"]), dec("y_sn", D))
cat("\nDeciel van de inclusive value tegen deciel 1, binnen de gemeente, met log oppervlakte en log objecten (procentpunt):\n")
print(R$C_iv_gemeente[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
## het jaar van de eerste poging op onveranderde sites: recente pogingen kunnen nog lopen
R$C_onv_jaar <- U[groep2 == "onveranderd" & strikt == TRUE, .(sites = .N, pct_verleend = pct(s_verleend), pct_geweigerd = pct(s_geweigerd),
                                                               pct_ingetrokken = pct(s_ingetrokken)),
                  by = .(jaar = as.integer(format(s_eerste, "%Y")))][order(jaar)]
cat("\nOnveranderde sites met een strikte poging, naar het jaar van de eerste zaak:\n"); print(R$C_onv_jaar)

R$meta <- list(export_econlogic = "perobject_Nederland_20260710", venster_bag = format(VENSTER_BAG), landelijk = format(LANDELIJK),
               eind = format(EIND), gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("pijplijn_sites_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("pijplijn_sites_%s.json", EXTRACT)))
sink()
