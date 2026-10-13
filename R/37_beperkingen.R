## =====================================================================
## 37_beperkingen.R -- is het land nabij natuur om andere redenen minder
##                     bebouwbaar, en verklaart dat de achterstand?
##
## Input : _derived/vbo_analyse.rds, _derived/vbo_xy.csv,
##         _derived/vbo_beperking.csv en _derived/beperking_oppervlakte.csv
##         (37a_beperkingen.py)
## Output: _output/beperkingen_<extract>.json en .txt
##
## Waarom
## ---------------------------------------------------------------------
## De opleveringen en vergunningen liggen nabij natuur na 2019 lager dan
## verderop in dezelfde gemeente (hoofdstuk 8 en 15), zonder knik bij de
## uitspraak. Nabij natuur gelden ook andere beperkingen voor bouwen: het
## Natuurnetwerk, stiltegebieden en grondwaterbescherming, landschap, de
## rijksregels voor water en kust, externe veiligheid, Defensie, geluid
## van luchthavens, infrastructuur en erfgoed (negen groepen van
## 37a_beperkingen.py), en hoogtegrenzen rond radars, luchthavens en
## laagvliegroutes (de tiende groep). Komt de achterstand daaruit, dan
## moet ze in de gebieden met zo'n beperking zitten en op plekken zonder
## beperking kleiner zijn of verdwijnen.
##   1. oppervlakte: welk deel van het land nabij en ver onder elke groep
##      valt, binnen en buiten de verstedelijkingscontour;
##   2. objecten: welk deel van de opleveringen en vergunningen in elke
##      groep ligt, nabij en ver, voor en na 2019Q2;
##   3. de DiD na 2019Q2 van 21_verdichting.R (Poisson op eenheid x nabij
##      x kwartaal, vaste effecten eenheid x nabij, eenheid x kwartaal en
##      nabij x seizoen, standaardfouten geclusterd op de eenheid): alle
##      objecten, alleen plekken zonder beperking, alleen plekken met een
##      beperking, binnen gemeente x beperkt, en zonder elke groep apart;
##      met een term na x beperkt (of na x hoogte) voor het verschil.
## De samengestelde maten ("een of meer", "strenger") tellen de hoogte-
## groep niet mee: een hoogtegrens verbiedt bouwen niet, ze begrenst het.
## De hoogtegroep heeft haar eigen vergelijking, vooral voor grote
## projecten. De beperkingen zijn de huidige stand (2018 tot 2026, per
## laag in de BRONNEN.md van bron\restricties en bron\rs_restricties):
## een vaste eigenschap van de plek, geen behandeling in de tijd.
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

sink(file.path(PATHS$output, sprintf("beperkingen_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
GROEPEN <- c("natuurnetwerk", "stilte en grondwater", "landschap", "water en kust", "externe veiligheid", "defensie",
             "geluid en luchtvaart", "infrastructuur", "erfgoed", "hoogte")
in_groep <- function(b, g) !is.na(b) & b > 0L & bitwAnd(b, bitwShiftL(1L, g - 1L)) > 0L
of_groep <- function(b, gg) Reduce(`|`, lapply(gg, function(g) in_groep(b, g)))
H <- match("hoogte", GROEPEN)
## "een of meer": de negen groepen zonder de hoogte. "Strenger": ook zonder landschap en erfgoed, twee kwaliteitsregimes
## waarin bouwen met voorwaarden kan; de overige zeven verbieden bouwen of binden het aan een vergunning of afweging.
ALLE <- setdiff(seq_along(GROEPEN), H)
STRENG <- setdiff(ALLE, match(c("landschap", "erfgoed"), GROEPEN))
een <- function(b) of_groep(b, ALLE)
streng <- function(b) of_groep(b, STRENG)

KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
Q_PAS <- kw_index(as.Date("2019-05-29"))
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))

## ---------------------------------------------------------------------
## 1. Oppervlakte van het land onder elke groep, nabij en ver
## ---------------------------------------------------------------------
o <- fread(file.path(PATHS$derived, "beperking_oppervlakte.csv"), sep = ";")
o <- rbind(o, copy(o)[, contour := 2L])            # 2 = binnen en buiten samen
opp_groep <- function(sel, naam) o[, .(groep = naam, ha = sum(ha), ha_groep = sum(ha[sel(beperking)])), by = .(kant, contour)]
R$oppervlakte <- rbindlist(c(lapply(seq_along(GROEPEN), function(g) opp_groep(function(b) in_groep(b, g), GROEPEN[g])),
                             list(opp_groep(een, "een of meer"), opp_groep(streng, "een of meer, zonder landschap en erfgoed"))))
R$oppervlakte[, `:=`(contour = c("buiten de contour", "binnen de contour", "alle land")[contour + 1L], aandeel = 100 * ha_groep / ha)]
kop("1. Aandeel van het land onder elke groep (procent), nabij (tot 2.500 m) en ver van habitat, en in het habitat zelf")
print(dcast(R$oppervlakte, groep ~ contour + kant, value.var = "aandeel")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
print(R$oppervlakte[groep == "een of meer", .(kant, contour, ha = round(ha))])

## ---------------------------------------------------------------------
## 2. De objecten
## ---------------------------------------------------------------------
d <- read_derived("vbo_analyse")
xy <- merge(fread(file.path(PATHS$derived, "vbo_xy.csv"), sep = ";"), fread(file.path(PATHS$derived, "vbo_beperking.csv"), sep = ";"), by = "id")
d <- merge(d, xy[, !"id"], by = c("x", "y"), all.x = TRUE, sort = FALSE)
s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)
R$meta <- list(extract = EXTRACT, gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"), n = lapply(UIT, nrow),
               met_beperking_bekend = lapply(UIT, function(x) mean(!is.na(x$beperking) & x$beperking >= 0L)))
kop("2. Objecten in elke groep (procent), nabij en ver, voor en na 2019Q2")
print(R$meta)
R$objecten <- rbindlist(lapply(names(UIT), function(u) {
  x <- UIT[[u]][!is.na(beperking) & beperking >= 0L]
  x[, `:=`(kant = fifelse(dist2n2000_m <= 2500, "nabij", "ver"), periode = fifelse(q >= Q_PAS, "na", "voor"))]
  rbindlist(c(lapply(seq_along(GROEPEN), function(g) x[, .(uitkomst = u, groep = GROEPEN[g], objecten = .N, aandeel = 100 * mean(in_groep(beperking, g))),
                                                     by = .(kant, periode)]),
              list(x[, .(uitkomst = u, groep = "een of meer", objecten = .N, aandeel = 100 * mean(een(beperking))), by = .(kant, periode)],
                   x[, .(uitkomst = u, groep = "een of meer, zonder landschap en erfgoed", objecten = .N, aandeel = 100 * mean(streng(beperking))),
                     by = .(kant, periode)])))
}))
print(dcast(R$objecten, uitkomst + groep ~ kant + periode, value.var = "aandeel")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

## ---------------------------------------------------------------------
## 3. Nabij tegenover ver na 2019Q2, op plekken met en zonder beperking
## ---------------------------------------------------------------------
paneel <- function(dt, eenheid) {
  dt <- dt[!is.na(gemeentecode)]
  setorder(dt, projectid, q, afstandsklasse)
  pp <- dt[, .(E = get(eenheid)[1L], G = as.integer(dist2n2000_m[1L] <= 2500), n = .N), by = .(projectid, q)]
  cel <- pp[, .(y = sum(n)), by = .(E, G, t = q)]
  vol <- CJ(E = sort(unique(cel$E)), G = 0:1, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("E", "G", "t"), all.x = TRUE); p[is.na(y), y := 0]
  p[, `:=`(E_G = paste(E, G), E_t = paste(E, t), G_szn = paste(G, (t - 1L) %% 4L), tr = G * as.integer(t >= Q_PAS))]
  p[]
}
did <- function(p) {
  m <- tryCatch(fepois(y ~ tr | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200), error = function(e) NULL)
  if (is.null(m) || !"tr" %in% names(coef(m))) return(data.table(b = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_, geconvergeerd = NA))
  ct <- coeftable(m)
  data.table(b = ct["tr", 1], se = ct["tr", 2], p = ct["tr", 4], n = nobs(m), geconvergeerd = m$convStatus)
}
maak <- function(dt, om) {
  dt <- dt[!is.na(beperking) & beperking >= 0L]
  if (om == "20 of meer") dt <- dt[projectomvang >= 20L]
  copy(dt)[, `:=`(gm = as.character(gemeentecode), gm_b = paste(gemeentecode, een(beperking)),
                  gm_h = paste(gemeentecode, in_groep(beperking, H)))]
}
DELEN <- c(list("alle objecten" = quote(TRUE), "zonder beperking" = quote(!een(beperking)), "met een beperking" = quote(een(beperking)),
                "zonder strenge beperking" = quote(!streng(beperking)), "met een strenge beperking" = quote(streng(beperking)),
                "met een hoogtegrens" = bquote(in_groep(beperking, .(H)))),
           setNames(lapply(seq_along(GROEPEN), function(g) bquote(!in_groep(beperking, .(g)))), paste("zonder", GROEPEN)))
kop("3. Nabij tegenover ver na 2019Q2 binnen de gemeente, per deel, en binnen gemeente x beperkt of x hoogte")
R$did <- rbindlist(lapply(names(UIT), function(u) rbindlist(lapply(c("alle", "20 of meer"), function(om) {
  dt <- maak(UIT[[u]], om)
  a <- rbindlist(lapply(names(DELEN), function(dn) {
    x <- dt[eval(DELEN[[dn]])]
    cbind(uitkomst = u, projecten = om, deel = dn, vergelijking = "binnen de gemeente",
          objecten_nabij = x[dist2n2000_m <= 2500, .N], objecten = nrow(x), did(paneel(x, "gm")))
  }))
  b <- rbindlist(lapply(c("binnen gemeente x beperkt" = "gm_b", "binnen gemeente x hoogte" = "gm_h"), function(e)
    cbind(uitkomst = u, projecten = om, deel = "alle objecten", objecten_nabij = dt[dist2n2000_m <= 2500, .N], objecten = nrow(dt),
          did(paneel(dt, e)))), idcol = "vergelijking")
  rbind(a, b, use.names = TRUE)
}))))
R$did[, `:=`(pct = 100 * (exp(b) - 1), pct_laag = 100 * (exp(b - 1.96 * se) - 1), pct_hoog = 100 * (exp(b + 1.96 * se) - 1))]
print(R$did[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)], nrows = 200)

## verschilt de achterstand tussen plekken met en zonder beperking, of met en zonder hoogtegrens? Een term na x beperkt
kop("3b. Verschil in de achterstand tussen plekken met en zonder beperking of hoogtegrens (term na x beperkt of na x hoogte)")
R$verschil <- rbindlist(lapply(c(beperking = "gm_b", hoogtegrens = "gm_h"), function(e) rbindlist(lapply(names(UIT), function(u)
  rbindlist(lapply(c("alle", "20 of meer"), function(om) {
    p <- paneel(maak(UIT[[u]], om), e)
    p[, tr_b := tr * as.integer(grepl("TRUE$", E))]
    m <- fepois(y ~ tr + tr_b | E_G + E_t + G_szn, data = p, cluster = ~E, notes = FALSE, glm.iter = 200)
    ct <- coeftable(m)
    data.table(uitkomst = u, projecten = om, term = c("zonder", "met min zonder"),
               b = ct[c("tr", "tr_b"), 1], se = ct[c("tr", "tr_b"), 2], p = ct[c("tr", "tr_b"), 4], n = nobs(m))
  }))))), idcol = "split")
print(R$verschil[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("beperkingen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("beperkingen_%s.json", EXTRACT)))
sink()
