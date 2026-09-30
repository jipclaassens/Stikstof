## =====================================================================
## 28_regels.R -- het plan dat op een datum gold, per punt, voor
##                28_bestemming_sites.R en 27b_econlogic_export.R
##
## Leest de uitvoer van 28a_bestemming.py in de map B (per laag de vlakken
## die een punt raken, met datum en status) en geeft per punt de stand op
## een datum: het vastgestelde vlak van de enkelbestemming met de laatste
## plandatum op of voor die datum (ontwerpen tellen niet; voor de stand van
## nu ook geen vervallen of historische versies). Bouwaanduiding en
## maatvoering tellen alleen als ze bij datzelfde plan horen.
## Verwacht in de aanroepende omgeving: B (de map) en na lees() de lagen
## EB, BA, MV en BV.
## =====================================================================

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
