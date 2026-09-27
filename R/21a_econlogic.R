## =====================================================================
## 21a_econlogic.R -- een slanke afgeleide van de objectexport van het
##                    densification-paper (EconLogicPaper, GeoDMS)
##
## Input : C:/LocalData/Redevelopment/R_werk/perobject_Nederland_20260710.rds
##         (EconLogicPaper/R/02_load_perobject.R; 8,7 miljoen verblijfs-
##         objecten, ook de onveranderde voorraad, met de mutatietypering
##         volgens de CBS-regels uit Claassens, Koomen en Rouwendal 2026)
## Output: _derived/econlogic_vbo.rds
##
## Per object: BAG-nummer van object en pand (als tekst, zestien cijfers),
## mutatietype en -maand, bouwjaar, woonfunctie, woningtype (vrijstaand,
## twee onder een kap, rijtjeswoning, appartement), coördinaten, gemeente, adresdichtheid
## (OAD, CBS-kerncijfers 2012 van de buurt van 2012) en stedelijkheidsklasse
## (0 vanaf 1.500, 1 van 500 tot 1.500, 2 onder 500 adressen per km2), UAI 2012, grondproductie-
## kosten, beschermd stadsgezicht en ligging in Natura 2000. De export
## staat op OVSRV08 (LocalData); pad te zetten met ECONLOGIC_PEROBJECT.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))

bron <- Sys.getenv("ECONLOGIC_PEROBJECT", "C:/LocalData/Redevelopment/R_werk/perobject_Nederland_20260710.rds")
x <- readRDS(bron)
kop("Export gelezen: %s rijen", format(nrow(x), big.mark = "."))
## het BAG-nummer staat in de export als double; tot 2^53 exact, dus via
## sprintf zonder e-notatie naar zestien cijfers
stopifnot(max(x$vbo_bag_nr, na.rm = TRUE) < 2^53)
stopifnot(max(x$pand_bag_nr, na.rm = TRUE) < 2^53)
y <- x[, .(vbo_id = sprintf("%016.0f", vbo_bag_nr), pand_id = sprintf("%016.0f", pand_bag_nr), redev_type = redev_type_lbl, redev_yearmonth,
           bouwjaar = obj_building_year, woon = obj_is_woonfunctie, woningtype = obj_housetype_lbl, x = x_coord, y = y_coord,
           gemeente_code, buurt_code, oad, stedelijkheid = urbanisationk_rel, uai_2012,
           grondprod = loc_grondprod_eur_ha, beschermd = isprotectheritagearea, in_natura2000 = is_natura2000,
           is_sn_door_bbg)]
rm(x); invisible(gc())
## een punt buiten elke CBS-buurt van 2012 krijgt in GeoDMS de uint32-nulwaarde
## 2^32 - 1 als OAD en klasse 255 (EconLogicPaper/R/02_load_perobject.R): leeg
cat("OAD-nulwaarde:", y[oad >= 4294967295, .N], "objecten\n")
y[oad >= 4294967295, `:=`(oad = NA, stedelijkheid = NA)]
## 667 objecten zonder punt hebben dezelfde nulwaarde als coördinaten
cat("coördinaat-nulwaarde:", y[x >= 4294967295 | y >= 4294967295, .N], "objecten\n")
y[x >= 4294967295 | y >= 4294967295, `:=`(x = NA, y = NA)]
print(y[, .N, by = redev_type][order(-N)])
cat("dubbele objecten:", y[, sum(duplicated(vbo_id))], "\n")
saveRDS(y, file.path(PATHS$derived, "econlogic_vbo.rds"))
kop("Geschreven: %s", file.path(PATHS$derived, "econlogic_vbo.rds"))
