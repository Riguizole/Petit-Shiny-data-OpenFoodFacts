# ----- Table product -----
product <- fromJSON("/media/samuel/SSD_BIS/products.json")
product <- as.data.table(product)
colnames(product)[1] <- "id"
product[,id:=as.integer64(id)]

# Général
product <- product[completeness > 0]
product <- product[product_name != ""]
product <- product[!is.na(product_name)]
product[completeness > 1, completeness := 1]

product[ecoscore_grade %in% c("unknown", "not-applicable"), ecoscore_grade := NA]
product[nutriscore_grade %in% c("unknown", "not-applicable"), nutriscore_grade := NA]
product[pnns_groups_1 == "unknown", pnns_groups_1 := NA]
product[pnns_groups_2 == "unknown", pnns_groups_2 := NA]


product[,created_t:=as.POSIXct(created_t, origin = "1970-01-01")]
product[,last_updated_t:=as.POSIXct(last_updated_t, origin = "1970-01-01")]

# Traductions
dict <- fread("dictionnaire")
setkey(dict, en)
product[dict[champs == "pnns_groups_1"], pnns_groups_1 := i.fr, on = .(pnns_groups_1 = en)]
product[dict[champs == "pnns_groups_2"], pnns_groups_2 := i.fr, on = .(pnns_groups_2 = en)]

# Pour visu 1
setorder(product, -completeness)
data_visu1 <- product[, .(completeness, pnns_groups_2)]
data_visu1 <- data_visu1[, .(produits = .N, surface = .N*completeness), by = .(completeness, pnns_groups_2)]

# Pour visu 2
data_visu2 <- melt(product[, .(pnns_groups_1, nutriscore_grade, nova_groups, ecoscore_grade)],
                    measure.vars = c("nutriscore_grade", "nova_groups", "ecoscore_grade"),
                    variable.name = "grade",
                    value.name = "valeur")
data_visu2 <- data_visu2[, .(produits = .N), by = .(pnns_groups_1, grade, valeur)]

# Pour visu 3
data_visu3 <- product[, .(période = ceiling_date(product$created_t,unit="month"), pnns_groups_1, creator)]
data_visu3 <- data_visu3[, .(produits = .N), by = .(période, Catégories = pnns_groups_1, Créateurs = creator)]
setorder(data_visu3, période, Catégories, -produits)

data_visu3_classement <- data_visu3[, .(produits = sum(produits)), by = .(Créateurs, Catégories)]
data_visu3_classement <- dcast(data_visu3_classement, Créateurs ~ Catégories, value.var = "produits", fill = 0)
data_visu3_classement[, Total := rowSums(.SD), .SDcols = -1]
setcolorder(data_visu3_classement, c("Créateurs","Total", .SD))
setorder(data_visu3_classement, -Total)
data_visu3_classement <- data_visu3_classement[Total > 9] # Par soucis de vitesse de lecture shiny

###
rm(product)
gc()
###

# ----- Table nutriments -----
nutriments <- fromJSON("/media/samuel/SSD_BIS/nutriments.json")
nutriments <- as.data.table(nutriments)
nutriments[,id:=as.integer64(id)]

# Pour visu 4
data_visu4 <- nutriments[!is.na(Categorie), .("Nb produits" = .N, Valeur = mean(Valeur)), by = .(Categorie, Nutriment)]
data_visu4 <- data_visu4[, .(Nutriment, `Nb produits`, Categorie_NB = max(`Nb produits`), Valeur), by = .(Categorie)]
setorder(data_visu4, -Categorie_NB, -`Nb produits`)
# Il y a un gros ménage requis sur la colonne Nutriment. L'organisation OpenFoodFact n'a pas normalisé de dictionnaire des nutriments...
data_visu4 <- data_visu4[`Nb produits`/Categorie_NB > 0.5 & `Nb produits` > 100] # On s'impose des aggrégats dénombrant au moins la moitié du total de la catégorie pour s'épargner des effets de bords sur l'impropreté de la BDD côté nutriments. Et au moins 30 produits.

# Nettoyage des non nutriments
data_visu4 <- data_visu4[!(Nutriment %in% c("energy_100g",
                              "energy-kcal_100g",
                              "nutrition-score-fr_100g",
                              "fruits-vegetables-legumes-estimate-from-ingredients_100g",
                              "fruits-vegetables-nuts-estimate-from-ingredients_100g",
                              "nova-group_100g",
                              "carbon-footprint-from-known-ingredients_100g",
                              "energy-kj_100g",
                              "fruits-vegetables-nuts-estimate_100g",
                              "carbon-footprint-from-meat-or-fish_100g",
                              "collagen-meat-protein-ratio_100g",
                              "fruits-vegetables-nuts_100g",
                              "nutrition-score-fr-producer_100g",
                              "carbon-footprint_100g"))]
data_visu4[dict[champs == "Nutriment"], Nutriment := i.fr, on = .(Nutriment = en)]

###
rm(nutriments, dict)
gc()


# ----- Sauvegarde workspace -----
save.image("~/Nextcloud/🐇 SIAD 2023/Dataviz/Projet final dataviz/.RData")
