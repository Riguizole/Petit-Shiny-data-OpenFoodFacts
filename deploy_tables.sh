#!/bin/bash
# Avertissement : la taille entière décompressé au format jsonl du serveur openfoodfacts va être déployer, c'est à dire environ 45 Go en 2024.
cd $(dirname $0)
# Pour les méta-info du fichier qui sera téléchargé :
wget --spider -S https://static.openfoodfacts.org/data/openfoodfacts-products.jsonl.gz 2>&1


# 0 - TÉLÉCHARGEMENT :
wget https://static.openfoodfacts.org/data/openfoodfacts-products.jsonl.gz
gunzip openfoodfacts-products.jsonl.gz

######################
# POUR RÉALISER DES TESTS :
#tail -n 10000 products.jsonl | jq 'select(.completeness > 0.98)' | jq -s '.' > test.json
######################



# 1 - FILTRER SUR LES (lignes) PRODUITS FRANÇAIS :
grep 'en:france' openfoodfacts-products.jsonl > products.jsonl && rm openfoodfacts-products.jsonl


# 2 - CRÉATION (filtrage colonnes) DE LA TABLE products.json
jq '{"id": ._id, completeness, product_name, brands, product_quantity, product_quantity_unit, food_groups, pnns_groups_1, pnns_groups_2, manufacturing_places, emb_codes, nova_groups, nutriscore_grade, ecoscore_grade, "salt": .nutrient_levels.salt, "sugars": .nutrient_levels.sugars, "fat": .nutrient_levels.fat, "saturated-fat": .nutrient_levels."saturated-fat", created_t, last_updated_t, creator, last_modified_by, popularity_key}' products.jsonl | jq -s '.' > products.json


# 3- CRÉATION (filtrage colonnes) DE LA TABLE nutriments.json
# 3.1 - Conservation des branches json "nutriments" (plusieurs milliers) "Categorie" et "id".
jq -c '{id: ._id, Categorie: .categories_hierarchy[-1], nutriments} | select(.nutriments != {})' products.jsonl > nutriments.jsonl && rm products.jsonl
# 3.2 - Conversion de tableau en largeur en un tableau en longueur pour le faire supporter par le moteur R. Voulu à cause de la non uniformité des nutriments et de leur nombre immense.
jq '{id: .id, Categorie} + (.nutriments | to_entries | sort_by(.key) | map(select(.key | endswith("_100g"))) | from_entries)' nutriments.jsonl | jq -c '. as $parent | to_entries[] | { "id": $parent.id, "Categorie": $parent.Categorie, "Nutriment": .key, "Valeur": .value } | select(.Nutriment != "id" and .Nutriment != "Categorie")' | jq -s '.' > nutriments.json && rm nutriments.jsonl