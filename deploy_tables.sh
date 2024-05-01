#!/bin/bash
#cd /media/samuel/SSD_BIS

# RÉALISER DES TESTS :
tail -n 10000 products.jsonl | jq 'select(.completeness > 0.98)' | jq -s '.' > test.json
######################



# Filtre sur les produits francais
grep 'en:france' openfoodfacts-products.jsonl > products.jsonl


# Création table products.json
jq '{"id": ._id, completeness, product_name, brands, product_quantity, product_quantity_unit, food_groups, pnns_groups_1, pnns_groups_2, manufacturing_places, emb_codes, nova_groups, nutriscore_grade, ecoscore_grade, "salt": .nutrient_levels.salt, "sugars": .nutrient_levels.sugars, "fat": .nutrient_levels.fat, "saturated-fat": .nutrient_levels."saturated-fat", created_t, last_updated_t, creator, last_modified_by, popularity_key}' products.jsonl | jq -s '.' > products.json


# Création table nutriments
#tail -n 50 products.jsonl | # pour TEST
jq -c '{id: ._id, Categorie: .categories_hierarchy[-1], nutriments} | select(.nutriments != {})' products.jsonl > nutriments.jsonl
jq '{id: .id, Categorie} + (.nutriments | to_entries | sort_by(.key) | map(select(.key | endswith("_100g"))) | from_entries)' nutriments.jsonl | jq -c '. as $parent | to_entries[] | { "id": $parent.id, "Categorie": $parent.Categorie, "Nutriment": .key, "Valeur": .value } | select(.Nutriment != "id" and .Nutriment != "Categorie")' | jq -s '.' > nutriments.json

#jq '{id: .id, product_name} + (.nutriments | to_entries | sort_by(.key) | map(select(.key | endswith("_100g") or endswith("_unit"))) | from_entries)' nutriments.jsonl | jq -s '.' > nutriments.json


