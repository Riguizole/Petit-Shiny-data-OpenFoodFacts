# ProduitConso
Petit projet étudiant de création d'une application Shiny représentant 4 appréciations de la base de données choisis d'Open Food Fact. La qualité de code sera toute relative, c'est une démonstration des capacités de visualisation ggplot2.

# Comme lancer ?
Exécuter app.R pour lancer le tableau de bord Shiny. (Cela prend prends du temps la première fois au téléchargements/compilations des données)

# Description fichiers
- **deploy_tables.sh** : Script shell devant télécharger et pré-compiler au format JSON les données vers ces fichiers ultérieurs :
  - **products.json**
  - **nutriments.json**
- **init_json.R** : Importe dans l'environnement R et compile les 4 tableaux de données macro pour les graphiques ggplot ultérieurs.
- **dictionnaire.csv** : Contient les traductions en français des champs textuels (sauf pour la visualisation 4 où c'était trop de travail)
- **app.R** : Contient le cœur de l'application avec le serveur et l'ui de l'application en une page d'accueil et 4 onglets.

# Librairies  utilisés
- shinydashboard
- shinyWidgets
- data.table
- bit64 (initialement pour les code EEA mais non exploité)
- ggplot2
- fmsb
- stringr
- plotly
- jsonlite
- lubridate
