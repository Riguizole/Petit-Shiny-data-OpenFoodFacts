if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(data.table)) install.packages("data.table")
if (!require(bit64)) install.packages("bit64")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(fmsb)) install.packages("fmsb")
if (!require(stringr)) install.packages("stringr")

############DONNÉEEEEEEEEEEEEEES######################
load(".RData")
if (sum(c("data_visu1", "data_visu2", "data_visu3", "data_visu3_classement", "data_visu4") %in% ls()) != 5) {
  rm(list = ls())
  source("init_json.R")
}
Familles_prod <- unique(data_visu1$pnns_groups_2)
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))
Types_prod <- unique(data_visu4$Categorie)
####################################################|#



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Accueil",
             icon = icon("barcode", lib = "glyphicon"),
             tabName = "tabDashboard",
             selected = TRUE),
    menuItem(text = " Visuel 1",
             icon = icon("th-list", lib = "glyphicon"),
             tabName = "tabVisu1"),
    menuItem(text = "Visuel 2",
             icon = icon("dashboard", lib = "glyphicon"),
             tabName = "tabVisu2"),
    menuItem(text = "Visuel 3",
             icon = icon("user", lib = "glyphicon"),
             tabName = "tabVisu3"),
    menuItem(text = "Visuel 4",
             icon = icon("grain", lib = "glyphicon"),
             tabName = "tabVisu4")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tabDashboard",
            box(title = "Description du projet", width = 12,
                p("Le projet collaboratif français Open Food Facts fournit une base de données libre et ouverte sur les produits alimentaires commercialisés dans le monde entier. La philosophie de cette organisation est qu'œuvrer pour la transparence des données des produits alimentaires est un acte citoyen. La base de données accumule depuis les années 2012 plus de 1 million de fiches produits. Du point de vue étudiant, ce sujet data semble très pertinent avec la structuration autour du code-barres EEA. Le code EEA est régulé pour être le code d'identification unique de chaque produit en principe. En effet, sans clé unique identification unique, pas de super base de données possible (i.e.: SIRET pour les entreprises / NIR pour les citoyens français / ISBN pour les livres / VIN pour les véhicules / Référence cadastrale pour la propriété territoriale / ...)."),
                p("L'objectif de ce travail est d'illustrer à travers 4 types de graphiques des grandeurs principales pour apprécier la BDD de Open Food Facts. Ensuite les graphiques construits dans ce notebook seront intégrer à un tableau de bord Shiny dédié à un utilisateur final. Compte tenu de cette attente restreinte, les données exploitées pour faire tourner ce code seront compilées non plus à la maille d'un produit (identifié par un EEA) mais en 4 tableaux agrégés. La portabilité de ce projet en pièce zippé sera plus d'autant facile. En premier lieu du notebook, nous retracerons succinctement les étapes de traitement pour la compilation des données."),
                p("Nos visualisations étayeront les normes alimentaires en échelle macro selon un rendement décroissant qui est lié à la qualité des données présentes. ")),
            box(title = "Références", width = 12,
                a("Open Food Facts", href = "https://fr.openfoodfacts.org/"), p(""),
                a("Dictionnaire des champs de données", href = "https://wiki.openfoodfacts.org/Data_fields"), p(""),
                a("Application mobile", href = "https://play.google.com/store/apps/details?id=org.openfoodfacts.scanner&hl=fr&gl=US"), p(""),
                a("Open food Prices (sous-projet aux balbutiements d'ajout à la BDD des prix exercés)", href = "https://prices.openfoodfacts.org/fr/")),
            box(title = "Crédits", width = 12,
                p("Cette application a été développée par Samuel Courteille & Ilyes Sall."),
                p("Dans le cadre du cours de Datavisualisation encadré par Nadarajen Veerapen au master SIAD.")
                )
    ),
    tabItem(tabName = "tabVisu1",
            fluidRow(
              box(title = "Complétude des fiches produits", width = 12,
                plotOutput(outputId = "visu1"))
            ),
            fluidRow(
              box(title = "Sélection", width = 10,
                pickerInput(inputId = "pnns_groups_2",
                            label = "Famille de produits",
                            choices = Familles_prod,
                            selected = Familles_prod,
                            multiple = TRUE,
                            options = list(size = 5, `actions-box` = T, dropupAuto = T))  # Modifier le nombre en fonction de votre préférence
                ),
              valueBoxOutput(outputId = "completude", width = 2)
            )
    ),
    tabItem(tabName = "tabVisu2",
            radioButtons(inputId = "grade",
                         label = "Type de score",
                         choices = c("Nutriscore" = "nutriscore_grade",
                                     "NOVA" = "nova_groups",
                                     "Éco-Score" = "ecoscore_grade"),
                         selected = "nutriscore_grade"),
            box(title = "Distribution des scores par familles de produits", width = 12,
                plotOutput(outputId = "visu2"))
    ),
    tabItem(tabName = "tabVisu3",
            box(title = "Chronologie de l'enrichissement de la base", width = 12,
                plotOutput(outputId = "visu3")),
            numericInput(inputId = "créateur",
                         label = "Choix du créateur de fiches",
                         value = 0, min = 0, max = nrow(data_visu3_classement), step = 1),
            box(title = "Classement", width = 12,
                tableOutput(outputId = "classement"))
    ),
    tabItem(tabName = "tabVisu4",
              column(width = 4,
                     box(selectInput(inputId = "prod1",
                                     label = "1er produit",
                                     choices = Types_prod,
                                     selected = "en:white-hams"),
                         background = "red")),
              column(width = 4,
                     box(selectInput(inputId = "prod2",
                                     label = "2ème produit",
                                     choices = Types_prod,
                                     selected = "en:crackers"),
                         background = "green")),
              column(width = 4,
                     box(selectInput(inputId = "prod3",
                                     label = "3ème produit",
                                     choices = Types_prod,
                                     selected = "en:strawberry-jams"),
                         background = "blue")),
            box(title = "Profils de nutriments par type de produits sur 100g consommés", width = 8,
                plotOutput(outputId = "visu4")),
            box(title = "Valeurs", width = 12,
                tableOutput(outputId = "nutriments"))
            
    )
  )
)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "OpenFoodFact"),
  sidebar,
  body
)

server <- function(input, output) {
  
  # Visuel 1
  
  data_visu1_filtre <- reactive({
    data_visu1[pnns_groups_2 %in% input$pnns_groups_2]
  })
  
  output$visu1 <- renderPlot({
    ggplot(data_visu1_filtre(), aes(y = completeness, x = cumsum(produits))) +
      geom_area(fill = "#FF8714",color="#99510C",alpha = 0.8) +
      labs(x = "Produits", y = "Complétude", title = "Répartition de la complétude des données produits") +
      theme_minimal() + ylim(0,1)
  })
  
  output$completude <- renderValueBox({
    completude <- sum(data_visu1_filtre()$surface) / sum(data_visu1_filtre()$produits)
    completude <- sprintf("%.0f%%", round(completude*100))  # Formater le pourcentage
    valueBox(value = completude, 
             subtitle = "Complétude de la données fiche produits",
             icon = icon("database"))
  })
  
  # Visuel 2
  
  data_visu2_filtre <- reactive({
    if (input$grade == "nutriscore_grade") {
      data_visu2[grade %in% input$grade & !is.na(pnns_groups_1) & pnns_groups_1 != "Boissons alcoolisées" & !is.na(valeur)]
    } else {
      data_visu2[grade == input$grade & !is.na(pnns_groups_1) & !is.na(valeur)]
    }
  })
  
  output$visu2 <- renderPlot({
    ggplot(data_visu2_filtre(), aes(x = valeur, y = produits, fill = valeur)) +
      geom_bar(stat = "identity") +
      facet_wrap(vars(pnns_groups_1), scales = "free") +
      scale_fill_manual(values = c(`1` = "#00AA00", `2` = "#FFCC00", `3` = "#FF6600", `4` = "#FF0000", a = "#118D51", b = "#7DC244", c = "#FEC927", d = "#F78124", e = "#ED5223")) +
      labs(x = str_extract(input$grade,".*(?=_)"), y = "Produits") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  # Visuel 3
  
  data_visu3_filtre <- reactive({
    if (input$créateur == 0) {
      data_visu3_ <- data_visu3[, .(produits = sum(produits)), by = .(période, Catégories)]
    } else {
      data_visu3_ <- data_visu3[Créateurs == data_visu3_classement$Créateurs[input$créateur], .(produits = sum(produits)), by = .(période, Catégories)]
    }
    data_visu3_ <- data_visu3_[, .(période, produits = cumsum(produits)), by = .(Catégories)]
    top_date <- max(data_visu3_$période)
    top <- data_visu3_[, .(période = top_date, produits = max(produits)), by = .(Catégories)]
    data_visu3_ <- rbind(data_visu3_,top, fill = T)
    setorder(data_visu3_, période, Catégories, -produits)
  })
  
  output$visu3 <- renderPlot({
    ggplot(data_visu3_filtre(), aes(x = période)) +
      geom_area(aes(y = produits, fill=Catégories),alpha = 0.8) +
      labs(x = "Date", y = "Produits", title = paste("Contribution totales en créations de fiches",data_visu3_classement$Créateurs[input$créateur])) +
      theme_minimal() + theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$classement <- renderTable({
    data_visu3_classement
  }, rownames = T)
  
  # Visuel 4
  
  data_visu4_filtre <- reactive({
    data_visu4_ <-  dcast(data_visu4[Categorie %in% c(input$prod1, input$prod2, input$prod3)], 
                          Categorie ~ Nutriment, value.var = "Valeur", fill = 0)
    data_visu4_[, Categorie:=NULL]
    data_visu4_ <- rbind(t(ceiling(apply(data_visu4_, 2, max)/5)*5),t(apply(data_visu4_, 2, function(x) 0)),data_visu4_)
  })
  
  output$visu4 <- renderPlot({
    radarchart(title = "unités en g", data_visu4_filtre(), axistype = 2, axislabcol = "black",
               cglty = 1,
               cglcol = "gray",
               pcol = 2:4,
               plwd = 2,
               plty = 1,
               pfcol = areas)
  })
  
  output$nutriments <- renderTable({
    if (!is.null(input$prod3) & !is.null(input$prod2) & !is.null(input$prod1)) {
      nutriments_table <- data_visu4_filtre()
      nutriments_table$`Type de produit` <- c("max", "min", str_extract(c(input$prod1, input$prod2, input$prod3), "(?<=:).*"))
      setcolorder(nutriments_table, c("Type de produit",.SD))
      return(nutriments_table)
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
