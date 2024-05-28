if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(data.table)) install.packages("data.table")
if (!require(bit64)) install.packages("bit64")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(fmsb)) install.packages("fmsb")
if (!require(stringr)) install.packages("stringr")
if (!require(plotly)) install.packages("plotly")

############DONNÉEEEEEEEEEEEEEES######################
if (file.exists(".RData")) {load(".RData")}
if (sum(c("data_visu1", "data_visu2", "data_visu3", "data_visu3_classement", "data_visu4") %in% ls()) != 5) {
  rm(list = ls())
  source("init_json.R")
}
Familles_prod <- unique(data_visu1$pnns_groups_2)
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))
Types_prod <- unique(data_visu4$Categorie)

data_visu1[is.na(data_visu1$pnns_groups_2)]$pnns_groups_2 <- "NA" # Pour éviter les mauvaises manipulations des widget Shiny sur les valeurs manquantes
Familles_prod[is.na(Familles_prod)] <- "NA"

# Pour obtenir les % par valeur de grade/valeur implantés dans le graphe ultérieur
data_visu2 <- data_visu2[order(grade,pnns_groups_1,valeur),
                         .(valeur, produits, "%" = round(100*produits/sum(produits),0), "total" = sum(produits)),
                         by = .(grade, pnns_groups_1)]
# Pour scorer les catégories par meilleurs grades moyens
data_visu2[, qualité := fcase(
  valeur == "a", `%` * 5,
  valeur == "b", `%` * 4,
  valeur == "c", `%` * 3,
  valeur == "d", `%` * 2,
  valeur == "e", `%` * 1,
  valeur == "1", `%` * 4,
  valeur == "2", `%` * 3,
  valeur == "3", `%` * 2,
  valeur == "4", `%` * 1,
  default = 0
)]
data_visu2 <- data_visu2[, .(valeur, produits, `%`, qualité = sum(qualité)), by = .(grade, pnns_groups_1)]
data_visu2 <- data_visu2[order(grade,-qualité,pnns_groups_1,valeur)]
data_visu2[, pnns_groups_1 := factor(pnns_groups_1, levels = unique(pnns_groups_1))]
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
                p("Projet étudiant de création d'un application Shiny représentant 4 appréciations de la base de données choisis d'Open Food Fact."),
                p("L'objectif de ce travail est d'illustrer à travers 4 types de graphiques des grandeurs principales pour apprécier la BDD d'Open Food Facts."),
                p("La donnée générale maillée sur le code EEA du produit (cas en France) a été compilée en 4 tableaux de données macro pour faciliter la conception des graphiques."),
                p(tagList("Le", strong("visuel 1"), "montre au global la complétude des fiches produits (au moins non vide). C'est un listing des produits sur l'axe horizontal allant de la mieux compléter à la pire pour faciliter la compréhension. D'un point de vue extrême, si le carré du graphique est entièrement rempli alors cela signifie que la BDD est totalement renseigné pour tout produits... L'aire en escalier grise correspond à toute famille de produits confondus, l'aire en orange à la sélection choisis plus bas (NA par défaut; i.e. pas de famille connus). Cliquer sur un angle de l'escalier orange ouvre sur internet une fiche aléatoire illustrée par cette valeur de complétude.")),
                p(tagList("Le", strong("visuel 2"), "classe les profils de grades sur 3 scores alimentaires connus : Le Nutri-Score / le nova / l'éco-score. Selon le score choisi à apprécier, les familles (ici moins nombreuses ; i.e. de plus haute hiérarchie) sont ordonnées moyennement des meilleurs scores aux pires scores relevés.")),
                p(tagList("Le", strong("visuel 3"), "dessine une chronologie des fiches créées par familles de produits. Plus bas se trouve le tableau de classement des meilleurs contributeurs d'Open Food Facts (au moins 10 fiches créées). La chronologie peut être filtrée sur un contributeur en relevant son index du tableau dans le champ au milieu de la page.")),
                p(tagList("Le", strong("visuel 4"), "compare les teneurs moyens en nutriments sur 100g par genre de produits. 3 références peuvent être comparées entre elles."))),
            box(title = "Références", width = 12,
                a("Open Food Facts", href = "https://fr.openfoodfacts.org/"), p(""),
                a("Dictionnaire des champs de données", href = "https://wiki.openfoodfacts.org/Data_fields"), p(""),
                a("Application mobile", href = "https://play.google.com/store/apps/details?id=org.openfoodfacts.scanner&hl=fr&gl=US"), p(""),
                a("Open food Prices (sous-projet aux balbutiements d'ajout à la BDD des prix exercés)", href = "https://prices.openfoodfacts.org/fr/")),
            box(title = "Crédits", width = 12,
                p("Conçu par Samuel Courteille et Ilyes Sall"),
                p("Dans le cadre d'un projet étudiant pour le master SIAD Villeneuve d'Ascq.")
                )
    ),
    tabItem(tabName = "tabVisu1",
            fluidRow(valueBoxOutput(outputId = "total_produits", width = 6),
                     valueBoxOutput(outputId = "completude", width = 6)),
            fluidRow(valueBoxOutput(outputId = "super_total_produits", width = 6),
                     valueBoxOutput(outputId = "super_completude", width = 6)),
            fluidRow(box(title = "Profil de la complétude des fiches des meilleurs aux pires", width = 12,
                            plotlyOutput(outputId = "visu1"),
                            radioGroupButtons(inputId = "pnns_groups_2",
                                              label = "Familles de produits :",
                                              choices = Familles_prod,
                                              individual = TRUE,
                                              selected = "NA",
                                              checkIcon = list(yes = tags$i(class = "fa fa-circle", 
                                                                            style = "color: #99510C"),
                                                               no = tags$i(class = "fa fa-circle-o", 
                                                                           style = "color: #FF8714"))
                                              )
                           )
                    ),
            fluidRow(
              
              
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
  
  # Visuel 1 --------------------------------------------------------------
  
  output$total_produits <- renderValueBox({
    total_produits <- max(data_visu1_filtre()$produits)
    valueBox(value = total_produits, 
             subtitle = paste("Nb produits",input$pnns_groups_2),
             icon = icon("barcode"))
  })
  
  output$completude <- renderValueBox({
    completude <- sum(data_visu1_filtre()$complétude * (data_visu1_filtre()$seg_produits/max(data_visu1_filtre()$produits)))
    completude <- sprintf("%.0f%%", round(completude*100))  # Formater le pourcentage
    valueBox(value = completude, 
             subtitle = paste("Complétude des fiches",input$pnns_groups_2),
             icon = icon("check"))
  })
  
  output$super_total_produits <- renderValueBox({
    total_produits <- sum(data_visu1$produits)
    valueBox(value = total_produits, 
             subtitle = "Nb produits dans la BDD",
             icon = icon("database"))
  })
  
  output$super_completude <- renderValueBox({
    completude <- sum(data_visu1$completeness * (data_visu1$produits/sum(data_visu1$produits)))
    completude <- sprintf("%.0f%%", round(completude*100))  # Formater le pourcentage
    valueBox(value = completude, 
             subtitle = "Complétude générale des fiches",
             icon = icon("circle-check"))
  })
  
  data_visu1_filtre <- reactive({
    data <- data_visu1[pnns_groups_2 %in% input$pnns_groups_2]
    data$seg_produits <- data$produits
    data$produits <- cumsum(data$produits)
    data <- data[, .("produits" = max(produits), seg_produits = sum(seg_produits)), by = .(completeness)]
    data <- data[, .(produits, "complétude" = completeness, seg_produits)]
    
    data <- rbind(data[1],data) # Astuce
    data[1]$produits <- 0       # pour forcer l'escalier à commencer en 0
    return(data)
  })
  
  data_visu1_tout <- reactive({
    tot <- sum(data_visu1[pnns_groups_2 %in% input$pnns_groups_2]$produits)
    data <- data_visu1
    data$produits <- cumsum(data$produits)
    data <- data[, .("produits" = max(produits)), by = .(completeness)]
    data <- data[, .(produits, "complétude" = completeness)]
    data$produits <- tot * (data$produits / max(data$produits))
    
    data <- rbind(data[1],data) # Astuce
    data[1]$produits <- 0       # pour forcer l'escalier à commencer en 0
    return(data)
  }) # pour le profil de répartition complétude toutes familles produits confondus
  
  output$visu1 <- renderPlotly({
    p <- ggplot(data_visu1_tout(), aes(y = complétude, x = produits)) +
      geom_area(alpha = 0.8, aes(text = NULL)) +
      geom_area(data = data_visu1_filtre(), fill = "#FF8714",color="#99510C", alpha = 0.6, aes(text = NULL)) +
      geom_point(data = data_visu1_filtre(), color = NA) +
      labs(x = "Produits", y = "Complétude", title = "Le profil de complétude de la famille choisis est en orange\n (gris correspond à toutes familles confondus)") +
      theme_minimal() + ylim(0, 1) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    
    style(ggplotly(p), hoverinfo = "skip", traces = 1:2)
  })
  
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    browseURL(sample(data_visu1[completeness == click_data[["y"]] & pnns_groups_2 %in% input$pnns_groups_2, .(url)]$url,1))
  })
  
  # Visuel 2 --------------------------------------------------------------
  
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
          geom_text(aes(label = paste0(`%`, "%")), 
              position = position_stack(vjust = 0.5), 
              fontface = "bold") +
      facet_wrap(vars(pnns_groups_1), scales = "free") +
      scale_fill_manual(values = c(`1` = "#00AA00", `2` = "#FFCC00", `3` = "#FF6600", `4` = "#FF0000", a = "#118D51", b = "#7DC244", c = "#FEC927", d = "#F78124", e = "#ED5223")) +
      labs(x = str_extract(input$grade,".*(?=_)"), y = "Produits") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  # Visuel 3 ---- ---------------------------------------------------------
  
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
  
  # Visuel 4 --------------------------------------------------------------
  
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
