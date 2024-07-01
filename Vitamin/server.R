library(shiny)
library(readxl)
library(readODS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)


# Define server logic required to draw a histogram
function(input, output, session) {

  
  ##### ETAPE 1 IMPORT DES DONNEES #####  
  
  observeEvent(input$recharge, {    
    
    
    print("Actualisation des données...")
    
    
    # Initialiser table_anova
    table_anova <- data.frame(Enquete = factor(),
                              Type_Enquete = factor(),
                              DEM=factor(),
                              Année = integer(),
                              Region=factor(),
                              Zone=factor(),
                              Nord_Sud=factor(),
                              Taux_collecte = numeric(),
                              Taux_dechet = numeric(),
                              Taux_hors_champ = numeric(),
                              FA=integer())
    
    
    for (database in c("EEC", "SRCV", "LC", "TIC", "Camme","Autres")) {
      
      
      print(database)
      Res_Enquete <- data.frame()
      
      if (database == "EEC") {
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\EEC 2007-2023\\harmonise"
        setwd(directory)
        
        for (year in 2007:2023) {
          print(year)
          file_path <- paste0("Result EEC ", year, ".ods")
          
          if (year %in% 2007:2017) {
            data <- read_ods(file_path, skip = 1, col_names = TRUE)
          } else if (year %in% 2018:2023) {
            data <- read_ods(file_path, skip = 3, col_names = TRUE)
          }
          
          if (year %in% 2007:2016) {
            df_annee <- data.frame(
              DEM = data$Région,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
              Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
              FA = as.integer(data$`TOTAL F.A.`)
            )
          } else {
            df_annee <- data.frame(
              DEM = data$`Region-Etablissement`,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte`),
              Taux_dechet = as.numeric(data$`Taux de déchets`),
              FA = as.integer(data$`Total FA`)
            )
          }
          
          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(database, each = nrow(data))
          Res_Enquete <- rbind(Res_Enquete, df_annee)
        }
      }
      
      if (database == "SRCV") {
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\SRCV 2007-2023\\harmonise"
        setwd(directory)
        
        for (year in 2007:2023) {
          print(year)
          file_path <- paste0("Result SRCV ", year, ".xls")
          
          if (year <= 2017) {
            data <- read_excel(file_path, skip = 1, col_names = TRUE, n_max = 23)
            df_annee <- data.frame(
              DEM = data$Région,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
              Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
              FA = as.integer(data$`TOTAL F.A.`)
            )
          } else {
            data <- read_excel(file_path, skip = 0, col_names = TRUE)
            df_annee <- data.frame(
              DEM = data$`Region-Etablissement`,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte`),
              Taux_dechet = as.numeric(data$`Taux de déchets`),
              FA = as.integer(data$`Total FA`)
            )
          }
          
          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(database, each = nrow(data))
          Res_Enquete <- rbind(Res_Enquete, df_annee)
        }
      }
      
      if (database == "LC") {
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\L&C 2007-2023\\harmonise"
        setwd(directory)
        
        for (year in 2007:2023) {
          print(year)
          file_path <- paste0("Result LC ", year, ".xls")
          data <- read_excel(file_path, skip = 1, col_names = TRUE)
          
          if (year <= 2017) {
            df_annee <- data.frame(
              DEM = data$Région,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
              Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
              FA = as.integer(data$`TOTAL F.A.`)
            )
          } else {
            df_annee <- data.frame(
              DEM = data$`Region-Etablissement`,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte`),
              Taux_dechet = as.numeric(data$`Taux de déchets`),
              FA = as.integer(data$`Total FA`)
            )
          }
          
          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(database, each = nrow(data))
          Res_Enquete <- rbind(Res_Enquete, df_annee)
        }
      }
      
      if (database == "TIC") {
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\TIC 2007-2023\\harmonise"
        setwd(directory)
        
        for (year in c(2007:2019, 2021:2023)) {
          print(year)
          file_path <- paste0("Result TIC ", year, ".xls")
          
          if (year <= 2017) {
            data <- read_excel(file_path, skip = 1, col_names = TRUE)
            df_annee <- data.frame(
              DEM = data$Région,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
              Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
              FA = as.integer(data$`TOTAL F.A.`)
            )
          } else {
            data <- read_excel(file_path, skip = 0, col_names = TRUE)
            df_annee <- data.frame(
              DEM = data$`Region-Etablissement`,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte`),
              Taux_dechet = as.numeric(data$`Taux de déchets`),
              FA = as.integer(data$`Total FA`)
            )
          }
          
          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(database, each = nrow(data))
          Res_Enquete <- rbind(Res_Enquete, df_annee)
        }
      }
      
      
      
      if (database == "Camme") {
        
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\Camme 2008-2023\\harmonise"
        setwd(directory)
        
        ### ETAPE 1 : IMPORT
        
        for (year in 2008:2023) {
          
          print(year)
          
          file_path <- paste0("Result CAMME ", year, ".xls")
          
          ### ETAPE 2 : Formatage
          
          if (year<= 2017){
            
            data <- read_excel(file_path, skip = 1, col_names = TRUE)
            
            df_annee <- data.frame(
              DEM = data$Région,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
              Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
              FA = as.integer(data$`TOTAL F.A.`))
          }
          else{
            data <- read_excel(file_path, skip = 3, col_names = TRUE)
            
            df_annee <- data.frame(
              DEM = data$`Region-Etablissement`,
              Année = as.integer(rep(year, each = nrow(data))),
              Taux_collecte = as.numeric(data$`Taux de collecte`),
              Taux_dechet = as.numeric(data$`Taux de déchets`),
              FA = as.integer(data$`Total FA`))
          }
          
          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(database, each = nrow(data))
          Res_Enquete <- rbind(Res_Enquete, df_annee)
        }
      } #FIN Camme
      
      
      #DEBUT ENQUETES PONCTUELLES
      
      if (database == "Autres") {
        
        directory <- "U:\\EXP QUAL\\CAPOST\\2024\\Stage\\Enquetes ponctuelles"
        files <- list.files(path = directory, pattern = "Result.*\\.xls$", full.names = TRUE)

        for (file_path in files) {
          
        
          file_name <- tools::file_path_sans_ext(basename(file_path))
          parts <- unlist(strsplit(file_name, " "))
          survey_name <- paste(parts[2:(length(parts) - 1)], collapse = " ")
          year <- as.numeric(parts[length(parts)])
          
          print(paste( survey_name," - ", year))
          
          
          data <- read_excel(file_path, skip = 1, col_names = TRUE)
          
          df_annee <- data.frame(
            DEM = data$Région,
            Année = as.integer(rep(year, each = nrow(data))),
            Taux_collecte = as.numeric(data$`Taux de collecte (1)`),
            Taux_dechet = as.numeric(data$`Taux de déchets (2)`),
            FA = as.integer(data$`TOTAL F.A.`))

          df_annee$Taux_hors_champ = 100 - df_annee$Taux_collecte - df_annee$Taux_dechet
          df_annee$Enquete = rep(survey_name, each = nrow(data))
          
          Res_Enquete <- rbind(Res_Enquete, df_annee)
          
        }
      }
      #FIN ENQUETES PONCTUELLES
      
      
      
      ### ETAPE 3 : UNIFORMSATION
      Res_Enquete <- Res_Enquete %>%
        mutate(DEM = factor(case_when(
          DEM == "Auvergne" ~ "Auvergne-Rhône-Alpes - Clermont-Ferrand",
          DEM == "Rhône-Alpes" ~ "Auvergne-Rhône-Alpes - Lyon",
          DEM == "Auvergne-Rhônes-Alpes - Lyon" ~ "Auvergne-Rhône-Alpes - Lyon",
          DEM == "Auvergne-Rhônes-Alpes - Clermont-Ferrand" ~ "Auvergne-Rhône-Alpes - Clermont-Ferrand",
          DEM == "Bourgogne" ~ "Bourgogne-Franche-Comté - Dijon",
          DEM == "Franche-Comté" ~ "Bourgogne-Franche-Comté - Besançon",
          DEM == "Bretagne" ~ "Bretagne - Rennes",
          DEM == "Centre" ~ "Centre-Val de Loire - Orléans",
          DEM == "Corse" ~ "Corse - Ajaccio",
          DEM == "Alsace" ~ "Grand Est - Strasbourg",
          DEM == "Champagne-Ardenne" ~ "Grand Est - Reims",
          DEM == "Lorraine" ~ "Grand Est - Nancy",
          DEM == "Basse-Normandie" ~ "Normandie - Caen",
          DEM == "Haute-Normandie" ~ "Normandie - Rouen",
          DEM == "Aquitaine" ~ "Nouvelle-Aquitaine - Bordeaux",
          DEM == "Limousin" ~ "Nouvelle-Aquitaine - Limoges",
          DEM == "Poitou-Charentes" ~ "Nouvelle-Aquitaine - Poitiers",
          DEM == "Languedoc-Roussillon" ~ "Occitanie - Montpellier",
          DEM == "Midi-Pyrénées" ~ "Occitanie - Toulouse",
          DEM == "Nord - Pas-de-Calais" ~ "Hauts-de-France - Lille",
          DEM == "Nord-Pas-de-Calais" ~ "Hauts-de-France - Lille",
          DEM == "Picardie" ~ "Hauts-de-France - Amiens",
          DEM == "Pays-de-la-Loire" ~ "Pays de la Loire - Nantes",
          DEM == "Provence-Alpes-Côte d'Azur" ~ "PACA - Marseille",
          DEM == "Ile-de-France" ~ "Ile-de-France - Saint-Quentin-en-Yvelines",
          DEM == "PACA" ~ "PACA - Marseille",
          DEM == "Pays de la Loire" ~ "Pays de la Loire - Nantes",
          DEM == "France métro" ~ "Total Métropole",
          DEM == "Total métropole" ~ "Total Métropole",
          DEM == "Total Métro" ~ "Total Métropole",
          DEM == "DIRAG" ~ "Total DIRAG",
          DEM == "DIROI" ~ "Total DIROI",
          DEM == "Guadeloupe" ~ "DIRAG - ST Guadeloupe",
          DEM == "Guyane" ~ "DIRAG - ST Guyane",
          DEM == "Martinique" ~ "DIRAG - ST Martinique",
          DEM == "Total France" ~ "Total général",
          DEM == "France entière" ~ "Total général",
          DEM == "DOM" ~ "Total DOM",
          DEM == "Total Dom" ~ "Total DOM",
          DEM == "Réunion" ~ "DIROI - Réunion",
          DEM == "RÉUNION" ~ "DIROI - Réunion",
          DEM == "Mayotte" ~ "DIROI - Mayotte",
          DEM == "Réunion_Mayotte" ~ "Total DIROI",
          
          if (database %in% c("Camme")) {
            DEM == "Total" ~ "Total Métropole"
          } else {
            DEM == "Total" ~ "Total général"
            
          },
          
          TRUE ~ DEM
        ))) %>%
        filter(!is.na(DEM)) %>%
        filter(DEM!="(1) Taux de collecte = Total réussies / Total F.A.") %>% 
        filter(DEM!="(2) Taux de déchets = Total déchets / Total F.A.") %>% 
        filter(DEM!="(3) Taux de réussite = (ERP + ERV) / (ERP + ERV + URP + URV + IAJ + ALD + IMP + REF + EVT + ENA + NTA +NTE)") %>% 
        filter(DEM!="Total DIRAG", DEM!="Total DIROI")  %>% # Je les enlève pour plus de lisibilité
        filter(DEM!="Total général") # Je l'enlève pour le recalculer partout
      
      Res_Enquete <- Res_Enquete %>%
        mutate(Region = factor(case_when(
          DEM %in% c("Auvergne-Rhône-Alpes - Clermont-Ferrand","Auvergne-Rhône-Alpes - Lyon") ~ "Auvergne-Rhône-Alpes",
          DEM %in% c("Bourgogne-Franche-Comté - Besançon", "Bourgogne-Franche-Comté - Dijon") ~ "Bourgogne-Franche-Comté",
          DEM %in% c("Bretagne - Rennes") ~ "Bretagne",
          DEM %in% c("Corse - Ajaccio") ~ "Corse",
          DEM %in% c("DIRAG - ST Guadeloupe","DIRAG - ST Guyane", "DIRAG - ST Martinique") ~ "DIRAG",
          DEM %in% c("Grand Est - Nancy","Grand Est - Strasbourg", "Grand Est - Reims") ~ "Grand Est",
          DEM %in% c("Hauts-de-France - Amiens","Hauts-de-France - Lille") ~ "Hauts-de-France",
          DEM %in% c("Ile-de-France - Saint-Quentin-en-Yvelines") ~ "Ile-de-France",
          DEM %in% c("DIROI - Mayotte","DIROI - Réunion") ~ "DIROI",
          DEM %in% c("Normandie - Caen","Normandie - Rouen") ~ "Normandie",
          DEM %in% c("Nouvelle-Aquitaine - Bordeaux","Nouvelle-Aquitaine - Poitiers","Nouvelle-Aquitaine - Limoges") ~ "Nouvelle-Aquitaine",
          DEM %in% c("Occitanie - Montpellier","Occitanie - Toulouse") ~ "Occitanie",
          DEM %in% c("PACA - Marseille") ~ "PACA",
          DEM %in% c("Pays de la Loire - Nantes") ~ "Pays de la Loire",
          DEM %in% c("Centre-Val de Loire - Orléans") ~ "Centre-Val de Loire",
          TRUE ~ DEM
        )))
      
      Res_Enquete <- Res_Enquete %>%
        mutate(Zone = factor(case_when(
          DEM %in% c("Bourgogne-Franche-Comté - Besançon", "Grand Est - Strasbourg", "Grand Est - Reims", "Grand Est - Nancy") ~ "Est",
          DEM %in% c("Bretagne - Rennes", "Pays de la Loire - Nantes") ~ "Ouest",
          DEM %in% c("Corse - Ajaccio", "Auvergne-Rhône-Alpes - Lyon", "Occitanie - Montpellier", "PACA - Marseille") ~ "Sud-Est",
          DEM %in% c("DIRAG - ST Guadeloupe", "DIRAG - ST Guyane", "DIRAG - ST Martinique", "DIROI - Mayotte", "DIROI - Réunion") ~ "DOM",
          DEM %in% c("Auvergne-Rhône-Alpes - Clermont-Ferrand", "Nouvelle-Aquitaine - Poitiers", "Nouvelle-Aquitaine - Limoges", "Bourgogne-Franche-Comté - Dijon", "Centre-Val de Loire - Orléans") ~ "Centre",
          DEM %in% c("Hauts-de-France - Amiens", "Hauts-de-France - Lille", "Normandie - Caen", "Normandie - Rouen") ~ "Nord",
          DEM %in% c("Ile-de-France - Saint-Quentin-en-Yvelines") ~ "Ile-de-France",
          DEM %in% c("Nouvelle-Aquitaine - Bordeaux", "Occitanie - Toulouse") ~ "Sud-Ouest",
          TRUE ~ DEM
        )))
      
      Res_Enquete <- Res_Enquete %>%
        mutate(Nord_Sud = factor(case_when(
          Zone %in% c("Nord","Ouest","Est","Ile-de-France","Centre") ~ "Nord Métropole",
          Zone %in% c("Sud-Ouest","Sud-Est") ~ "Sud Métropole",
          Zone == 'Total Métropole' ~ 'Total Métropole',
          Zone == 'Total DOM' ~ 'Total DOM',
          
        )))
      
      
      save(Res_Enquete, file = "Res_Enquete.rda")
      
      table_anova <- bind_rows(table_anova, Res_Enquete)

    }
    
    ## Ajout de province ##
    annual_weighted_mean_province <- table_anova %>%
      filter(!grepl("DIRAG|DIROI|ST Guadeloupe|ST Guyane|ST Martinique|Réunion|Mayotte|Ile-de-France|Total", DEM)) %>%
      group_by(Enquete, Année) %>%
      summarise(
        Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE),
        Taux_dechet = weighted.mean(Taux_dechet, FA, na.rm = TRUE),
        Taux_hors_champ = weighted.mean(Taux_hors_champ, FA, na.rm = TRUE),
        .groups = 'drop'  # Pour éviter le message d'avertissement sur le regroupement
      )
    
    
    # Préparer rbind en créant les colonnes manquantes
    province_data <- annual_weighted_mean_province %>%
      mutate(DEM = "Province",
             Region = "Province",
             Zone = "Province",
             Nord_Sud = "Province",
             FA = 1) %>%
      select(Enquete, DEM, Region, Zone, Nord_Sud, Année, Taux_collecte, Taux_dechet, Taux_hors_champ, FA)
    
    
    # Ajouter les données de province à table_anova
    table_anova <- bind_rows(table_anova, province_data)
    
    
    # Calculer les taux pondérés pour le total général (métropole + DOM)
    annual_weighted_mean_tot <- table_anova %>%
      filter(DEM %in% c("Total Métropole", "Total DOM")) %>%
      group_by(Enquete, Année) %>%
      summarise(
        Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE),
        Taux_dechet = weighted.mean(Taux_dechet, FA, na.rm = TRUE),
        Taux_hors_champ = weighted.mean(Taux_hors_champ, FA, na.rm = TRUE),
        .groups = 'drop'  # Pour éviter le message d'avertissement sur le regroupement
      )
    
    # Préparer rbind en créant les colonnes manquantes
    tot_general_data <- annual_weighted_mean_tot %>%
      mutate(DEM = "Total général",
             Region = "Total général",
             Zone = "Total général",
             Nord_Sud = "Total général",
             FA = 1) %>%
      select(Enquete, DEM, Region, Zone, Nord_Sud, Année, Taux_collecte, Taux_dechet, Taux_hors_champ, FA)
    
    table_anova <- bind_rows(table_anova, tot_general_data)
    
    table_anova <- table_anova %>%
      mutate(Type_Enquete = factor(case_when(
        Enquete %in% c("EEC", "SRCV", "Camme", "LC", "TIC") ~ "Permanente",
        TRUE ~ "Ponctuelle"
      )))
    
    
    
    save(table_anova, file = "U:/EXP QUAL/CAPOST/2024/Stage/Code R/Table_Anova_Entiere.rda")
    
    
    output$occurence <- renderTable({

      table_anova %>%  
        group_by(Enquete, DEM) %>% 
        summarise(Count = n(), Années = paste((Année), collapse = ", "), .groups = 'drop')
    })
    
    output$dataTable <- renderTable({
      req(input$database)
  
      table_anova 

    })
    
  })      
  
  
  
  
    ##### IMPORT CLASSIQUE #####
  
  # Vérifiez si le fichier existe et chargez-le ou lancez le processus de création
  if (file.exists("U:/EXP QUAL/CAPOST/2024/Stage/Code R/Table_Anova_Entiere.rda")) {
    
    
    load(file = "U:/EXP QUAL/CAPOST/2024/Stage/Code R/Table_Anova_Entiere.rda")
    print('Fichier chargé')

      output$occurence <- renderTable({
        req(input$database)
        
        table_anova %>%  
          filter(Enquete == input$database) %>%
          group_by(Enquete, DEM) %>% 
          summarise(Effectif = n(), Années = paste((Année), collapse = ", "), .groups = 'drop')
      })
      
      output$dataTable <- renderTable({
        req(input$database)
        
        table_anova

      })
      }
    else {
    
      print('Fichier non trouvé')
      
    }
      
      
      
      

  
  ###########################################################################
  #####################DEBUT DES PANNEAUX ###################################
  ###########################################################################
  
  #### SERIES ####
  ##### SERIE TEMP WraP PAR ANNEE #####
  observeEvent(c(input$groupBy, input$supr_2020, input$filtre_ponctuel, input$recharge), {

    output$timeSeriesWrapPlot <- renderPlot({
      
      # Filter and summarize the data
      filtered_data <- table_anova %>%
        filter(!is.na(!!sym(input$groupBy))) %>% 
        group_by(Type_Enquete, Enquete, !!sym(input$groupBy), Année) %>%
        summarise(mean_Taux_collecte =  weighted.mean(Taux_collecte, FA, na.rm = TRUE))
      
      if (input$supr_2020) {
        filtered_data <- filtered_data %>%
          filter(Année != 2020)
      }      
      
      if (!input$filtre_ponctuel){
        filtered_data <- filtered_data %>%
          filter(Type_Enquete != "Ponctuelle")}
      
      ggplot(filtered_data,
             aes(x = Année, y = mean_Taux_collecte, color = !!sym(input$groupBy))) +
        geom_line(linewidth = 1) +
        geom_point() +
        facet_wrap(~Enquete, nrow=1) +
        labs(title = paste("Moyenne des taux de collecte des enquêtes permanentes par", input$groupBy),
             x = "Année", y = "Taux de Collecte") +
        theme_minimal() +
        theme(legend.position = "none")
    },)  # Specify the desired height here
  })
  
  #### SERIE TEMP DETAILLE ANNEE ####     
  observeEvent(c(input$database, input$groupBy, input$supr_2020, input$recharge), {
    req(input$database)
    
    output$timeSeriesPlot <- renderPlotly({
      
      # Filter and summarize the data
      filtered_data <- table_anova %>%
        filter(Enquete == input$database) %>%
        filter(!is.na(!!sym(input$groupBy))) %>% 
        group_by(!!sym(input$groupBy), Année) %>%
        summarise(mean_Taux_collecte =  weighted.mean(Taux_collecte, FA, na.rm = TRUE))
      
      if (input$supr_2020){
        filtered_data <- filtered_data %>%
          filter(Année != 2020)}
      
      p <- ggplot(filtered_data,
                  aes(x = Année, y = mean_Taux_collecte, color = !!sym(input$groupBy))) +
        geom_line(linewidth=1) +
        geom_point() +
        labs(title = paste("Moyenne des taux de collecte de", input$database, "par", input$groupBy),
             x = "Année", y = "Taux de Collecte") +
        theme_minimal() +
        theme(legend.title = element_blank()) # Remove legend title
      
      ggplotly(p)
    })
  })
  
  
  
  ##### SERIE TEMP ENQUETE WRAP #####
  observeEvent(c(input$groupBy, input$region, input$supr_2020, input$filtre_ponctuel, input$recharge), {
    req(input$region)
    
    output$timeSeriesPlotWrapTaux <- renderPlot({
      
      # Filter and summarize the data
      filtered_data <- table_anova %>%
        filter(!!sym(input$groupBy) == input$region) %>%
        group_by(Type_Enquete, Enquete, !!sym(input$groupBy), Année) %>%
        summarise(mean_Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE),
                  mean_Taux_dechet = weighted.mean(Taux_dechet, FA, na.rm = TRUE)) %>%
        mutate(mean_Taux_hors_champ = 100 - mean_Taux_collecte - mean_Taux_dechet)
      
      if (input$supr_2020) {
        filtered_data <- filtered_data %>%
          filter(Année != 2020)
      }
      
      if (!input$filtre_ponctuel){
        filtered_data <- filtered_data %>%
          filter(Type_Enquete != "Ponctuelle")}
      
      # Reshape data for plotting
      melted_data <- filtered_data %>%
        pivot_longer(cols = c(mean_Taux_collecte, mean_Taux_dechet, mean_Taux_hors_champ), 
                     names_to = "Taux_Type", values_to = "Taux")
      
      ggplot(melted_data, aes(x = Année, y = Taux, color = Taux_Type)) +
        geom_line(linewidth = 1) +
        geom_point() +
        facet_wrap(~Enquete, nrow=1) +
        labs(title = paste("Moyenne des taux par enquête pour", input$region),
             x = "Année", y = "Taux") +
        theme_minimal() +
        theme(legend.title = element_blank()) # Remove legend title
    })
  })

  
      ##### SERIE TEMP PAR ENQUETE #####
      observeEvent(c(input$groupBy, input$region, input$supr_2020, input$filtre_ponctuel, input$recharge), {
        req(input$region)
        
        output$timeSeriesPlotEnquete <- renderPlot({
          
          # Filter and summarize the data
          filtered_data <- table_anova %>%
            filter(!!sym(input$groupBy) == input$region) %>%
            group_by(Type_Enquete, Enquete, !!sym(input$groupBy), Année) %>%
            summarise(mean_Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE))
          
          if (input$supr_2020){
            filtered_data <- filtered_data %>%
              filter(Année != 2020)}
          
          if (!input$filtre_ponctuel){
            filtered_data <- filtered_data %>%
              filter(Type_Enquete != "Ponctuelle")}
          
          ggplot(filtered_data, aes(x = Année, y = mean_Taux_collecte, color = Enquete)) +
            geom_line(linewidth = 1) +
            geom_point(aes(size = ifelse(filtered_data$Type_Enquete == "Ponctuelle", "Ponctuelle", "Permanente"))) +
            labs(y = "Taux de Collecte") +
            theme_minimal() +
            theme(legend.title = element_blank()) +  # Remove legend title
            scale_size_manual(
              name = "Type d'Enquête",  # Legend title
              values = c("Permanente" = 1, "Ponctuelle" = 5)  # Specify size values for legend
            )
          
          
        })
      })
      
      
  


  ##### SERIE TEMP DETAILLEE AVEC LES 3 TAUX #####
observeEvent(c(input$database, input$groupBy, input$region, input$supr_2020, input$recharge), {
  req(input$database, input$region)
  
  # Filter and summarize the data
  filtered_data <- table_anova %>%
    filter(Enquete == input$database) %>%
    filter(!!sym(input$groupBy) == input$region) %>%
    group_by(Année) %>%
    summarise(mean_Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE),
              mean_Taux_dechet = weighted.mean(Taux_dechet, FA, na.rm = TRUE),
              mean_Taux_hors_champ = 100 - weighted.mean(Taux_collecte, FA, na.rm = TRUE) - weighted.mean(Taux_dechet, FA, na.rm = TRUE))  # Calcul de mean_Taux_hors_champ
  
  if (input$supr_2020) {
    filtered_data <- filtered_data %>%
      filter(Année != 2020)
  }
  
  # Reshape data for plotting
  melted_data <- filtered_data %>%
    pivot_longer(cols = c(mean_Taux_collecte, mean_Taux_dechet, mean_Taux_hors_champ), 
                 names_to = "Taux_Type", values_to = "Taux")
  
  
  output$timeSeriesPlotTaux <- renderPlot({
    
    # Time series plot
    ggplot(melted_data, aes(x = Année, y = Taux, color = Taux_Type)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = paste("Moyenne des taux pour l'enquête ", input$database, "pour", input$region),
           x = "Année", y = "Taux") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
    output$timeSeriesBoxPlotTaux <- renderPlot({
    ggplot(melted_data, aes(x = Taux_Type, y = Taux, fill = Taux_Type)) +
      geom_boxplot() +
      labs(title = "Distribution des taux",
           x = "Type de Taux", y = "Taux") +
      theme_minimal() +
      theme(legend.title = element_blank(),  # Remove legend title
            axis.text.x =element_blank())  # Rotate x-axis labels
  })
})
   
      #### FIN SERIES ####
  
      
  #### REGRESSION LINEAIRE ####
  observeEvent(c(input$database, input$groupBy, input$region, input$annee_coupure, input$supr_2020, input$recharge), {
    req(input$database, input$region)
    
    filtered_data <- table_anova %>%
      filter(Enquete == input$database) %>%
      filter(get(input$groupBy) == input$region) %>%
      group_by(Année) %>%
      summarise(mean_Taux_collecte = weighted.mean(Taux_collecte, FA, na.rm = TRUE))
    
    if (input$supr_2020){
      filtered_data <- filtered_data %>%
        filter(Année != 2020)}
    
    #Pour la double regression
    Res_Enquete_Region_avt <- filtered_data %>%
      filter(Année <= input$annee_coupure)
    
    Res_Enquete_Region_apr <- filtered_data %>%
      filter(Année >= input$annee_coupure)

    Res_Enquete_Region_avt$Period <- "Avant l'année de coupure"
    Res_Enquete_Region_apr$Period <- "Apres l'année de coupure"
    combined_data <- bind_rows(Res_Enquete_Region_avt, Res_Enquete_Region_apr)

    
    
    ##### REGRESSION LINEAIRE PLOT #####
    output$linearRegressionPlot <- renderPlot({

      # DOUBLE REGRESSION
      if (input$annee_coupure != 'Aucune') {
        
        # Create the plot
        ggplot(combined_data, aes(x = Année, y = mean_Taux_collecte, color = Period)) +
          geom_point() +
          geom_line(linewidth=1) +
          
          geom_smooth(method = "lm", se = FALSE, formula = y~x) +
          scale_color_manual(values = c("Avant l'année de coupure" = "blue", "Apres l'année de coupure" = "red")) +
          labs(title = paste("Régression linéaire sur la moyenne des taux de collecte de", input$region, "pour l'enquête ",input$database),
               x = "Année", y = "Moyenne Taux Collecte") +
           theme_minimal()+
          theme(legend.position = "none") # Remove legend        

       } else { #Regression Simple

         ggplot(
           filtered_data,
           aes(x = Année, y = mean_Taux_collecte))+
           geom_line(linewidth=1) +
           geom_point() +
           geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "red", linetype = "dashed") + # Add regression line
           labs(title = paste("Moyenne des taux de collecte de", input$database, "pour", input$region),
                x = "Année", y = "Taux de Collecte") +
           theme_minimal() +
           theme(legend.title = element_blank()) # Remove legend title
       }
     })


      ##### REGRESSION LINEAIRE SUMMARY #####

  output$regressionSummary <- renderPrint({

    #DOUBLE REG LIN
    if (input$annee_coupure != 'Aucune') {

      modele_reg_avt <- lm(mean_Taux_collecte ~ Année, data = Res_Enquete_Region_avt)
      modele_reg_apr <- lm(mean_Taux_collecte ~ Année, data = Res_Enquete_Region_apr)

      list(
        Summary_Before_Cutoff = summary(modele_reg_avt),
        Summary_After_Cutoff = summary(modele_reg_apr))
    }
    
    else{   #Regression Simple
      
      modele_reg <- lm(mean_Taux_collecte ~ Année, data = filtered_data)
      summary(modele_reg)}
    })

  })
  
  #### FIN REGRESSION LINEAIRE ####
      
      
  #### ENQUETE : ANOVA ####
      #### ENQUETE : ANOVA BOXPLOT ####
  observeEvent(c(input$groupBy, input$region, input$supr_2020, input$filtre_ponctuel, input$recharge), {
    
    req(input$region, input$groupBy)
    
    filtered_data <- table_anova %>%            
      filter(!!sym(input$groupBy) == input$region)
    
    if (input$supr_2020){
      filtered_data <- filtered_data %>%
        filter(Année != 2020)}
    
    if (!input$filtre_ponctuel){
      filtered_data <- filtered_data %>%
        filter(Type_Enquete != "Ponctuelle")}
    
    output$AnovaPlot <- renderPlot({     
      

      # Boxplot
      ggplot(filtered_data, aes(x = Enquete, y = Taux_collecte, fill = Enquete)) +
        geom_boxplot() +
        labs(y = "Type de collecte") +
        theme_minimal() +
        theme(legend.title = element_blank())
      
    })
    
    #### ENQUETE : ANOVA SUMMARY ####
    output$AnovaSummary <- renderPrint({
      
      fit <- aov(Taux_collecte ~ Enquete, filtered_data)
      summary(fit)
    })
  })
  
  #### FIN ANOVA ####
      
      
  #### UI : SELECT REGION ####
  observeEvent(c(input$database, input$groupBy, input$recharge), {    
    req(input$database)
    
  
    # Stocker la sélection actuelle de la région
    current_region <- isolate(input$region)
    
    localisations <- NULL
    
    output$regionSelector <- renderUI({
      if (input$format != 'enquete') {
        filtered_data <- table_anova %>%
          filter(Enquete == input$database)
        
        localisations <<- sort(unique(filtered_data[[input$groupBy]]))
      } else {
        localisations <<- sort(unique(table_anova[[input$groupBy]]))
      }
      
      selectInput("region", "Choix :", choices = localisations, selected = current_region)
    })
    
    # Mettre à jour la sélection de la région
    observe({
      if (!is.null(current_region) && current_region %in% localisations) {
        updateSelectInput(session, "region", selected = current_region)
      }
    })
    
  })
  #### FIN UI : SELECT REGION ####

  
  
  
  
  
  
  observeEvent(c(input$filtre_ponctuel, input$recharge), {
    
    output$databaseSelector <- renderUI({
      
      databases_ponct <- table_anova %>%
        filter(Type_Enquete == "Ponctuelle") %>%
        pull(Enquete) %>%
        sort() %>%
        unique()
      
      if (input$filtre_ponctuel) {
        databases <- list(
          "Enquêtes Permanentes" = c("EEC", "SRCV", "Camme", "LC", "TIC"),
          "Enquêtes Ponctuelles" = databases_ponct)
      } 
      
      else {
        databases <- list(
          "Enquêtes Permanentes" = c("EEC", "SRCV", "Camme", "LC", "TIC"))
      }
      
      selectInput("database", "Choisir l'enquête:",
                  choices = databases)
      
    })
    
  })
      
  


  
      
  

  

  
  
  
  
}