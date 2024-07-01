

library(shiny)
library(plotly)

ui <- fluidPage(
  
  # Application title
  titlePanel("VITAMIN (VIsualisation des Taux de collecte Annuels auprès des Ménages Intérrogés au niveau National)"),
  
  
  # Sidebar layout with inputs and outputs
  sidebarLayout(
    
    sidebarPanel(
      
      # conditionalPanel(
      #   condition = "input.format != 'taux de déchets et hors-champs'",
      # 
      #   radioButtons("taux_affiche", 
      #                label = "Taux à étudier:", 
      #                choices = c("collecte","dechets","hors-champs")),      
      #   
      #   hr(),  # Horizontal line for separation
      # 
      # ),
      
      radioButtons("format", 
                   label = "Etude du taux de collecte par :", 
                   choices = c("année","enquete","taux de déchets et hors-champs")),
      

      hr(),  # Horizontal line for separation
      
      checkboxInput("filtre_ponctuel", "Inclure les enquêtes ponctuelles"),
      
      conditionalPanel(
        condition = "input.format != 'enquete'",
        
        uiOutput("databaseSelector"),
      ),
      
    
      hr(),  # Horizontal line for separation
      
      selectInput("groupBy", "Découpage par :", 
                  choices = c("DEM", "Region", "Zone","Nord_Sud")),
      
      # Dynamic UI for selecting region based on groupBy
      uiOutput("regionSelector"),
      
      hr(),  # Horizontal line for separation
      
      conditionalPanel(
        condition = "input.format == 'année'",
        selectInput("annee_coupure", "Séparation par années :", 
                  choices = c('Aucune', 2007:2023)),      
        hr()  # Horizontal line for separation
      ),
      
      
      
      checkboxInput("supr_2020", "Supprimer l'année du COVID (2020)"),
      helpText("Note : L'enquete TIC n'a pas été réalisée en 2020"),
      
      
      hr(),  # Horizontal line for separation
      
      actionButton("recharge","Recharger les données"),
      helpText("Note : ce n'est utile qu'en cas de modification des fichiers ou de la partie import du code"),
      
      checkboxInput("voirdata", "Voir les données brutes"),
      
      
      width = 3  # Width of the sidebar panel
    ),
    
    
    
    mainPanel(
      conditionalPanel(
        condition = "input.format == 'année'",
        # Section: Evolution Temporelle
        h3("Evolution Temporelle"),
        plotOutput("timeSeriesWrapPlot", height = 200),
        plotlyOutput("timeSeriesPlot", height = 350),
        
        fixedRow(
          column(7, plotOutput("linearRegressionPlot", height= 350)),
          column(5, verbatimTextOutput("regressionSummary")))
      ),

    
    
      conditionalPanel(
          condition = "input.format == 'enquete'",

          h3("Evolution Temporelle"),
          plotOutput("timeSeriesPlotEnquete"),
          
          
          # Section: Anova
          h3("Anova"),
          
          fixedRow(
            column(8, plotOutput("AnovaPlot", height= 240)),
            column(4, verbatimTextOutput("AnovaSummary")))

      ),
          
          
      
      
      conditionalPanel(
        condition = "input.format == 'taux de déchets et hors-champs'",
        
        h3(),        
        
        plotOutput("timeSeriesPlotWrapTaux", height = 200),
        
        fixedRow(
          column(9, plotOutput("timeSeriesPlotTaux")),
          column(3, plotOutput("timeSeriesBoxPlotTaux")))
      ),
      
        

      conditionalPanel(
        condition = "input.voirdata",
        
        # Section: Voir les données
        hr(),  # Horizontal line for separation
        h3("Voir les données brutes"),
        tableOutput("occurence"),
        tableOutput("dataTable")
      ),

      
    ),
    fluid = FALSE
  )
)
