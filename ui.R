
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("Liste électorale => bureaux de vote"),


  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choisir un fichier CSV à uploader", 
                accept = c("text/csv", 
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv')
                ),
      selectInput("separateur", "Séparateur de colonnes dans le fichier CSV",
                  choices = c(";", ","), selected = ";", multiple = FALSE),
      selectInput("adresse", "Nom de la colonne comportant les adresses",
                  choices = "", selected = NULL, multiple = FALSE),
      selectInput("bureau", "Nom de la colonne comportant les numéros/ID de bureau de vote",
                  choices = "", selected = NULL, multiple = FALSE),
      selectizeInput("codeinsee", "Nom de la commune concernée",
                  choices = NULL, selected = NULL, multiple = FALSE),
      sliderInput("seuil", "Seuil de qualité minimal de la géolocalisation à utiliser", 0, 1, 0.8, step = 0.05),
      actionButton("valider", "Générer les bureaux de vote")
    ),

    mainPanel(
      DT::dataTableOutput('tbl'),
      tags$div(id = 'placeholder')
#        downloadButton('downloadData', 'Télécharger le geojson')
      )
    )
  )
)
