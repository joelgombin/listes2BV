

library(shiny)
library(DT)
library(leaflet)
library(adresses2shape)
library(dplyr)
library(multidplyr)
library(magrittr)
library(rgeos)
library(memoise)
library(geojsonio)

geocode <- memoise(adresses2shape::geocode)
create_shapes <- memoise(adresses2shape::create_shapes)

# cl <- get_default_cluster()
# cl %>%
#   cluster_library("dplyr") %>%
#   cluster_library("Adresses2Shape") %>%
#   cluster_library("maptools") %>%
#   cluster_library("rgeos")

options(shiny.maxRequestSize=50*1024^2) 

load("./data/communes2016.Rdata")
liste_communes <- communes$insee
names(liste_communes) <- paste0(communes$nom, " (", liste_communes, ")")
liste_communes <- liste_communes[order(names(liste_communes))]

shinyServer(function(input, output, session) {

  raw <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    readr::read_delim(inFile[1, "datapath"], delim = input$separateur)
  })
  
  observe({
    updateSelectInput(session, "adresse", choices = names(raw()))
  })
  observe({
    updateSelectInput(session, "bureau", choices = names(raw()))
  })
  
  
  updateSelectizeInput(session, "codeinsee", choices = liste_communes, server = TRUE)
  
  output$tbl <- DT::renderDataTable(raw(), 
                                    options = list(dom="t"), 
                                    server = TRUE, 
                                    rownames = FALSE,
                                    filter = "top")
  
  adresses <- reactive({
    if (input$valider) {
      try({
        withProgress(message = "Un peu de patience !", value = 0, {
          interm <- raw() %>% 
            filter_(input$adresse != "") %>% 
            mutate_(Bureau = lazyeval::interp(~ stringr::str_pad(var, 4, "left", "0"), var = as.name(input$bureau))) %>% 
            #      mutate(Arrondissement = stringr::str_sub(Bureau, 1, 2)) %>%
            distinct_(input$adresse, .keep_all = TRUE) %>% 
            mutate(insee = as.character(input$codeinsee))
          incProgress(amount = 1/2, message = "Un peu de patience !", detail = "Géocodage des adresses")
          geocode(interm, input$adresse, "insee")
      })
    })
    } else {
      NA
    }
  })
  
  points <- reactive({
    if (input$valider) {
        coordonnees <- as.data.frame(adresses())
        coordonnees <- coordonnees[!(is.na(coordonnees$longitude) | is.na(coordonnees$longitude)),]
        spdf <- SpatialPointsDataFrame(coordonnees[, c("longitude", "latitude")], coordonnees[, c(input$adresse, "Bureau", "longitude", "latitude", "result_score")])
        spdf@data$adresse <- spdf@data[, input$adresse]
        print(head(spdf@data))
        spdf
    } else {
      NA
    }
  })
  
  bureaux <- reactive({
    if (input$valider) {
      try({
        withProgress(message = "Un peu de patience !", value = 0, {
          incProgress(amount = 1, detail = "Calcul des contours des bureaux de vote", message = "Un peu de patience !")
          polygons <- adresses() %>% 
            create_shapes("Bureau", threshold = input$seuil, clip_by_chull = TRUE)
          polygons_cropped <- gIntersection(polygons, communes[communes@data$insee %in% as.character(input$codeinsee),], byid = c(TRUE, FALSE))
          print(row.names(polygons_cropped))
          print(row.names(polygons@data))
          polygons <- SpatialPolygonsDataFrame(polygons_cropped, polygons[row.names(polygons_cropped),]@data)
          polygons
        })
      })
    } else {
      NA
    }
  })
  
  observeEvent(class(bureaux()), {
    if (class(bureaux()) %in% "SpatialPolygonsDataFrame") {
      insertUI(
        selector = '#placeholder',
        ui = tags$div(
          leafletOutput("carte"),
          downloadButton('downloadData', 'Télécharger le geojson')
        )
      )
    }
  })
  
  output$carte <- renderLeaflet({
    factpal <- colorFactor(topo.colors(length(unique(bureaux()@data$ID))), sample(bureaux()@data$ID), ordered = TRUE)
    
    leaflet(bureaux()) %>% 
      addTiles() %>% 
      addPolygons(popup = ~ paste0("Numéro de bureau : ", ID),
                  fillColor = ~ factpal(ID)
                  ) %>% 
      addCircles(data = points(), 
                 radius = 1.5,
                 popup = ~ paste0(adresse, "<BR> Bureau n° : ", Bureau)
                 )
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste0("BV", input$codeinsee, ".geojson") },
    content = function(file) {
      geojson_write(bureaux(), file = file)
    }
  )
})
