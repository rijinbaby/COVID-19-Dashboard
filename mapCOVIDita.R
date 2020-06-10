
#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(leaflet, tidyverse)

#Load Packages
library(leaflet)
library(tidyverse)
library(htmltools)

#   colnames(pcmTOTData)
#   database=ProvDB

ProvinceMap <- function(database, var) {
  options(warn=-1)
  
  # database <- pcmTOTData
  # var <- "totale_casi.x"
  # prevIndex
  
  discrVariable <- round((database[[var]]/sum(database[[var]]))*100, digits = 2)
  if(sum(is.nan(discrVariable))>0){
    discrVariable[which(is.nan(discrVariable))] <- 0
  }
  database$discrVariable <- discrVariable
  # sort(discrVariable)
  discrLevel <- as.numeric(quantile(discrVariable, probs = seq(0, 1, length.out = 11)))
  
  # getColor <- function(database) {
  #   sapply(database$discrVariable, function(discrVariable) {
  #     if(discrVariable <= 0) {
  #       "darkgreen"
  #     } else if(discrVariable <= discrLevel[6]) {
  #       "orange"
  #     } else if(discrVariable <= discrLevel[9]) {
  #       "red"
  #     }else if(discrVariable > discrLevel[9]) {
  #       "black"
  #     } })
  # }
  # 
  getColor <- function(database) {
    sapply(database$discrVariable, function(discrVariable) {
      if(discrVariable <= 0) {
        "green"
      } else if(discrVariable <= discrLevel[6]) {
        "yellow"
      } else if(discrVariable <= discrLevel[9]) {
        "orange"
      }else if(discrVariable > discrLevel[9]) {
        "red"
      } })
  }
  

  setLabel <- paste(sep = "<br/>"
                   , paste0("<b>", toupper(database$denominazione_provincia), "</b>")
                   , paste0(" Cumulative cases: ", database$totale_casi.x)
                   , paste0(" Cumulative rates: ", database$prevIndex))
  # # 
  
  #Map building----
  icons <- awesomeIcons(
    icon = 'clipboard',
    iconColor = 'white',
    library = 'ion',
    markerColor = getColor(database)
  )

  provMap <- leaflet(data = database) %>%
    addTiles() %>%
    addAwesomeMarkers(~long.x, ~lat.x
                      , icon=icons
                      , popup = ~setLabel
                      , label = ~toupper(denominazione_provincia))

    
  #Map plot----
  return(provMap)
    
}

