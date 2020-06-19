#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, plotly)

#Load Packages
library(tidyverse)
library(ggplot2)
library(plotly)

# database = cumDeathsProv


# death_rate_home chart ---------------------------------------------------

death_rate_home <- function(database, variable){
  options(warn=-1)
  
  untilDate <- max(database$data)
  
  # if(variable=="PrevIDX"){
    
    database <- database[,c("provincia",  "data", "deathRates")]
    
    colnames(database) <- c("Province", "Date", "Death rates")
    
    # title=paste0("Death rates until the "
    #              , paste0(str_split(untilDate, "-")[[1]][3]
    #                       , "/",  str_split(untilDate, "-")[[1]][2])
    #              , " at provincial level")) +
    
    Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
      geom_line(aes(y=`Death rates`)) +
      geom_point(aes(y=`Death rates`), size=1) +
      labs(
        x = "Dates",
        #y = "Death rates",
        color="Provinces",
        title=paste0("Province Level Death Rates")) +
      theme(legend.position = "none"
            ,legend.title = element_text(colour="black", face = "bold"))
    
    GGPlotly<-ggplotly(Plot)
    
    GGPrlotly <- GGPlotly %>%
      layout(xaxis = list(visible=FALSE))
    
    return(print(x = GGPrlotly))
  }

deathTS <- function(database, variable){
  options(warn=-1)
  
  untilDate <- max(database$data)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("provincia",  "data", "deathRates")]
    
    colnames(database) <- c("Province", "Date", "Death rates")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
      geom_line(aes(y=`Death rates`)) +
      geom_point(aes(y=`Death rates`), size=1) +
      labs(
        x = "Dates",
        y = "Death rates",
        color="Provinces",
        title=paste0("Death rates until the "
                     , paste0(str_split(untilDate, "-")[[1]][3]
                              , "/",  str_split(untilDate, "-")[[1]][2])
                     , " at provincial level")) +
      theme(legend.position = "none"
        ,legend.title = element_text(colour="black", face = "bold"))
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_provincia",  "data", "totale_casi.x")]
    
    colnames(database) <- c("Province", "Date", "Total Cases")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
      geom_line(aes(y=`Total Cases`)) +
      geom_point(aes(y=`Total Cases`), size=1) +
      labs(
        x = "Dates",
        y = "Cumulative cases",
        color="Provinces",
        title=paste0("Cumulative cases until the "
                     , paste0(str_split(untilDate, "-")[[1]][3]
                              , "/",  str_split(untilDate, "-")[[1]][2])
                     , " in the provinces selected")) +
      theme(legend.title = element_text(colour="black", face = "bold"))
    
  }
  
  
  GGPlotly<-ggplotly(Plot)
  
  GGPrlotly <- GGPlotly %>%
    layout(xaxis = list(visible=FALSE))
  
  return(print(x = GGPrlotly))
  
}

#-------------------------------------------

ProvinceTS <- function(database, variable){
  options(warn=-1)
  
  untilDate <- max(database$data)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("denominazione_provincia",  "data", "prevIndex")]
   
    colnames(database) <- c("Province", "Date", "Cumulative rates")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
      geom_line(aes(y=`Cumulative rates`)) +
      geom_point(aes(y=`Cumulative rates`), size=1) +
      labs(
        x = "Dates",
        y = "Cumulative rates",
        color="Provinces",
        title=paste0("Cumulative rates until the "
                     , paste0(str_split(untilDate, "-")[[1]][3]
                              , "/",  str_split(untilDate, "-")[[1]][2])
                     , " in the provinces selected")) +
					 theme(legend.title = element_text(colour="black", face = "bold"))
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_provincia",  "data", "totale_casi.x")]
    
    colnames(database) <- c("Province", "Date", "Total Cases")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
      geom_line(aes(y=`Total Cases`)) +
      geom_point(aes(y=`Total Cases`), size=1) +
      labs(
        x = "Dates",
        y = "Cumulative cases",
        color="Provinces",
        title=paste0("Cumulative cases until the "
                     , paste0(str_split(untilDate, "-")[[1]][3]
                              , "/",  str_split(untilDate, "-")[[1]][2])
                     , " in the provinces selected")) +
					 theme(legend.title = element_text(colour="black", face = "bold"))
    
  }
  

  GGPlotly<-ggplotly(Plot)

  GGPrlotly <- GGPlotly %>%
    layout(xaxis = list(visible=FALSE))

  return(print(x = GGPrlotly))
  
}

#-------------------------------------------

RegionTS1 <- function(database, rangeDays, variable){
  options(warn=-1)
  
  regione <- unique(database$denominazione_regione)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("denominazione_regione"
                            , "denominazione_provincia"
                            , "data"
                            , "prevIndex")]
    
    colnames(database) <- c("Regione", "Provincia", "Date", "Cumulative rates")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Provincia)) +
      geom_line(aes(y=`Cumulative rates`)) +
      labs(
        x = "Dates",
        y = "Cumulative rates",
        color="Province",
        title=paste0("Cumulative rates until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione, " by province")) +
					 theme(legend.title = element_text(color="black", face = "bold"))
    
    GGPlotly<-ggplotly(Plot)
    
    GGRglotly <- GGPlotly %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_regione"
                            , "denominazione_provincia"
                            , "data"
                            , "totale_casi.x")]
    
    colnames(database) <- c("Regione", "Provincia", "Date", "Province Cases")
    
    Plot<-ggplot(data=database, aes(x=Date, color=Provincia)) +
      geom_line(aes(y=`Province Cases`)) +
      labs(
        x = "Dates",
        y = "Cumulative cases",
        color="Province",
        title=paste0("Cumulative cases until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione, " by province")) +
					 theme(legend.title = element_text(color="black", face = "bold"))
    
    GGPlotly<-ggplotly(Plot)
    
    GGRglotly <- GGPlotly %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  }
  
  return(print(x = GGRglotly))
  
}

#-------------------------------------------

RegionTS2 <- function(database, rangeDays, variable){
  options(warn=-1)
  
  regione <- unique(database$denominazione_regione)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "guarPrevIdxReg"
                            , "decePrevIdxReg"
                            , "actualPrevIdxReg")]
    
    colnames(database) <- c("Regione", "Date", "Recovered rates", "Deceased rates", "Total rates")
    
    colors <- c("Recovered rates" = "green", "Deceased rates" = "black", "Total rates" = "red")
    
    Plot2<-ggplot(data=database, aes(x=Date)) +
      geom_line(aes(y = `Recovered rates`, color="Recovered rates"), size = 1) +
      geom_line(aes(y = `Deceased rates`, color="Deceased rates"), size = 1) +
      geom_line(aes(y = `Total rates`, color="Total rates"), size = 1) +
      geom_point(aes(y=`Recovered rates`), size=0.5) +
      geom_point(aes(y=`Deceased rates`), size=0.5) +
      geom_point(aes(y=`Total rates`), size=0.5) +
      labs(
        x = "Dates",
        y = "Cumulative rates",
        color="Legend",
        title=paste0("Cumulative rates until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione)) +
      scale_color_manual(values = colors) +
      theme(legend.title = element_text(color="black", face = "bold"))+
      guides(color=guide_legend("Variables"))
    
    GGPlotly2<-ggplotly(Plot2)
    
    GGRglotly2 <- GGPlotly2 %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "dimessi_guariti.x"
                            , "deceduti.x"
                            , "totale_casi.y")]
    
    colnames(database) <- c("Regione", "Date", "Recovered cases", "Deceased cases", "Total cases")
    
    colors <- c("Recovered cases" = "green", "Deceased cases" = "black", "Total cases" = "red")
    
    Plot2<-ggplot(data=database, aes(x=Date)) +
      geom_line(aes(y = `Recovered cases`, color="Recovered cases"), size = 1, show.legend = F) +
      geom_line(aes(y = `Deceased cases`, color="Deceased cases"), size = 1, show.legend = F) +
      geom_line(aes(y = `Total cases`, color="Total cases"), size = 1, show.legend = F) +
      geom_point(aes(y=`Recovered cases`), size=0.5) +
      geom_point(aes(y=`Deceased cases`), size=0.5) +
      geom_point(aes(y=`Total cases`), size=0.5) +
      labs(
        x = "Dates",
        y = "Cumulative cases",
        color="Variables",
        title=paste0("Cumulative cases until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione)) +
      scale_color_manual(values = colors) +
      theme(legend.title = element_text(color="black", face = "bold"))+
      guides(color=guide_legend("Variables"))
    
    GGPlotly2<-ggplotly(Plot2)
    
    GGRglotly2 <- GGPlotly2 %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  }
  
  return(print(x = GGRglotly2))
  
}

#-------------------------------------------

RegionTS3 <- function(database, rangeDays, variable){
  options(warn=-1)
  
  regione <- unique(database$denominazione_regione)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "ricSinPrevIdxReg"
                            , "terIntPrevIdxReg"
                            , "isolDomPrevIdxReg"
                            , "totPosPrevIdxReg")]
    
    colnames(database) <- c("Regione", "Date", "Hospit. sympt. rates", "Intens. care rates", "Home isol. rates", "Tot. pos. rates")
    
    colors <- c("Hospit. sympt. rates" = "red", "Intens. care rates" = "black", "Home isol. rates" = "yellow", "Tot. pos. rates" = "blue")
    
    Plot2<-ggplot(data=database, aes(x=Date)) +
      geom_line(aes(y = `Hospit. sympt. rates`, color = "Hospit. sympt. rates"), size = 1) +
      geom_line(aes(y = `Intens. care rates`, color = "Intens. care rates"), size = 1) +
      geom_line(aes(y = `Home isol. rates`, color = "Home isol. rates"), size = 1) +
      geom_line(aes(y = `Tot. pos. rates`, color = "Tot. pos. rates"), size = 1) +
      labs(
        x = "Dates",
        y = "Rates",
        color="Legend",
        title=paste0("Rates until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione)) +
      scale_color_manual(values = colors) +
      theme(legend.title = element_text(color="black", face = "bold"))+
      guides(color=guide_legend("Variables"))
    
    GGPlotly2<-ggplotly(Plot2)
    
    GGRglotly2 <- GGPlotly2 %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "ricoverati_con_sintomi.x"
                            , "terapia_intensiva.x"
                            , "isolamento_domiciliare.x"
                            , "totale_positivi.x")]
    
    colnames(database) <- c("Regione", "Date", "Hospit. sympt. cases", "Intens. care cases", "Home isol. cases", "Tot. pos. cases")
    
    colors <- c("Hospit. sympt. cases" = "red", "Intens. care cases" = "black", "Home isol. cases" = "yellow", "Tot. pos. cases" = "blue")
    
    Plot2<-ggplot(data=database, aes(x=Date)) +
      geom_line(aes(y = `Hospit. sympt. cases`, color = "Hospit. sympt. cases"), size = 1) +
      geom_line(aes(y = `Intens. care cases`, color = "Intens. care cases"), size = 1) +
      geom_line(aes(y = `Home isol. cases`, color = "Home isol. cases"), size = 1) +
      geom_line(aes(y = `Tot. pos. cases`, color = "Tot. pos. cases"), size = 1) +
      labs(
        x = "Dates",
        y = "Cases",
        color="Variables",
        title=paste0("Cases until the "
                     , paste0(str_split(rangeDays, "-")[[1]][3]
                              , "/",  str_split(rangeDays, "-")[[1]][2])
                     , " in ", regione)) +
      scale_color_manual(values = colors) +
      theme(legend.title = element_text(color="black", face = "bold"))+
      guides(color=guide_legend("Variables"))
    
    GGPlotly2<-ggplotly(Plot2)
    
    GGRglotly2 <- GGPlotly2 %>%
      layout(xaxis = list(visible=FALSE)
             , showlegend = TRUE)
    
  }
  
  return(print(x = GGRglotly2))
  
}