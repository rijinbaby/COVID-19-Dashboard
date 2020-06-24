#########################
###  shinydashboard   ###
#########################

#https://rstudio.github.io/shinydashboard/index.html

#Options
options(warn=-1)

#Install packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, shinydashboardPlus, leaflet, tidyverse, plotly, ggplot2,
               geojsonio,sp,htmltools,shinycssloaders)

#Load Packages
{
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  #-
  library(tidyverse)
  library(leaflet)
  library(plotly)
  library(ggplot2)
  #-
  library(geojsonio)
  library(sp)
  library(htmltools)
  library(shinycssloaders)
}

#Usefull Functions
`%notin%` <- Negate(`%in%`)

#Usefull Variables
shinyPath <- "C:/Users/Rijin/Documents/Covid-19-Dashboard/"

#-----------------
# Source helper functions -----

setwd(shinyPath)  #   /srv/shiny-server/shidash_COVID_Italy

source("getDataCOVIDita.R")
downAndSaveDataPMC()
downDatiSQL()
downAndSaveDataISTAT()
cumulDeathsProv()
italy_data()

source("SIRModelParamCV_optimGG.R")
source("SIRModelParam_15gg.R")
paramComp <<- myParam_comp("Italy") #Added this line on the code
#optimal_J <<- myFindOptimalLags(paramComp$transmission) #Added this line on the code
optimal_J <<- 7
last_day <<- max(pcmTOTData$data)
SIRDParam_Dataset <<- SIRDParameterDataset()

source("mapCOVIDita.R")
source("prevIDXup2lowBuond.R")
source("timeSeriesCOVIDita.R")
#-----------------
rangeDate <<- range(unique(pcmTOTData$data)[2:length(unique(pcmTOTData$data))])
#-----------------

## Header content
source("Header.R")
## Sidebar content
source("Sidebar.R")
## Body content
source("Body.R")

# source("UI_mod.R")

ui <- dashboardPage(header, sidebar, body ) #,skin = "red"


#-----------------

server <- function(input, output, session){
  
  #"Provinces map"----
  {
    #"infobox"----
    # output$total_box <- renderValueBox({
    #   ifelse(info_box_data$confirmed_diff>=0
    #          ,valueBox(paste0(info_box_data$confirmed," (+",info_box_data$confirmed_diff,")"), "Confirmed", icon = icon("user", lib = "glyphicon"),color = "red")
    #          ,valueBox(paste0(info_box_data$confirmed," (",info_box_data$confirmed_diff,")"), "Confirmed", icon = icon("user", lib = "glyphicon"),color = "red")
    #          )
    # })
    # output$active_box <- renderValueBox({
    #   ifelse(info_box_data$active_diff>=0
    #          ,valueBox(paste0(info_box_data$active," (+",info_box_data$active_diff,")"), "Active",color = "blue")
    #          ,valueBox(paste0(info_box_data$active," (",info_box_data$active_diff,")"), "Active", color = "blue")
    #          )
    # })
    # output$Recovered_box <- renderValueBox({
    #   ifelse(info_box_data$recovered_diff>=0
    #     ,valueBox(paste0(info_box_data$recovered," (+",info_box_data$recovered_diff,")" ), "Recovered", color = "green")
    #     ,valueBox(paste0(info_box_data$recovered," (",info_box_data$recovered_diff,")" ), "Recovered", color = "green")
    #     )
    # })
    # output$deceased_box <- renderValueBox({
    #   ifelse(info_box_data$death_diff>=0
    #     ,valueBox(paste0(info_box_data$death," (+",info_box_data$death_diff,")" ), "Deceased", color = "orange")
    #     ,valueBox(paste0(info_box_data$death," (",info_box_data$death_diff,")" ), "Deceased", color = "orange")
    #   )
    # }) 
    
      # ip_date <- input$selectDate1
      # info_box_data <- italy_data(ip_date)
    output$total_box <- renderValueBox({
      if(info_box_data$confirmed_diff>=0){
      valueBox(
        paste0(info_box_data$confirmed," (+",info_box_data$confirmed_diff,")"), paste("CONFIRMED    ","Last Update:",rangeDate[2]), icon = icon("user", lib = "glyphicon"),color = "red"
        )}
        else{
          valueBox(
            paste0(info_box_data$confirmed," (",info_box_data$confirmed_diff,")"), paste("CONFIRMED    ","Last Update:",rangeDate[2]), icon = icon("user", lib = "glyphicon"),
            color = "red"
        )}
      })

    output$active_box <- renderValueBox({
      if(info_box_data$active_diff>=0){
      valueBox(
        paste0(info_box_data$active," (+",info_box_data$active_diff,")"), "ACTIVE",color = "blue"
          #icon = "fas fa-ambulance"
        )}
        else{
          valueBox(
            paste0(info_box_data$active," (",info_box_data$active_diff,")"), "ACTIVE",
            color = "blue"
        )}
    })
    output$Recovered_box <- renderValueBox({
      if(info_box_data$recovered_diff>=0){
      valueBox(
        paste0(info_box_data$recovered," (+",info_box_data$recovered_diff,")" ), "RECOVERED", color = "green"
        #icon = "fas fa-heart-broken"

      )}
      else{
        valueBox(
          paste0(info_box_data$recovered," (",info_box_data$recovered_diff,")" ), "RECOVERED", color = "green"
          #icon = "fas fa-heart-broken"
      )}
    })
    output$deceased_box <- renderValueBox({
      if(info_box_data$death_diff>=0){
      valueBox(
        paste0(info_box_data$death," (+",info_box_data$death_diff,")" ), "DECEASED", color = "orange"
        #icon = "fas fa-heart-broken"
      )}
      else{
        valueBox(
          paste0(info_box_data$death," (",info_box_data$death_diff,")" ), "DECEASED", color = "orange"
          #icon = "fas fa-heart-broken"
        )}
      })
    
    # Create the map----
    output$provinceMap <- renderLeaflet({

      selectDate <- input$selectDate1
      #   selectDate="2020-03-26"
      ProvDB <- pcmTOTData[which(pcmTOTData$data==selectDate),]

      # var <- switch(input$Province
      #               , "Cumulative Case" = "totale_casi.x"
      #               , "Cumulative Rate" = "prevIndex")
        var="totale_casi.x"

      provMap <- ProvinceMap(ProvDB, var)
      provMap

    })
    
    output$provinceMap1 <- renderLeaflet({
      
      selectDate <- input$selectDate1
      #   selectDate="2020-03-26"
      ProvDB <- pcmTOTData[which(pcmTOTData$data==selectDate),]
      
      # var <- switch(input$Province
      #               , "Cumulative Case" = "totale_casi.x"
      #               , "Cumulative Rate" = "prevIndex")
      var="prevIndex"
      
      provMap <- ProvinceMap(ProvDB, var)
      provMap
      
    })
    
    output$textPres <- renderText({
      HTML(paste0("<b>COVID-Pro: a province-based analysis for Italy<b>
                  <br />
                  <br />
                  The COVID-19 outbreak in Italy has spread mainly in northern regions, particularly in Lombardy. 
                  However, even within the same region the virus has spread irregularly from province to province, producing real epicenters of infection in some provinces but also affecting other areas with relatively lower intensity.

                  In this dashboard we present some tools for analyzing and visualizing the COVID-19 outbreak in 
                  Italy at a provincial (NUTS-3) level by integrating official data from the Italian Ministry of 
                  Health with data extracted from official press conferences of regional health authorities, 
                  especially regarding the number of deaths due to the Covid-19 which is not currently reported in 
                  official data releases. An adjusted time-dependent SIRD model is used to predict the epidemics behavior 
                  in the near future.
                  
                  Dashboard developed by: L.Ferrari, G.Gerardi, G.Manzi, A.Micheletti, F.Nicolussi, S.Salini
                  We thank students R. Baby, A. Iordache, A. Singh and N. Velardo for their contribution"))
    })
    
   
     #----
    
    # Plot the Deaths prevalence index----
    output$Drates_TS <- renderPlotly({
      
      if(nrow(cumDeathsProv)>0){
        
        if (is.null(input$inReg)&is.null(input$inProv)){
           cumDeathsProv <- cumDeathsProv
        }
        
        else if((is.null(input$inReg)&(!is.null(input$inProv)))|((!is.null(input$inReg))&(!is.null(input$inProv)))){
          prov <- input$inProv
          # updateSelectInput(input$inReg,NULL)
          cumDeathsProv <- cumDeathsProv[which(cumDeathsProv$provincia %in% prov),]
        }
        else if(!(is.null(input$inReg))&is.null(input$inProv)){
          reg <- input$inReg
          cumDeathsProv <- cumDeathsProv[which(cumDeathsProv$Regione %in% reg),]
        }
        
      }
        DR_Plot <- death_rate_home(cumDeathsProv, "PrevIDX")
        
        print(DR_Plot)
        
      }
      
    )
    
    output$textDrates <- renderText({
      HTML(paste0("<b><em>Cumulative Rate</em></b>
                  <b>are the ratio between the cumulative cases and the total population for the selected province multiplied by 100000.</b>
                  <br />
                  The results are computed using the Italian Civil Protection dataset.
                  <br />
                   <br />
                  <b><em>Death Rate</em></b>
                   <b> are the ratio between the cumulative death cases and the total population for the selected provinces multiplied by 100000.</b>
                   To build this time series we used data collected by scraping the daily press conferences and Covid-19 bulletins provided by regions.
                   <br />
                   <b> Indeed, the official data repository of the Italian Ministry of Health and the Civil Protection Agency does not provide Covid-19 data on the daily number of deaths at a provincial level, but only on a regional level.</b>"))
    })

    output$ProvList1 <- downloadHandler(
      filename = "Covid_APP_ReadMore_Provinces.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Provinces.pdf"), file)}
    )

    #----

    # Plot the National prevalence index
    output$N_TS <- renderPlotly({
      
      rangeDays <- input$selectDate1
      
      #   rangeDays="2020-04-07"
      prevIDXBound(pcmTOTData, rangeDays)
      
      if(nrow(totalPrevIDX)>0){
        IdxPlot <-  nat_min_max(totalPrevIDX)
      }
      print(IdxPlot)
      
      
#       if(nrow(totalPrevIDX)>0){
#         # title = "Cumulative rates in Italy and in the provinces with min and max values"
#         IdxPlot <- plot_ly(totalPrevIDX, x = ~data, y = ~natIDX, mode = 'lines', name = 'National') %>%
#           add_trace(y = ~upIDX, mode = 'lines', name = upperPR) %>%
#           add_trace(y = ~lowIDX, mode = 'lines', name = lowerPR) %>%
#           add_trace(y = ~natIDX, mode = 'lines', name = 'National') %>%
#           layout(title = "Comparing National Rate with worst and least affected Provinces"
#                  , xaxis = list(title = "Days")
#                  , yaxis = list (title = "Cumulative rates")
# 				 , legend=list(title=list(text='Legend')))
#         
#         IdxPlot <- IdxPlot %>% layout(legend = list(x = 0.1, y = 0.9))
# 
#         withSpinner((IdxPlot))
#         # print(IdxPlot)
#       }

    })

    #----

    #Last date Boxplot

    # output$BoxPLTy <- renderPlotly({
    # 
    #   rangeDays <- input$selectDate2
    # 
    #   lastPrevIDX <- pcmTOTData %>%
    #     ungroup() %>%
    #     filter(data==rangeDays) %>%
    #     select(prevIndex)
    # 
    # 
    #   if(nrow(lastPrevIDX)>0){
    # 
    #     lastPrIDX <- lastPrevIDX$prevIndex/sqrt(sum(lastPrevIDX$prevIndex^2))
    # 
    #     IdxBox <- plot_ly(y = lastPrIDX, type = "box") %>%
    #       layout(autosize = T, height = 290
    #              , title = "Normalized rates")
    # 
    #     print(IdxBox)
    # 
    #   }
    # 
    # })

  }

  #"Time Series"----
  {
    # Plot the provinces time series----
    output$P_TS <- renderPlotly({
      
      # selectDate <- input$selectDate1
      #   # selectDate="2020-05-26"
      # timeSeriesCOVIDpr <- pcmTOTData[which(pcmTOTData$data<=selectDate),]
      
      reg <- input$inReg
      varTSp <- input$inProv
      #   varTSp="Piacenza"
      # varTSdata <- switch(input$inVarP,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      varTSdata="Cases"
      
      
      if (is.null(input$inReg)&is.null(input$inProv)){
        timeSeriesCOVIDpr <- pcmTOTData %>%
            ungroup() %>%
            select(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)%>%
            group_by(denominazione_regione
                     , denominazione_provincia
                     , data
                     , totale_casi.x
                     , prevIndex)
        }

        else if((is.null(input$inReg)&(!is.null(input$inProv)))|((!is.null(input$inReg))&(!is.null(input$inProv)))){
          
          timeSeriesCOVIDpr <- subset(pcmTOTData
                                      , (denominazione_provincia%in%c(varTSp)))%>%
            ungroup() %>%
            select(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)%>%
            group_by(denominazione_regione
                     , denominazione_provincia
                     , data
                     , totale_casi.x
                     , prevIndex)
        }
      else if(!(is.null(input$inReg))&is.null(input$inProv)){
        
        timeSeriesCOVIDpr <- subset(pcmTOTData
                                    , (denominazione_regione%in%c(reg)))%>%
          ungroup() %>%
          select(denominazione_regione
                 , denominazione_provincia
                 , data
                 , totale_casi.x
                 , prevIndex)%>%
          group_by(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)
      }
      
      
      # timeSeriesCOVIDpr <- subset(pcmTOTData
      #                             , (denominazione_provincia%in%c(varTSp)))%>%
      #   ungroup() %>%
      #   select(denominazione_regione
      #          , denominazione_provincia
      #          , data
      #          , totale_casi.x
      #          , prevIndex)%>%
      #   group_by(denominazione_regione
      #            , denominazione_provincia
      #            , data
      #            , totale_casi.x
      #            , prevIndex)

      if(nrow(timeSeriesCOVIDpr)>0){

        P1 <- ProvinceTS(timeSeriesCOVIDpr, varTSdata)
        
        print(P1)

      }

    })
    
    output$P_TS2 <- renderPlotly({
      
      # selectDate <- input$selectDate1
      # #   selectDate="2020-03-26"
      # timeSeriesCOVIDpr <- pcmTOTData[which(pcmTOTData$data<=selectDate),]
      
      reg <- input$inReg
      varTSp <- input$inProv
      #   varTSp="Piacenza"
      # varTSdata <- switch(input$inVarP,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      
      varTSdata="PrevIDX"
      
      if (is.null(input$inReg)&is.null(input$inProv)){
        timeSeriesCOVIDpr <- pcmTOTData %>%
          ungroup() %>%
          select(denominazione_regione
                 , denominazione_provincia
                 , data
                 , totale_casi.x
                 , prevIndex)%>%
          group_by(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)
      }
      
      else if((is.null(input$inReg)&(!is.null(input$inProv)))|((!is.null(input$inReg))&(!is.null(input$inProv)))){
        
        timeSeriesCOVIDpr <- subset(pcmTOTData
                                    , (denominazione_provincia%in%c(varTSp)))%>%
          ungroup() %>%
          select(denominazione_regione
                 , denominazione_provincia
                 , data
                 , totale_casi.x
                 , prevIndex)%>%
          group_by(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)
      }
      else if(!(is.null(input$inReg))&is.null(input$inProv)){
        
        timeSeriesCOVIDpr <- subset(pcmTOTData
                                    , (denominazione_regione%in%c(reg)))%>%
          ungroup() %>%
          select(denominazione_regione
                 , denominazione_provincia
                 , data
                 , totale_casi.x
                 , prevIndex)%>%
          group_by(denominazione_regione
                   , denominazione_provincia
                   , data
                   , totale_casi.x
                   , prevIndex)
      }
      
      
      # timeSeriesCOVIDpr <- subset(pcmTOTData
      #                             , (denominazione_provincia%in%c(varTSp)))%>%
      #   ungroup() %>%
      #   select(denominazione_regione
      #          , denominazione_provincia
      #          , data
      #          , totale_casi.x
      #          , prevIndex)%>%
      #   group_by(denominazione_regione
      #            , denominazione_provincia
      #            , data
      #            , totale_casi.x
      #            , prevIndex)
      
      if(nrow(timeSeriesCOVIDpr)>0){
        
        P1 <- ProvinceTS(timeSeriesCOVIDpr, varTSdata)
        
        print(P1)
        
      }
      
    })
    
    output$textPLOT1 <- renderText({
      HTML(paste0("<b>Please, select more than one province. It is possible to plot <em> cumulative cases </em> or <em> cumulative rates </em>.</b>"
                  , "<em> Cumulative rates</em>"
                  , " are the ratio between the cumulative cases and the total population for the selected province multiplied by 100000."))
    })
    
    output$PLOT1 <- downloadHandler(
      filename = "Covid_APP_ReadMore_Timeseries_Plot1.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Timeseries_Plot1.pdf"), file)}
    )

    #----

    # Plot the regions1 time series----
    # output$R_TS1 <- renderPlotly({
    # 
    #   varTSr <- input$inReg
    #   #   varTSr="Lombardia"
    #   varTSdata <- switch(input$inVarR,
    #                       "Cumulative cases" = "Cases",
    #                       "Cumulative rates" = "PrevIDX")
    #   #   varTSdata="Cases"
    #   rangeRGDays <- rangeDate[2]
    # 
    #   timeSeriesCOVIDrg1 <- subset(pcmTOTData
    #                               , (denominazione_regione%in%c(varTSr)))%>%
    #     ungroup() %>%
    #     select(denominazione_regione
    #            , denominazione_provincia
    #            , data
    #            , totale_casi.x
    #            , prevIndex)%>%
    #     group_by(denominazione_regione
    #              , denominazione_provincia
    #              , data
    #              , totale_casi.x
    #              , prevIndex)%>%
    #     filter(data<=rangeRGDays)
    # 
    #   #sum(timeSeriesCOVIDrg$totale_casi.x[timeSeriesCOVIDrg$data==rangeRGDays])
    # 
    #   if(nrow(timeSeriesCOVIDrg1)>0){
    # 
    #     R1 <- RegionTS1(timeSeriesCOVIDrg1, rangeRGDays, varTSdata)
    # 
    #     print(R1)
    # 
    #   }
    # 
    # })
    
    # Plot the regions2 time series----
    output$R_TS2 <- renderPlotly({
      
      varTSr <- input$inReg
      #   varTSr="Lombardia"
      # varTSdata <- switch(input$inVarR,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      
      varTSdata="Cases"
      rangeRGDays <- rangeDate[2]
     
      timeSeriesCOVIDrg2 <- subset(pcmTOTData
                                  , (denominazione_regione%in%c(varTSr)))%>%
        ungroup() %>%
        select(denominazione_regione
               , data
               , dimessi_guariti.x
               , deceduti.x
               , totale_casi.y
               #---
               # , "ricoverati_con_sintomi.x"
               # , "terapia_intensiva.x"
               # , "isolamento_domiciliare.x"
               # , "totale_positivi.x"
               #---
               , guarPrevIdxReg
               , decePrevIdxReg
               , actualPrevIdxReg)%>%
        dplyr::distinct(denominazione_regione
                 , data
                 , dimessi_guariti.x
                 , deceduti.x
                 , totale_casi.y
                 , guarPrevIdxReg
                 , decePrevIdxReg
                 , actualPrevIdxReg)%>%
        filter(data<=rangeRGDays)
      
      if(nrow(timeSeriesCOVIDrg2)>0){
        
        R2 <- RegionTS2(timeSeriesCOVIDrg2, rangeRGDays, varTSdata)
        
        print(R2)
        
      }
      
    })
    
    output$R_TS2_2 <- renderPlotly({
      
      varTSr <- input$inReg
      #   varTSr="Lombardia"
      # varTSdata <- switch(input$inVarR,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      
      varTSdata="PrevIDX"
      rangeRGDays <- rangeDate[2]
      
      timeSeriesCOVIDrg2 <- subset(pcmTOTData
                                   , (denominazione_regione%in%c(varTSr)))%>%
        ungroup() %>%
        select(denominazione_regione
               , data
               , dimessi_guariti.x
               , deceduti.x
               , totale_casi.y
               #---
               # , "ricoverati_con_sintomi.x"
               # , "terapia_intensiva.x"
               # , "isolamento_domiciliare.x"
               # , "totale_positivi.x"
               #---
               , guarPrevIdxReg
               , decePrevIdxReg
               , actualPrevIdxReg)%>%
        dplyr::distinct(denominazione_regione
                        , data
                        , dimessi_guariti.x
                        , deceduti.x
                        , totale_casi.y
                        , guarPrevIdxReg
                        , decePrevIdxReg
                        , actualPrevIdxReg)%>%
        filter(data<=rangeRGDays)
      
      if(nrow(timeSeriesCOVIDrg2)>0){
        
        R2 <- RegionTS2(timeSeriesCOVIDrg2, rangeRGDays, varTSdata)
        
        print(R2)
        
      }
      
    })
    
    # Plot the regions3 time series----
    output$R_TS3 <- renderPlotly({
      
      varTSr <- input$inReg
      #   varTSr="Lombardia"
      # varTSdata <- switch(input$inVarR,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      varTSdata="Cases"
      rangeRGDays <- rangeDate[2]
      
      timeSeriesCOVIDrg3 <- subset(pcmTOTData
                                   , (denominazione_regione%in%c(varTSr)))%>%
        ungroup() %>%
        select(denominazione_regione
               , data
               , ricoverati_con_sintomi.x
               , terapia_intensiva.x
               , isolamento_domiciliare.x
               , totale_positivi.x
               #---
               , ricSinPrevIdxReg
               , terIntPrevIdxReg
               , isolDomPrevIdxReg
               , totPosPrevIdxReg)%>%
        dplyr::distinct(denominazione_regione
                        , data
                        , ricoverati_con_sintomi.x
                        , terapia_intensiva.x
                        , isolamento_domiciliare.x
                        , totale_positivi.x
                        #---
                        , ricSinPrevIdxReg
                        , terIntPrevIdxReg
                        , isolDomPrevIdxReg
                        , totPosPrevIdxReg)%>%
        filter(data<=rangeRGDays)
      
      if(nrow(timeSeriesCOVIDrg3)>0){
        
        R3 <- RegionTS3(timeSeriesCOVIDrg3, rangeRGDays, varTSdata)
        
        print(R3)
        
      }
      
    })
    
    output$R_TS3_2 <- renderPlotly({
      
      varTSr <- input$inReg
      #   varTSr="Lombardia"
      # varTSdata <- switch(input$inVarR,
      #                     "Cumulative cases" = "Cases",
      #                     "Cumulative rates" = "PrevIDX")
      varTSdata="PrevIDX"
      rangeRGDays <- rangeDate[2]
      
      timeSeriesCOVIDrg3 <- subset(pcmTOTData
                                   , (denominazione_regione%in%c(varTSr)))%>%
        ungroup() %>%
        select(denominazione_regione
               , data
               , ricoverati_con_sintomi.x
               , terapia_intensiva.x
               , isolamento_domiciliare.x
               , totale_positivi.x
               #---
               , ricSinPrevIdxReg
               , terIntPrevIdxReg
               , isolDomPrevIdxReg
               , totPosPrevIdxReg)%>%
        dplyr::distinct(denominazione_regione
                        , data
                        , ricoverati_con_sintomi.x
                        , terapia_intensiva.x
                        , isolamento_domiciliare.x
                        , totale_positivi.x
                        #---
                        , ricSinPrevIdxReg
                        , terIntPrevIdxReg
                        , isolDomPrevIdxReg
                        , totPosPrevIdxReg)%>%
        filter(data<=rangeRGDays)
      
      if(nrow(timeSeriesCOVIDrg3)>0){
        
        R3 <- RegionTS3(timeSeriesCOVIDrg3, rangeRGDays, varTSdata)
        
        print(R3)
        
      }
      
    })
    
    # output$textPLOT2 <- renderText({
    #   HTML(paste0("<b>Please, select one region. It is possible to plot <em> cumulative cases </em> or <em> cumulative rates </em>.</b>"
    #               , "<em> Cumulative rates</em>"
    #               , " are the ratio between the cumulative cases and the total population for the selected province multiplied by 100000."))
    # })
    # 
    # output$PLOT2 <- downloadHandler(
    #   filename = "Covid_APP_ReadMore_Timeseries_Plot2.pdf",
    #   content = function(file) {
    #     file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Timeseries_Plot2.pdf"), file)}
    # )
      
    #----
    
    #Deaths Time Series----
    output$D_TS <- renderPlotly({

      varTSd <- input$inProvD
      # varTSd="Piacenza"
      deathDF <- deathDatesReg[which(deathDatesReg$province==varTSd),]

      if(nrow(deathDF)>0){
        
        maxData <- str_split(max(deathDF$data), "-")[[1]]
        maxData <- paste(maxData[3], maxData[2], sep = "/")

        deathDF$data <- as.Date(deathDF$data)
        
        deathPlolt <- plot_ly(deathDF, x = ~data, y = ~death15, type = 'scatter', mode = 'lines', name = '2015'
                              , line = list(color = 'rgb(220, 220, 220)', width = 2, dash = 'dash'))%>%
          add_trace(y = ~death16, type = 'scatter', mode = 'lines', name = '2016'
                    , line = list(color = 'rgb(220, 220, 220)', width = 2, dash = 'dash')) %>%
          add_trace(y = ~death17, type = 'scatter', mode = 'lines', name = '2017'
                    , line = list(color = 'rgb(220, 220, 220)', width = 2, dash = 'dash')) %>%
          add_trace(y = ~death18, type = 'scatter', mode = 'lines', name = '2018'
                    , line = list(color = 'rgb(220, 220, 220)', width = 2, dash = 'dash')) %>%
          add_trace(y = ~death19, type = 'scatter', mode = 'lines', name = '2019'
                    , line = list(color = 'rgb(220, 220, 220)', width = 2, dash = 'dash')) %>%
          add_trace(y = ~death20, type = 'scatter', mode = 'lines', name = '2020'
                    , line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'line')) %>%
          add_trace(y = ~weekDeaths, type = 'scatter', mode = 'lines', name = 'COVID deaths'
                    , line = list(color = 'rgb(0, 0, 0)', width = 2, dash = 'line')) %>%
          layout(
            title = paste0("Time series of weekly deaths by year in ", unique(deathDF$province),  " until the ", maxData)
            , xaxis = list( type = 'date'
							, tickformat = "%d/%m"
							, title = "Date")
            , yaxis = list(title = "Death_Count")
			, legend=list(title=list(text='Year')) #'<b> Year </b>'
          )

      }

      print(deathPlolt)

    })
    
    output$textPLOT3 <- renderText({
      HTML(paste0("<b>ISTAT data about weekly deaths are plotted</b> (according to the ISTAT dataset only some municipalities are present in the count)<b>. For some provinces</b> (see the list below)<b> also the deaths for Covid19 are plotted.</b>"))
    })
    
    output$PLOT3 <- downloadHandler(
      filename = "Covid_APP_ReadMore_Timeseries_Plot3.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Timeseries_Plot3.pdf"), file)}
    )
    
    output$ProvList <- downloadHandler(
      filename = "Covid_APP_ReadMore_Provinces.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Provinces.pdf"), file)}
    )

    #----
  }
  
  #SIRD Models----
  {
    # Province SIRD----
    output$SIRDp <- renderPlotly({

      varSIRpCV <- input$SIRprovCV
      # varSIRpCV = "Torino"
      varLagpCV <- input$LagDaysCV
      # varLagpCV = 7

      #varCutDayCV <- as.numeric(input$CutDayCV)-as.numeric(as.Date("2020-02-23"))
      # varCutDayCV=FALSE

      # SIRDresults <- myTD_SIRD_model(prov=varSIRpCV, optJ=varLagpCV )
      # SIRD_CV_plotP <- mySIRD_plot(results=SIRDresults, Province=varSIRpCV, j=varLagpCV )
      
      model_Trainer <- modelTrainer(parComp=paramComp, J=varLagpCV)
      SIRD_CV_plotP <- SIRD_plot(region=varSIRpCV, J=varLagpCV, models=model_Trainer, n_days=15)

      SIRD_CV_plotP

    })

    output$textSIRD1 <- renderText({
      HTML(paste0("<b>The SIRD model describes the development of an epidemiological curve. 
                  <br />
                  <br />
                  In this application, the model parameters are considered dynamic. The number of lags represent the number of days used in the autoregressive models for the parameters. 
                  <br />
                  <br />
                  This is a preliminary model whose uncertainty grows as the number of days goes by.</b>"))
    })

    #----

    # SIRD time series----
    output$SIRDts <- renderPlotly({

      varSIRpCV <- input$SIRprovCV
      # varSIRpCV = "Torino"
      varLagpCV <- input$LagDaysCV
      # varLagpCV = 7

      #varCutDayCV <- as.numeric(input$CutDayCV)-as.numeric(as.Date("2020-02-23"))
      # varCutDayCV=FALSE

      # SIRDresults <- myTD_SIRD_model(prov=varSIRpCV, optJ=varLagpCV )
      # SIRD_CV_TimeS <- myTimeSeries(results=SIRDresults, Province=varSIRpCV, j=varLagpCV)
      
      model_Trainer <- modelTrainer(parComp=paramComp, J=varLagpCV)
      SIRD_CV_TimeS <- SIRDtimeSeries(region=varSIRpCV, J=varLagpCV, models=model_Trainer, d=15)

      SIRD_CV_TimeS

    })

    output$textSIRD2 <- renderText({
      HTML(paste0("<b>These plots show the time series of the model's parameters. 
                  <br />
                  <br />
      The transmission rate is the number of effective infected contacts by a single case per unit in time. The recovery/mortality rates are the rates at which infectious people leave the currently infected group and become recovered/deceased.</b>
                  <br />
                  <br />
      <b>R0 is the basic reproduction number, the expected number of secondary cases from an infected individual, in a population where all individuals are susceptible. 
                  <br />
                  <br />
                  The dashed line in the R0 plot is the threshold at which the parameter equals 1: when R0 is below this value, the disease will eventually be controlled.</b>"))
    })

    output$readSIRD <- downloadHandler(
      filename = "SIRD_Model_PDF.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/SIRD_Model_PDF.pdf"), file)}
    )

    output$ProvListSIRD <- downloadHandler(
      filename = "Covid_APP_ReadMore_Provinces.pdf",
      content = function(file) {
        file.copy(paste0(shinyPath, "data/PDF/Covid_APP_ReadMore_Provinces.pdf"), file)}
    )
    #----
    output$textSIRD3 <- renderText({
      HTML(paste0("<b>This plot shows the current values of a given parameter in each province at a certain point in time.
                  <br />
                  <br />
                  The data are computed daily and smoothed using a moving average on a 7-day period. 
                  <br />
                  <br />
                  Only the provinces for which the data are available are shown.</b>"))
    })

    output$SIRDMap <- renderLeaflet({

      selectDateSIRD <- input$selectDateSIRD
      #   selectDateSIRD="2020-05-04"
      ProvDB_SIRD <- SIRDParam_Dataset[which(SIRDParam_Dataset$date==selectDateSIRD),]

      varMapSIRD <- input$varMapSIRD
      #   varMapSIRD="Basic Reproduction Number"

      SIRDprovMap <- SIRD_Map(database=ProvDB_SIRD, var=varMapSIRD)
      SIRDprovMap

    })

  }

}

#-----------------

shinyApp(ui, server)

#-----------------
#-----------------
#-----------------

