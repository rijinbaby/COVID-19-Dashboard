#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, plotly)

#Load Packages
library(tidyverse)
library(ggplot2)
library(plotly)

# database = cumDeathsProv


# national , min max ------------------------------------------------------

nat_min_max <- function(totalPrevIDX){
  options(warn=-1)
  
  IdxPlot <- plot_ly(totalPrevIDX, x = ~data, y = ~natIDX, mode = 'lines', name = 'National') %>%
    add_trace(y = ~upIDX, mode = 'lines', name = upperPR) %>%
    add_trace(y = ~lowIDX, mode = 'lines', name = lowerPR) %>%
    add_trace(y = ~natIDX, mode = 'lines', name = 'National') %>%
    layout(title = "Comparing National Rate with worst and least affected Provinces"
           , xaxis = list(title = "Days")
           , yaxis = list (title = "Cumulative rates")
           , legend=list(title=list(text='Legend')))
  
  IdxPlot <- IdxPlot %>% layout(legend = list(x = 0.1, y = 0.9))
  
  return(print(x = IdxPlot))
}

# death_rate_home chart ---------------------------------------------------

death_rate_home <- function(database, variable){
  options(warn=-1)
  
  untilDate <- max(database$data)
  
  # if(variable=="PrevIDX"){
    
    database <- database[,c("provincia",  "data", "deathRates")]
    
    colnames(database) <- c("Province", "Date", "Death_rates")
    
    n_plot <- database %>%
      plotly::plot_ly()
    
    n_plot <- n_plot %>%
        plotly::add_trace(x = ~Date, y=~Death_rates,type='scatter', mode='lines', color=~Province,text=~Province,
                          hovertemplate = paste(
                                                 "%{text}<br>",
                                                  "Date: %{x}<br>",
                                                  "Death Rate: %{y}")) %>% 
       plotly::layout(title = "Province Level Death Rate Comparission"
                      , xaxis = list(title = "Days")
                      , yaxis = list (title = "Death Rate"))
        
    n_plot <- n_plot %>% layout(legend = list(x = 100, y = 0.5))
      
      
    return(print(x = n_plot))
    
    # title=paste0("Death rates until the "
    #              , paste0(str_split(untilDate, "-")[[1]][3]
    #                       , "/",  str_split(untilDate, "-")[[1]][2])
    #              , " at provincial level")) +
    
    # Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
    #   geom_line(aes(y=`Death rates`)) +
    #   geom_point(aes(y=`Death rates`), size=1) +
    #   labs(
    #     x = "Dates",
    #     #y = "Death rates",
    #     color="Provinces",
    #     title=paste0("Province Level Death Rates")) +
    #   theme(legend.position = "none"
    #         ,legend.title = element_text(colour="black", face = "bold"))
    # 
    # GGPlotly<-ggplotly(Plot)
    # 
    # GGPrlotly <- GGPlotly %>%
    #   layout(xaxis = list(visible=FALSE))
    # 
    # return(print(x = GGPrlotly))
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
   
    colnames(database) <- c("Province", "Date", "Cumulative_rates")
    
    n_plot <- database %>%
      plotly::plot_ly()
    
    n_plot <- n_plot %>%
      plotly::add_trace(x = ~Date, y=~Cumulative_rates, type='scatter', mode='lines',color=~Province,text=~Province,
                        hovertemplate = paste(
                          "%{text}<br>",
                          "Date: %{x}<br>",
                          "Cumulative Rate.: %{y}")) %>% 
      plotly::layout(xaxis = list(title = "Days")
                   , yaxis = list (title = "Death Rates"))
      # plotly::layout(title=paste0("Cumulative rates until the "
      #                                                  , paste0(str_split(untilDate, "-")[[1]][3]
      #                                                           , "/",  str_split(untilDate, "-")[[1]][2])))
    
#     Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
#       geom_line(aes(y=`Cumulative rates`)) +
#       geom_point(aes(y=`Cumulative rates`), size=1) +
#       labs(
#         x = "Dates",
#         y = "Cumulative rates",
#         color="Provinces",
#         title=paste0("Cumulative rates until the "
#                      , paste0(str_split(untilDate, "-")[[1]][3]
#                               , "/",  str_split(untilDate, "-")[[1]][2])
#                      , " in the provinces selected")) +
# 					 theme(legend.title = element_text(colour="black", face = "bold"))
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_provincia",  "data", "totale_casi.x")]
    
    colnames(database) <- c("Province", "Date", "Total_Cases")
    
    n_plot <- database %>%
      plotly::plot_ly()
    
    n_plot <- n_plot %>%
      plotly::add_trace(x = ~Date, y=~Total_Cases,type='scatter',mode='lines',color=~Province,text=~Province,
                        hovertemplate = paste(
                          "%{text}<br>",
                          "Date: %{x}<br>",
                          "Total Cases.: %{y}")) %>%
      plotly::layout(xaxis = list(title = "Days")
                     , yaxis = list (title = "Total Cases"))
    
      # plotly::layout(title=paste0("Total Cases rates until the "
      #                             , paste0(str_split(untilDate, "-")[[1]][3]
      #                                      , "/",  str_split(untilDate, "-")[[1]][2])))
    
#     Plot<-ggplot(data=database, aes(x=Date, color=Province)) +
#       geom_line(aes(y=`Total Cases`)) +
#       geom_point(aes(y=`Total Cases`), size=1) +
#       labs(
#         x = "Dates",
#         y = "Cumulative cases",
#         color="Provinces",
#         title=paste0("Cumulative cases until the "
#                      , paste0(str_split(untilDate, "-")[[1]][3]
#                               , "/",  str_split(untilDate, "-")[[1]][2])
#                      , " in the provinces selected")) +
# 					 theme(legend.title = element_text(colour="black", face = "bold"))
    
  }
  
  n_plot <- n_plot %>% layout(legend = list(x = 100, y = 0.5))
  
  return(print(x = n_plot))

  # GGPlotly<-ggplotly(Plot)
  # 
  # GGPrlotly <- GGPlotly %>%
  #   layout(xaxis = list(visible=FALSE))

  # return(print(x = GGPrlotly))
  
}

# RTS1-------------------------------------------

# RegionTS1 <- function(database, rangeDays, variable){
#   options(warn=-1)
#   
#   regione <- unique(database$denominazione_regione)
#   
#   if(variable=="PrevIDX"){
#     
#     database <- database[,c("denominazione_regione"
#                             , "denominazione_provincia"
#                             , "data"
#                             , "prevIndex")]
#     
#     colnames(database) <- c("Regione", "Provincia", "Date", "Cumulative rates")
#     
#     Plot<-ggplot(data=database, aes(x=Date, color=Provincia)) +
#       geom_line(aes(y=`Cumulative rates`)) +
#       labs(
#         x = "Dates",
#         y = "Cumulative rates",
#         color="Province",
#         title=paste0("Cumulative rates until the "
#                      , paste0(str_split(rangeDays, "-")[[1]][3]
#                               , "/",  str_split(rangeDays, "-")[[1]][2])
#                      , " in ", regione, " by province")) +
# 					 theme(legend.title = element_text(color="black", face = "bold"))
#     
#     GGPlotly<-ggplotly(Plot)
#     
#     GGRglotly <- GGPlotly %>%
#       layout(xaxis = list(visible=FALSE)
#              , showlegend = TRUE)
#     
#   } else if(variable=="Cases"){
#     
#     database <- database[,c("denominazione_regione"
#                             , "denominazione_provincia"
#                             , "data"
#                             , "totale_casi.x")]
#     
#     colnames(database) <- c("Regione", "Provincia", "Date", "Province Cases")
#     
#     Plot<-ggplot(data=database, aes(x=Date, color=Provincia)) +
#       geom_line(aes(y=`Province Cases`)) +
#       labs(
#         x = "Dates",
#         y = "Cumulative cases",
#         color="Province",
#         title=paste0("Cumulative cases until the "
#                      , paste0(str_split(rangeDays, "-")[[1]][3]
#                               , "/",  str_split(rangeDays, "-")[[1]][2])
#                      , " in ", regione, " by province")) +
# 					 theme(legend.title = element_text(color="black", face = "bold"))
#     
#     GGPlotly<-ggplotly(Plot)
#     
#     GGRglotly <- GGPlotly %>%
#       layout(xaxis = list(visible=FALSE)
#              , showlegend = TRUE)
#     
#   }
#   
#   return(print(x = GGRglotly))
#   
# }

#RTS2-------------------------------------------

RegionTS2 <- function(database, rangeDays, variable){
  options(warn=-1)
  
  regione <- unique(database$denominazione_regione)
  
  if(variable=="PrevIDX"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "guarPrevIdxReg"
                            , "decePrevIdxReg"
                            , "actualPrevIdxReg")]
    
    colnames(database) <- c("Regione", "Date", "Recovered_rates", "Deceased_rates", "Total_rates")
    
    # colors <- c("Recovered rates" = "green", "Deceased rates" = "black", "Total rates" = "red")
    
    n_plot <- plotly::plot_ly(database,x = ~Date,y=~Recovered_rates,mode='lines',name ='Recovered',
                                           hovertemplate = paste("Date: %{x}<br>","Recovered Rate: %{y}")) %>%
                      add_trace(y=~Deceased_rates,mode='lines',name='Deceased',
                                          hovertemplate = paste("Date: %{x}<br>","Deceased Rate: %{y}")) %>%
                      add_trace(y=~Total_rates,mode='lines',name='Total',
                                          hovertemplate = paste("Date: %{x}<br>","Total Rate: %{y}")) %>%
                      add_trace(y=~Recovered_rates,mode='lines',name='Recovered Rate') %>%
                      layout(title = paste0("Cumulative Rates of ",regione)
                             , xaxis = list(title = "Days")
                             , yaxis = list (title = "Rates")
                             , legend=list(x = 0.1, y = 0.9))
                                                       # , paste0(str_split(rangeDays, "-")[[1]][3]
                                                       # , "/",  str_split(rangeDays, "-")[[1]][2])
                                                       # , " in ", regione))
                                       
    # n_plot <- n_plot %>% layout(legend = list(x = 0.1, y = 0.9))
    
    
    # Plot2<-ggplot(data=database, aes(x=Date)) +
    #   geom_line(aes(y = `Recovered rates`, color="Recovered rates"), size = 1) +
    #   geom_line(aes(y = `Deceased rates`, color="Deceased rates"), size = 1) +
    #   geom_line(aes(y = `Total rates`, color="Total rates"), size = 1) +
    #   geom_point(aes(y=`Recovered rates`), size=0.5) +
    #   geom_point(aes(y=`Deceased rates`), size=0.5) +
    #   geom_point(aes(y=`Total rates`), size=0.5) +
    #   labs(
    #     x = "Dates",
    #     y = "Cumulative rates",
    #     color="Legend",
    #     title=paste0("Cumulative rates until the "
    #                  , paste0(str_split(rangeDays, "-")[[1]][3]
    #                           , "/",  str_split(rangeDays, "-")[[1]][2])
    #                  , " in ", regione)) +
    #   scale_color_manual(values = colors) +
    #   theme(legend.title = element_text(color="black", face = "bold"))+
    #   guides(color=guide_legend("Variables"))
    # 
    # GGPlotly2<-ggplotly(Plot2)
    # 
    # GGRglotly2 <- GGPlotly2 %>%
    #   layout(xaxis = list(visible=FALSE)
    #          , showlegend = TRUE)
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "dimessi_guariti.x"
                            , "deceduti.x"
                            , "totale_casi.y")]
    
    colnames(database) <- c("Regione", "Date", "Recovered_cases", "Deceased_cases", "Total_cases")
    
    # colors <- c("Recovered cases" = "green", "Deceased cases" = "black", "Total cases" = "red")
    
    
    n_plot <- plotly::plot_ly(database,x = ~Date,y=~`Recovered_cases`,type='scatter',mode='lines',name='Recovered',
                                          hovertemplate = paste("Date: %{x}<br>","Recovered Case: %{y}")) 
    n_plot <- n_plot %>%plotly::add_trace(y=~Deceased_cases,type='scatter',mode='lines',name='Deceased',
                                          hovertemplate = paste("Date: %{x}<br>","Deceased Case: %{y}"))
    n_plot <- n_plot %>%plotly::add_trace(y=~Total_cases,type='scatter',mode='lines',name='Total',
                                          hovertemplate = paste("Date: %{x}<br>","Total Case: %{y}")) 
    
    
    n_plot <- n_plot %>%plotly::layout(title = paste0("Total Cases of ",regione)
                                       , xaxis = list(title = "Days")
                                       , yaxis = list (title = "Cases")
                                                      # , paste0(str_split(rangeDays, "-")[[1]][3]
                                                      #          , "/",  str_split(rangeDays, "-")[[1]][2])
                                                      # , " in ", regione)
                                       ,legend = list(x = 0.1, y = 0.9))
    
    
    # Plot2<-ggplot(data=database, aes(x=Date)) +
    #   geom_line(aes(y = `Recovered cases`, color="Recovered cases"), size = 1, show.legend = F) +
    #   geom_line(aes(y = `Deceased cases`, color="Deceased cases"), size = 1, show.legend = F) +
    #   geom_line(aes(y = `Total cases`, color="Total cases"), size = 1, show.legend = F) +
    #   geom_point(aes(y=`Recovered cases`), size=0.5) +
    #   geom_point(aes(y=`Deceased cases`), size=0.5) +
    #   geom_point(aes(y=`Total cases`), size=0.5) +
    #   labs(
    #     x = "Dates",
    #     y = "Cumulative cases",
    #     color="Variables",
    #     title=paste0("Cumulative cases until the "
    #                  , paste0(str_split(rangeDays, "-")[[1]][3]
    #                           , "/",  str_split(rangeDays, "-")[[1]][2])
    #                  , " in ", regione)) +
    #   scale_color_manual(values = colors) +
    #   theme(legend.title = element_text(color="black", face = "bold"))+
    #   guides(color=guide_legend("Variables"))
    # 
    # GGPlotly2<-ggplotly(Plot2)
    # 
    # GGRglotly2 <- GGPlotly2 %>%
    #   layout(xaxis = list(visible=FALSE)
    #          , showlegend = TRUE)
    
  }
  
  return(print(x = n_plot))
  
}

#RTS3-------------------------------------------

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
    
    # colnames(database) <- c("Regione", "Date", "Hospit. sympt. rates", "Intens. care rates", "Home isol. rates", "Tot. pos. rates")
    
    colnames(database) <- c("Regione", "Date", "Hospitalized", "ICU", "Home_Isolation", "Total_Positive")
    
    # colors <- c("Hospit. sympt. rates" = "red", "Intens. care rates" = "black", "Home isol. rates" = "yellow", "Tot. pos. rates" = "blue")
    
    n_plot <- plotly::plot_ly(database,x = ~Date,y=~Hospitalized,type='scatter',mode='lines',name='Hospitalized',
                              hovertemplate = paste("Date: %{x}<br>","Hospitalized: %{y}")) 
    n_plot <- n_plot %>%plotly::add_trace(y=~ICU,type='scatter',mode='lines',name='ICU',
                                          hovertemplate = paste("Date: %{x}<br>","ICU: %{y}"))
    n_plot <- n_plot %>%plotly::add_trace(y=~Home_Isolation,type='scatter',mode='lines',name='Home Isolation',
                                          hovertemplate = paste("Date: %{x}<br>","Home Isolation: %{y}"))
    n_plot <- n_plot %>%plotly::add_trace(y=~Total_Positive,type='scatter',mode='lines',name='Total Positive',
                                          hovertemplate = paste("Date: %{x}<br>","Total Positive: %{y}")) 
    
    
    n_plot <- n_plot %>%plotly::layout(title = paste0("Daily Rates of ",regione)
                                       , xaxis = list(title = "Days")
                                       , yaxis = list (title = "Rates")
                                       # , paste0(str_split(rangeDays, "-")[[1]][3]
                                       #          , "/",  str_split(rangeDays, "-")[[1]][2])
                                       # , " in ", regione)
                                       ,legend = list(x = 0.1, y = 0.9))
    
    # Plot2<-ggplot(data=database, aes(x=Date)) +
    #   geom_line(aes(y = `Hospit. sympt. rates`, color = "Hospit. sympt. rates"), size = 1) +
    #   geom_line(aes(y = `Intens. care rates`, color = "Intens. care rates"), size = 1) +
    #   geom_line(aes(y = `Home isol. rates`, color = "Home isol. rates"), size = 1) +
    #   geom_line(aes(y = `Tot. pos. rates`, color = "Tot. pos. rates"), size = 1) +
    #   labs(
    #     x = "Dates",
    #     y = "Rates",
    #     color="Legend",
    #     title=paste0("Rates until the "
    #                  , paste0(str_split(rangeDays, "-")[[1]][3]
    #                           , "/",  str_split(rangeDays, "-")[[1]][2])
    #                  , " in ", regione)) +
    #   scale_color_manual(values = colors) +
    #   theme(legend.title = element_text(color="black", face = "bold"))+
    #   guides(color=guide_legend("Variables"))
    # 
    # GGPlotly2<-ggplotly(Plot2)
    # 
    # GGRglotly2 <- GGPlotly2 %>%
    #   layout(xaxis = list(visible=FALSE)
    #          , showlegend = TRUE)
    
  } else if(variable=="Cases"){
    
    database <- database[,c("denominazione_regione"
                            , "data"
                            , "ricoverati_con_sintomi.x"
                            , "terapia_intensiva.x"
                            , "isolamento_domiciliare.x"
                            , "totale_positivi.x")]
    
    # colnames(database) <- c("Regione", "Date", "Hospit. sympt. cases", "Intens. care cases", "Home isol. cases", "Tot. pos. cases")
    
    colnames(database) <- c("Regione", "Date", "Hospitalized", "ICU", "Home_Isolation", "Total_Positive")
    
    # colors <- c("Hospit. sympt. cases" = "red", "Intens. care cases" = "black", "Home isol. cases" = "yellow", "Tot. pos. cases" = "blue")
    
    n_plot <- plotly::plot_ly(database,x = ~Date,y=~Hospitalized,type='scatter',mode='lines',name='Hospitalized',
                              hovertemplate = paste("Date: %{x}<br>","Hospitalized: %{y}")) 
    n_plot <- n_plot %>%plotly::add_trace(y=~ICU,type='scatter',mode='lines',name='ICU',
                                          hovertemplate = paste("Date: %{x}<br>","ICU: %{y}"))
    n_plot <- n_plot %>%plotly::add_trace(y=~Home_Isolation,type='scatter',mode='lines',name='Home Isolation',
                                          hovertemplate = paste("Date: %{x}<br>","Home Isolation: %{y}"))
    n_plot <- n_plot %>%plotly::add_trace(y=~Total_Positive,type='scatter',mode='lines',name='Total Positive',
                                          hovertemplate = paste("Date: %{x}<br>","Total Positive: %{y}")) 
    
    
    n_plot <- n_plot %>%plotly::layout(title = paste0("Daily Cases of ",regione)
                                       , xaxis = list(title = "Days")
                                       , yaxis = list (title = "Cases")
                                       # , paste0(str_split(rangeDays, "-")[[1]][3]
                                       #          , "/",  str_split(rangeDays, "-")[[1]][2])
                                       # , " in ", regione)
                                       ,legend = list(x = 0.1, y = 0.9))
    
    # Plot2<-ggplot(data=database, aes(x=Date)) +
    #   geom_line(aes(y = `Hospit. sympt. cases`, color = "Hospit. sympt. cases"), size = 1) +
    #   geom_line(aes(y = `Intens. care cases`, color = "Intens. care cases"), size = 1) +
    #   geom_line(aes(y = `Home isol. cases`, color = "Home isol. cases"), size = 1) +
    #   geom_line(aes(y = `Tot. pos. cases`, color = "Tot. pos. cases"), size = 1) +
    #   labs(
    #     x = "Dates",
    #     y = "Cases",
    #     color="Variables",
    #     title=paste0("Cases until the "
    #                  , paste0(str_split(rangeDays, "-")[[1]][3]
    #                           , "/",  str_split(rangeDays, "-")[[1]][2])
    #                  , " in ", regione)) +
    #   scale_color_manual(values = colors) +
    #   theme(legend.title = element_text(color="black", face = "bold"))+
    #   guides(color=guide_legend("Variables"))
    # 
    # GGPlotly2<-ggplotly(Plot2)
    # 
    # GGRglotly2 <- GGPlotly2 %>%
    #   layout(xaxis = list(visible=FALSE)
    #          , showlegend = TRUE)
    
  }
  
  return(print(x = n_plot))
  
}