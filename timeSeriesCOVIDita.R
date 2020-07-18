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
  
  IdxPlot <- IdxPlot %>% config(displaylogo = FALSE,
                                modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","autoScale2d"
                                                           ,"resetScale2d","zoom3d","zoom2d","toggleSpikelines"
                                                           ,"hoverCompareCartesian","toImage"))
  IdxPlot <- IdxPlot %>% layout(legend = list(x = 0.05, y = 0.9),hovermode = 'compare')
              
  
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
                   , yaxis = list (title = "Cumulative Rates"))

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
    
    
  }
  
  n_plot <- n_plot %>% layout(legend = list(x = 100, y = 0.5))
  
  return(print(x = n_plot))
  
}


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
                             , legend=list(x = 0.1, y = 0.9))%>% 
                      config(displaylogo = FALSE,
                             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","autoScale2d"
                                                        ,"resetScale2d","zoom3d","zoom2d","toggleSpikelines"
                                                        ,"toImage"))
    
    
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
                                       ,legend = list(x = 0.1, y = 0.9))%>% 
                                config(displaylogo = FALSE,
                                       modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","autoScale2d"
                                                                  ,"resetScale2d","zoom3d","zoom2d","toggleSpikelines"
                                                                  ,"toImage"))
    
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
                                        ,legend = list(x = 0.7, y = 0.9))%>% 
                                config(displaylogo = FALSE,
                                       modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","autoScale2d"
                                                                  ,"resetScale2d","zoom3d","zoom2d","toggleSpikelines"
                                                                  ,"toImage"))

    
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
                                       ,legend = list(x = 0.7, y = 0.9))%>% 
                                config(displaylogo = FALSE,
                                       modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d","autoScale2d"
                                                                  ,"resetScale2d","zoom3d","zoom2d","toggleSpikelines"
                                                                  ,"toImage"))
    
  }
  
  return(print(x = n_plot))
  
}