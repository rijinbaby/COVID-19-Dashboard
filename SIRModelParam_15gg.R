#Options
options(warn=-1)

#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, plotly, lmridge)

#Load Packages
library(tidyverse)
library(plotly)
library(lmridge)

#----------------------------------
findPopulation<-function(reg="Italy"){
  
  populationDataset <- pcmTOTData %>% 
    distinct(
      denominazione_regione
      , denominazione_provincia
      , popolazione
    )
  
  if (reg=="Italy") {
    return(sum(populationDataset$popolazione))
  } else if(reg %in% c(unique(populationDataset$denominazione_regione))){
    
    popolazione <- populationDataset %>%
      filter(denominazione_regione==reg) %>%
      group_by(denominazione_regione) %>%
      summarize_if(is.numeric, sum, na.rm = TRUE)
    
    population <- popolazione$popolazione
    
    return(population)
  } else if(reg %in% c(unique(populationDataset$denominazione_provincia))){
    
    popolazione <- populationDataset %>%
      filter(denominazione_provincia==reg) %>%
      group_by(denominazione_provincia) %>%
      summarize_if(is.numeric, sum, na.rm = TRUE)
    
    population <- popolazione$popolazione
    
    return(population)
  } 
}

#----------------------------------

createDataset<-function(r="Italy", pcmTotData=pcmTOTData
                        , deathDataset=deathDatesReg) {
  
  deathDatesCOVID <- deathDatesReg %>%
    filter(!is.na(decessi_tot))
  
  if(r=="Italy"){
    dataset <- pcmTotData %>%
      ungroup() %>%
      select(
        data
        , totale_positivi.y
        , dimessi_guariti.y
        , deceduti.y
        , totale_casi
        , nuovi_positivi.y
      ) %>%
      distinct(
        data
        , totale_positivi.y
        , dimessi_guariti.y
        , deceduti.y
        , totale_casi
        , nuovi_positivi.y
      )
    colnames(dataset) <- c("data",  "totale_positivi", "dimessi_guariti", "deceduti", "totale_casi", "nuovi_positivi")
    
    population<-findPopulation(reg=r)
    return(as_data_frame(cbind(cases=dataset$totale_positivi,
                               recovered=dataset$dimessi_guariti,
                               deaths=dataset$deceduti,
                               susceptible=population-dataset$totale_casi,
                               new_cases=dataset$nuovi_positivi)))
    
  } else if(r %in% unique(pcmTotData$denominazione_regione)){
    dataset <- pcmTotData %>%
      filter(denominazione_regione==r)%>%
      ungroup() %>%
      select(
        data
        , totale_positivi.x
        , dimessi_guariti.x
        , deceduti.x
        , totale_casi.y
        , nuovi_positivi.x
      ) %>%
      distinct(
        data
        , totale_positivi.x
        , dimessi_guariti.x
        , deceduti.x
        , totale_casi.y
        , nuovi_positivi.x
      )
    colnames(dataset) <- c("data",  "totale_positivi", "dimessi_guariti", "deceduti", "totale_casi", "nuovi_positivi")
    
    population<-findPopulation(reg=r)
    return(as_data_frame(cbind(cases=dataset$totale_positivi,
                               recovered=dataset$dimessi_guariti,
                               deaths=dataset$deceduti,
                               susceptible=population-dataset$totale_casi,
                               new_cases=dataset$nuovi_positivi)))
  } else if (r %in% unique(mortiProvinces$provincia)) {
    
    d_provincia<-mortiProvinces%>%
      filter(provincia==r) %>%
      select(data
             ,nuovi_casi
             ,casi_att_positivi
             ,decessi_tot)%>%
      arrange(data)
    
    regione<-pcmTOTData%>%
      filter(denominazione_provincia==r)%>%
      distinct(denominazione_regione)
    
    d_regione<-pcmTOTData%>%
      filter(denominazione_regione==regione$denominazione_regione) %>%
      arrange(data) %>%
      group_by(data) %>%
      select(data,totale_positivi.x,dimessi_guariti.x,deceduti.x,totale_casi.y,nuovi_positivi.x) %>%
      summarise_all(.funs=mean)
    
    tasso_guariti<-(d_regione$dimessi_guariti.x/d_regione$totale_casi.y)[1:nrow(d_provincia)]
    
    tasso_guariti[is.nan(tasso_guariti)|is.infinite(tasso_guariti)]<-0
    
    guariti<-tasso_guariti*d_provincia$casi_att_positivi

    set.seed(42)
    for (i in 1:length(guariti)) {
      guariti[i]<-trunc(guariti[i])+rbinom(1,1,prob=(guariti[i]-trunc(guariti[i])))
    }
    d_provincia$dimessi_guariti<-guariti
    
    d_provincia$infetti<-d_provincia$casi_att_positivi-d_provincia$decessi_tot-d_provincia$dimessi_guariti
    
    population<-findPopulation(reg=r)
    
    return(as_data_frame(cbind(cases=d_provincia$infetti,
                               recovered=d_provincia$dimessi_guariti,
                               deaths=d_provincia$decessi_tot,
                               susceptible=population-d_provincia$casi_att_positivi,
                               new_cases=d_provincia$nuovi_casi)))
    }
}

#----------------------------------

param_comp<-function(region="Italy"){
  dataset<-createDataset(region)
  population<-findPopulation(region)
  n<-nrow(dataset)
  recovery<-diff(dataset$recovered, lag=1)/dataset$cases[1:(n-1)]
  death_rate<-diff(dataset$deaths, lag=1)/dataset$cases[1:(n-1)]
  transmission<-(population/dataset$susceptible[1:(n-1)])*
    (diff(dataset$cases,lag=1)+
       diff(dataset$recovered,lag=1)+
       diff(dataset$deaths,lag=1))/dataset$cases[1:(n-1)]
  #Added the is.nan() | is.infinite() check
  recovery[recovery<0|is.nan(recovery)|is.infinite(recovery)]<-0
  transmission[transmission<0|is.nan(transmission)|is.infinite(transmission)]<-0
  death_rate[death_rate<0|is.nan(death_rate)|is.infinite(death_rate)]<-0
  
  return(as_data_frame(cbind(transmission=transmission
                             ,recovery=recovery
                             ,death_rate=death_rate)))
}

#----------------------------------

findLastJ<-function(set,J,all=FALSE) {
  x_preds<-data.frame()
  N<-length(set)+1
  for (i in (J+1):N) {
    x<-c()
    for (j in 1:J) {
      a<-set[i-j]
      x<-c(x,a)
    }
    x_preds<-rbind(x_preds,t(x))
  }
  if (isFALSE(all)) {
    return(as.data.frame(x_preds[N-J,]))
  } else {
    return(as.data.frame(cbind(y=set[(J+1):(N-1)], x_preds[1:(N-J-1),])))
  }
}

#----------------------------------

findOptimalLags<-function(set=paramComp$transmission
                          , min=4, max=10) {
  aic_values<-data.frame()
  for (i in min:max) {
    y_and_preds<-findLastJ(set,i,all=TRUE)
    #How did you choose this K??
    aic_values<-c(aic_values,infocr(lmridge(y~.,data=y_and_preds, K = 0.03))[1])
  }
  optimal_J<-which.min(aic_values)+min-1
  # y_and_preds<-findLastJ(set,optimal_J,all=TRUE)
  # return(lmridge(y~.,data=y_and_preds, K = 0.03))
  return(optimal_J)
}

#----------------------------------
###################################
#----------------------------------

modelTrainer <- function(parComp=paramComp, J=varLagP){
  
  model_transmission<-lmridge(y~., data=findLastJ(parComp$transmission,J,all=TRUE), K =0.03)
  model_recovery<-lmridge(y~.,data=findLastJ(parComp$recovery,J,all=TRUE), K =1e-9)
  model_death<-lmridge(y~.,data=findLastJ(parComp$death_rate,J,all=TRUE), K =1e-9)
  
  return(list(modelTransm=model_transmission
              , modelRecov=model_recovery
              , modelDeath=model_death))
}

#----------------------------------

TD_SIRD_model<-function(model=model_Trainer, reg="Italy", j=varLagP, days=15){
  
  model_transmission <- model$modelTransm
  model_recovery <- model$modelRecov
  model_death <- model$modelDeath
  
  #regional parameters and data
  population<-findPopulation(reg)
  region_p<-param_comp(reg)
  transmission_predicted<-region_p$transmission
  recovery_predicted<-region_p$recovery
  death_predicted<-region_p$death_rate
  dataset_predicted<-createDataset(reg)[,1:4]
  n<-nrow(dataset_predicted)
  
  for (i in n:(n+days)) {
    new_beta<-unname(predict(model_transmission, findLastJ(transmission_predicted,j)))
    if (new_beta<0) {
      new_beta<-0
    }
    new_g_rec<-unname(predict(model_recovery,findLastJ(recovery_predicted,j)))
    new_g_death<-unname(predict(model_death,findLastJ(death_predicted,j)))
    new_I<-round((1+new_beta*(dataset_predicted$susceptible[i]/population)-new_g_rec-new_g_death)
                 *dataset_predicted$cases[i],0)
    new_R<-round(dataset_predicted$recovered[i]+new_g_rec*dataset_predicted$cases[i],0)
    new_D<-round(dataset_predicted$deaths[i]+new_g_death*dataset_predicted$cases[i],0)
    transmission_predicted<-c(transmission_predicted,new_beta)
    recovery_predicted<-c(recovery_predicted,new_g_rec)
    death_predicted<-c(death_predicted,new_g_death)
    dataset_predicted<-rbind.data.frame(dataset_predicted,cbind(cases=new_I
                                                                , recovered=new_R
                                                                , deaths=new_D
                                                                , susceptible=population-new_I-new_R-new_D))
  }
  
  dataset_predicted$new_cases<-c(0,diff(dataset_predicted$cases,lag=1)+
                                   diff(dataset_predicted$recovered,lag=1)+
                                   diff(dataset_predicted$deaths,lag=1))
  dataset_predicted$new_cases[dataset_predicted$new_cases<0]=0
  return(cbind(dataset_predicted
               ,"Beta"=c(transmission_predicted,0)
               ,"Recovery Rate"=c(recovery_predicted,0)
               ,"Death Rate"=c(death_predicted,0)
               ,R0=c(transmission_predicted/(recovery_predicted+death_predicted),0)))
}


#----------------------------------

#Plots the SIRD curves
SIRD_plot<-function(region=varSIRp, J=varLagP, models=model_Trainer, n_days=15){

  dataset<-createDataset(region)
  population<-findPopulation(region)
  n<-nrow(dataset)
  results<-TD_SIRD_model(model=models, reg=region, j=J, days=n_days)
  
  date<-seq(as.Date("24 Feb 2020","%d %b %Y"),length.out = n+n_days+1, by=1)
  height <-seq(0 ,max(population-results$susceptible))
  
  SIRDgraph<-plot_ly(type="scatter",mode="markers")%>%
    #-----------
    add_trace(x =rep(date[10],times=length(height)) , 
            y =height,
            mode = "lines",line=list(color="grey", dash="dot"),
            text="School closure", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[14],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", dash="dot"),
              text="Northern regions lockdown", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[17],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", dash="dot"),
              text="National lockdown", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[27],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", dash="dot"),
              text="Only key activities", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[71],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", dash="dot"),
              text="Phase 2: reopening", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[85],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", dash="dot"),
              text="Phase 2: Lifted lockdown within regions", showlegend=F, hoverinfo='text')%>%
    
    #-----------
    add_trace(x=-100000, y=-10000000,mode = "markers", name="<b>Model Predictions</b>",
              marker=list(color="white")) %>%
    add_trace(x=date,  y=population-results$susceptible,mode = "lines", name="Total Cases",
              line=list(color="green",width = 4))%>%
    add_trace(x=date,  y=results$new_cases,mode = "lines", name="New Cases",
              line=list(color="yellow",width = 4))%>%
    add_trace(x=date, y=results$cases,mode = "lines",type="scatter",name="Infected",
              line = list(color = "red", width = 4))%>%
    add_trace(x=date,  y=results$recovered,mode = "lines", name="Recovered", 
              line=list(color="blue",width = 4))%>%
    add_trace(x=date,  y=results$deaths, mode = "lines", name="Deaths",
              line=list(color="grey",width = 4))%>%
    ##real data
    add_trace(x=-100000,  y=-100000000, name="   ",
              mode="markers", marker=list(color="white", line=list(color="white",width=1)))%>%
    add_trace(x=-100000,  y=-1000000, name="<b>Real Data</b>",
              mode="markers", marker=list(color="white", line=list(color="white",width=1)))%>%
    add_trace(x=date[1:n],  y=population-dataset$susceptible, name="Total Cases",
              mode="markers", marker=list(color="green", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=dataset$new_cases, name="New Cases",
              mode="markers", marker=list(color="yellow", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=dataset$cases,name="Infected",
              mode="markers", marker=list(color="red", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=dataset$recovered, name="Recovered",
              mode="markers", marker=list(color="blue", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=dataset$deaths, name="Deaths",
              mode="markers", marker=list(color="grey", line=list(color="black",width=1)))%>%
    layout(title = sprintf('<b> SIRD Model - %s </b>',region),
           xaxis = list(title = "Days",type='date',tickformat = "%d %B", tickangle=45,range=c(date[1],date[n+n_days+1])),
           yaxis = list(title = "Individuals",range=c(0,max(cumsum(results$new_cases)))),
           legend=list(x=100, y=0.5))%>%
    plotly::config(modeBarButtonsToRemove = c("lasso2d", "select2d",  "toggleSpikelines",
                                      "hoverClosestCartesian","hoverCompareCartesian","autoScale2d"))
  
  return(SIRDgraph)
}

#----------------------------------

SIRDtimeSeries<-function(region=varSIRp, J=varLagP, models=model_Trainer, d=15) {
  SIR_predictions<-TD_SIRD_model(model=models, reg=region, j=J, days = d)
  
  model_transmission <- models$modelTransm
  model_recovery <- models$modelRecov
  model_death<-models$modelDeath
  
  #Compute estimated beta, gamma revovery and gamma death to compute estimated R0
  parRegion<-param_comp(region)
  len<-nrow(parRegion)
  date<-seq(as.Date("24 Feb 2020","%d %b %Y"),length.out = len+1+d, by=1) 
  
  #Compute predictions using the model for the first part and the td_sird_model predictions for the second part
  tran_p<-unname(predict.lmridge(model_transmission,
                                 findLastJ(parRegion$transmission,J,all=TRUE)))
  recov_p<-unname(predict.lmridge(model_recovery,
                                  findLastJ(parRegion$recovery,J,all=TRUE)))
  death_p<-unname(predict.lmridge(model_death,
                                  findLastJ(parRegion$death_rate,J,all=TRUE)))
  
  #append all the models predictions together
  R0_p<-c(tran_p/(recov_p+death_p),SIR_predictions[["R0"]][(len+1):(len+1+d)])
  tran_p<-c(tran_p,SIR_predictions[["Beta"]][(len+1):(len+1+d)])
  recov_p<-c(recov_p,SIR_predictions[["Recovery Rate"]][(len+1):(len+1+d)])
  death_p<-c(death_p,SIR_predictions[["Death Rate"]][(len+1):(len+1+d)])
  #check for non-appropriate values
  recov_p[recov_p<0|is.nan(recov_p)|is.infinite(recov_p)]<-0
  tran_p[tran_p<0|is.nan(tran_p)|is.infinite(tran_p)]<-0
  death_p[death_p<0|is.nan(death_p)|is.infinite(death_p)]<-0
  R0_p[R0_p<0|is.nan(R0_p)|is.infinite(R0_p)]<-0
  #r0 real values plus check
  r0_real_values<-SIR_predictions[["R0"]][(J+1):len]
  r0_real_values[r0_real_values<0|is.nan(r0_real_values)|is.infinite(r0_real_values)]<-0
  
  #common settings for the plots
  type_title<-list(xref = "paper",yref = "paper",
                   yanchor = "bottom",xanchor = "center",align = "center",x = 0.5,y = 1,
                   showarrow = F)
  #Plots
  beta_plot<-plot_ly(type="scatter",mode="markers")%>%
    add_trace(x=date[(J+1):len], y=SIR_predictions[["Beta"]][(J+1):len],mode = "markers", 
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(J+1):(len+1+d)], y=tran_p,mode = "lines", 
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(showlegend=FALSE,xaxis = list(type='date',tickformat = "%d-%m",tickangle=45),
           annotations= c(text = "<b>Transmission rate</b>",type_title),
           yaxis=list(range=c(0,max(SIR_predictions[["Beta"]][(J+1):len]))))
  
  recovery_plot<-plot_ly(type="scatter",mode="markers")%>%
    add_trace(x=date[(J+1):len], y=SIR_predictions[["Recovery Rate"]][(J+1):len],mode = "markers", 
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(J+1):(len+1+d)], y=recov_p,mode = "lines", 
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(showlegend=FALSE,xaxis = list(type='date',tickformat = "%d-%m",tickangle=45),
           annotations= c(text = "<b>Recovery rate</b>",type_title))
  
  mortality_plot<-plot_ly(type="scatter",mode="markers")%>%
    add_trace(x=date[(J+1):len], y=SIR_predictions[["Death Rate"]][(J+1):len],mode = "markers", 
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(J+1):(len+1+d)], y=death_p,mode = "lines", 
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(showlegend=FALSE,xaxis = list(type='date',tickformat = "%d-%m",tickangle=45),
           annotations= c(text = "<b>Mortality rate</b>",type_title))
  
  r0_plot<-plot_ly(type="scatter",mode="markers")%>%
    #plot real values
    add_trace(x=date[(J+1):len], y=r0_real_values,mode = "markers", 
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    #plot predictions
    add_trace(x=date[(J+1):(len+1+d)], y=R0_p,mode = "lines", 
              line=list(color="black",width=1),name="Model Predictions")%>%
    add_segments(x =date[1] , xend =date[length(date)] , y =1 , yend = 1,line=list(color="grey",dash="dash"))%>%
    layout(showlegend=FALSE,xaxis = list(type='date',tickformat = "%d-%m",tickangle=45),
           yaxis=list(range=c(0,max(r0_real_values))),
           annotations= c(text = "<b>R0</b>",type_title))
  
  return(subplot(beta_plot,r0_plot,recovery_plot,mortality_plot,nrows=2, margin=0.07)%>%
           config(modeBarButtonsToRemove = c("lasso2d", "select2d",  "toggleSpikelines",
                                             "hoverClosestCartesian","hoverCompareCartesian",
                                             "autoScale2d")))
}


