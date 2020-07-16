#Options
options(warn=-1)

#save.image("SIRDmodelDATA.RData")
#load("SIRDmodelDATA.RData")

#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, plotly, lmridge, glmnet, foreach, doParallel, iterators, parallel)

#Usefull Functions
`%notin%` <- Negate(`%in%`)
Weights <- function(x){
  return(x/sum(x))
}

{
  #Load Packages
  library(tidyverse)
  library(plotly)
  library(lmridge)
  library(foreach)
  library(doParallel)
  
  #Regional Actual Index Correlation----
  library(glmnet)
  
  nOfDays <- length(unique(pcmTOTData$data))
  Regions <- unique(pcmTOTData$denominazione_regione)
  ActIDXReg <- as_tibble(matrix(nrow=(nOfDays-1), ncol=length(Regions)))
  colnames(ActIDXReg) <- Regions
  W <- Weights(c(2:nOfDays))
  for(reg in Regions){
    ActIDXReg[[reg]] <- diff(pcmTOTData$actualPrevIdxReg[which(pcmTOTData$denominazione_regione==reg)][1:nOfDays])
  }
  
  #corrIDXreg <- cov.wt(ActIDXReg, wt = W, method = c("unbiased"), cor=TRUE, center=FALSE)$cor
  corrIDXreg <- cor(ActIDXReg, method = c("pearson"))
  
  
  # rRow <- which(Regions=="Piemonte")
  # rRegCorr <- names(corrIDXreg[rRow,][corrIDXreg[rRow,]
  #                                     >=quantile(corrIDXreg[rRow,][corrIDXreg[rRow,]!=1])[3]])
  
  #Provinces Actual Index Correlation----
  deathDatesCOVID <- mortiProvinces %>%
    filter(!is.na(decessi_tot))
  Provinces <- unique(deathDatesCOVID$provincia)
  popByProv <- pcmTOTData %>%
    ungroup() %>%
  distinct(denominazione_provincia
           ,  popolazione)
  deathDatesCOVID <- left_join(deathDatesCOVID, popByProv, by = c("provincia" = "denominazione_provincia"))
  deathDatesCOVID$actualPrevIdxProv <- round((deathDatesCOVID$casi_att_positivi/deathDatesCOVID$popolazione)*100000)
  
  countDay <- c()
  for(prov in Provinces){
    countDay <- c(countDay, length(deathDatesCOVID$data[deathDatesCOVID$provincia==prov]))
  }
  minDay<-min(countDay)
  
  ActIDXProv <- as_tibble(matrix(nrow=(minDay-1), ncol=length(Provinces)))
  colnames(ActIDXProv) <- Provinces
  W <- Weights(c(2:minDay))
  for(prov in Provinces){
    ActIDXProv[[prov]] <- diff(deathDatesCOVID$actualPrevIdxProv[which(deathDatesCOVID$provincia==prov)][1:minDay])
  }
  
  #corrIDXprov <- cov.wt(ActIDXProv, wt = W, method = c("unbiased"), cor=TRUE, center=FALSE)$cor
  corrIDXprov <- cor(ActIDXProv)
  
  #Provinces by Regions
  ProvByReg <- pcmTOTData %>%
    distinct(denominazione_regione
             , denominazione_regione)
  colnames(ProvByReg) <- c("Region", "Province")
}  
  
{  
  #findPopulation----
  myFindPopulation<-function(reg="Italy"){
    
    populationDataset <- pcmTOTData %>% 
      distinct(
        denominazione_regione
        , denominazione_provincia
        , popolazione
      )
    
    if (reg[1]=="Italy") {
      return(sum(populationDataset$popolazione))
    } else if(sum(reg %in% c(unique(populationDataset$denominazione_regione)))==length(reg)){
      
      popolazione <- populationDataset %>%
        filter(denominazione_regione%in%reg) %>%
        group_by(denominazione_regione) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)
      
      population <- sum(popolazione$popolazione)
      
      return(population)
    } else if(reg %in% c(unique(populationDataset$denominazione_provincia))){
      
      popolazione <- populationDataset %>%
        filter(denominazione_provincia%in%reg) %>%
        group_by(denominazione_provincia) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)
      
      population <- sum(popolazione$popolazione)
      
      return(population)
    } 
  }
  
  #createDataset----
  myCreateDataset<-function(r="Emilia-Romagna"
                            , pcmTotData=pcmTOTData
                            , deathDataset=mortiProvinces
                            , corrIDXregMatr=corrIDXreg
                            , corrIDXprovMatr=corrIDXprov
                            , regions=Regions
                            , provinces=Provinces
                            , provCorr=TRUE
                            , cut=FALSE) {
    
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
      
      population<-myFindPopulation(reg="Italy")
      return(as_tibble(bind_cols(cases=dataset$totale_positivi,
                                     recovered=dataset$dimessi_guariti,
                                     deaths=dataset$deceduti,
                                     susceptible=population-dataset$totale_casi,
                                     new_cases=dataset$nuovi_positivi)))
      
    } else if(r %in% unique(pcmTotData$denominazione_regione)){

      rRow <- which(regions==r)
      
      rRegCorr <- names(corrIDXregMatr[rRow,][corrIDXregMatr[rRow,]
                                              >=quantile(corrIDXregMatr[rRow,][corrIDXregMatr[rRow,]!=1])[3]])
      
      dataset <- pcmTotData %>%
        filter(denominazione_regione%in%rRegCorr)%>%
        ungroup() %>%
        select(
          denominazione_regione
          , data
          , totale_positivi.x 
          , dimessi_guariti.x
          , deceduti.x
          , totale_casi.y 
          , nuovi_positivi.x 
        ) %>%
        distinct(
          denominazione_regione
          , data
          , totale_positivi.x 
          , dimessi_guariti.x
          , deceduti.x
          , totale_casi.y 
          , nuovi_positivi.x 
        ) %>%
        select(
          data
          , totale_positivi.x 
          , dimessi_guariti.x
          , deceduti.x
          , totale_casi.y 
          , nuovi_positivi.x 
        ) %>%
        group_by(
          data
        ) %>%
        summarize_if(is.numeric, sum)
      
      colnames(dataset) <- c("data",  "totale_positivi", "dimessi_guariti", "deceduti", "totale_casi", "nuovi_positivi")
      
      population<-myFindPopulation(reg=rRegCorr)
      dataset <- as_tibble(bind_cols(cases=dataset$totale_positivi,
                                     recovered=dataset$dimessi_guariti,
                                     deaths=dataset$deceduti,
                                     susceptible=population-dataset$totale_casi,
                                     new_cases=dataset$nuovi_positivi))
      if (isFALSE(cut)) {
        return(dataset)
      } else {
        return(dataset[1:cut,])
      }
      
    } else if((r %in% unique(mortiProvinces$provincia))|provCorr) {
      
      if(provCorr){
        
        pRow <- which(provinces==r)
        rProvCorr <- names(corrIDXprov[pRow,][corrIDXprov[pRow,]>=quantile(corrIDXprov[pRow,][corrIDXprov[pRow,]!=1])[3]])
        
        d_provincia <- mortiProvinces %>%
          filter(provincia%in%rProvCorr)%>%
          ungroup() %>%
          select(
            provincia
            , data
            , nuovi_casi
            , casi_att_positivi
            , decessi_tot
          ) %>%
          distinct(
            provincia
            , data
            , nuovi_casi
            , casi_att_positivi
            , decessi_tot
          ) %>%
          select(
            data
            , nuovi_casi
            , casi_att_positivi
            , decessi_tot 
          ) %>%
          group_by(
            data
          ) %>%
          summarize_if(is.numeric, sum)
        
        provPop <- rProvCorr
        
      }else if(!provCorr){
        
        d_provincia<-mortiProvinces%>%
          filter(provincia==r) %>%
          select(data
                 ,nuovi_casi
                 ,casi_att_positivi
                 ,decessi_tot)%>%
          arrange(data)
        
        provPop <- r
        
      }
      
      regione<-pcmTOTData%>%
        filter(denominazione_provincia==r)%>%
        distinct(denominazione_regione)
      
      d_regione<-pcmTOTData%>%
        filter(denominazione_regione==regione$denominazione_regione) %>%
        arrange(data) %>%
        group_by(data) %>%
        select(data
               , totale_positivi.x
               , dimessi_guariti.x
               , deceduti.x
               , totale_casi.y
               , nuovi_positivi.x) %>%
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
      
      population<-myFindPopulation(reg=provPop)
      
      dataset <- as_tibble(bind_cols(cases=d_provincia$infetti,
                                     recovered=d_provincia$dimessi_guariti,
                                     deaths=d_provincia$decessi_tot,
                                     susceptible=population-d_provincia$casi_att_positivi,
                                     new_cases=d_provincia$nuovi_casi))
      if (isFALSE(cut)) {
        return(dataset)
      } else {
        return(dataset[1:cut,])
      }
    }
  }
  
  #param_comp----
  myParam_comp<-function(region="Piacenza", model=TRUE, cut_point=FALSE, corr="Prov"){
    
    if (model){ #Used only when we create the model
    
      if(corr=="Reg"){
        
        if(region %in% c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")){
          region <- "Piemonte"
        } else if(region %in% c("Aosta")){
          region <- "Valle d'Aosta"
        } else if(region %in% c("Lodi", "Cremona")){
          region <- "Lombardia"
        } else if(region %in% c("Bolzano","Trento")){
          region <- "Trentino Alto Adige"
        } else if(region %in% c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")){
          region <- "Emilia-Romagna"
        } else if(region %in% c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")){
          region <- "Toscana"
        } else if(region %in% c("Perugia", "Terni")){
          region <- "Umbria"
        } else if(region %in% c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")){
          region <- "Marche"
        } else if(region %in% c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")){
          region <- "Lazio"
        } else if(region %in% c("Chieti", "L'Aquila", "Pescara", "Teramo" )){
          region <- "Abruzzo"
        } else if(region %in% c("Campobasso", "Isernia")){
          region <- "Molise"
        } else if(region %in% c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")){
          region <- "Puglia"
        } else if(region %in% c("Matera", "Potenza")){
          region <- "Basilicata"
        } else if(region %in% c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")){
          region <- "Calabria"
        } else if(region %in% c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")){
          region <- "Sicilia"
        } else if(region %in% c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")){
          region <- "Sardegna"
        } else if(region %in% c("Imperia")){
          region <- "Liguria"
        } 
        
      } else if(corr=="Prov"){
        
        region <- region
        
      }
      
      dataset<-myCreateDataset(r=region, cut=cut_point, provCorr=T)
      
    } else if(!model){
      
      dataset<-myCreateDataset(r=region, cut=cut_point, provCorr=F)
      
    }
    
    population<-dataset$susceptible[1]
    n<-nrow(dataset)
    recovery<-diff(dataset$recovered, lag=1)/dataset$cases[1:(n-1)]
    death_rate<-diff(dataset$deaths, lag=1)/dataset$cases[1:(n-1)]
    transmission<-(population/dataset$susceptible[1:(n-1)])*
      (diff(dataset$cases,lag=1)+
         diff(dataset$recovered,lag=1)+
         diff(dataset$deaths,lag=1))/dataset$cases[1:(n-1)]
    R0 <- transmission/(recovery+death_rate)
    #Added the is.nan() | is.infinite() check
    recovery[recovery<0|is.nan(recovery)|is.infinite(recovery)]<-0
    transmission[transmission<0|is.nan(transmission)|is.infinite(transmission)]<-0
    death_rate[death_rate<0|is.nan(death_rate)|is.infinite(death_rate)]<-0
    R0[R0<0|is.nan(R0)|is.infinite(R0)]<-0
    return(as_tibble(bind_cols(transmission=transmission
                               ,recovery=recovery
                               ,death_rate=death_rate
                               ,R0=R0)))
  }
  
  
  
  #findLastJ----
  myFindLastJ<-function(set, J, all=FALSE) {
    x_preds<-tibble()
    N<-length(set)+1
    
    for (i in (J+1):N) {
      x<-c()
      for (j in 1:J) {
        a<-set[i-j]
        x<-c(x, a)
      }

      x_preds<-x_preds %>%
        bind_rows(set_names(x, paste0("V", 1:J)))  
    }
    
    if (isFALSE(all)) {
      return(as_tibble(x_preds[N-J,]))
    } else {
      return(as_tibble(bind_cols(y=set[(J+1):(N-1)], x_preds[1:(N-J-1),])))
    }
  }
  
  #findOptimalLags----
  myFindOptimalLags<-function(set=paramComp$transmission
                            , min=4, max=20) {
    aic_values<-tibble()
    for (i in min:max) {
      y_and_preds<-myFindLastJ(set, i ,all=TRUE)
      
      Y <- y_and_preds$y
      X <- y_and_preds %>% select(-y) %>% data.matrix()
      
      OPT_lambda <- c()
      for(i in 1:10){
        
        CV_fit <- cv.glmnet(X, Y, alpha = 0, lambda = 10^seq(5, -5, -.1))
        
        OPT_lambda <- c(OPT_lambda, CV_fit$lambda.min)
        
      }
      OPT_K <- mean(OPT_lambda)
      
      aic_values<-c(aic_values, infocr(lmridge(y~., data=y_and_preds, K = OPT_K))[1])
    }
    optimal_J<-which.min(aic_values)+min-1
    
    return(optimal_J)
  }

  #----------------------------------
  mySetLambda <- function(dataset=paramComp, J=varLagpCV){
    
    lambdas <- 10^seq(5, -5, -.1)
    
    dataTrans <- myFindLastJ(dataset$transmission, J, all=TRUE)
    dataRec <- myFindLastJ(dataset$recovery, J, all=TRUE)
    dataDea <- myFindLastJ(dataset$death_rate, J, all=TRUE)
    
    Y_Trans <- dataTrans$y
    X_Trans <- dataTrans %>% select(-y) %>% data.matrix()
    Y_Rec <- dataRec$y
    X_Rec <- dataRec %>% select(-y) %>% data.matrix()
    Y_Dea <- dataDea$y
    X_Dea <- dataDea %>% select(-y) %>% data.matrix()
    
    OPT_Trans <- c()
    OPT_Rec <- c()
    OPT_Dea <- c()
    for(i in 1:30){
      
      CV_fitTrans <- cv.glmnet(X_Trans, Y_Trans, alpha = 0, lambda = lambdas)
      OPT_Trans <- c(OPT_Trans, CV_fitTrans$lambda.min)
      
      CV_fitRec <- cv.glmnet(X_Rec, Y_Rec, alpha = 0, lambda = lambdas)
      OPT_Rec <- c(OPT_Rec, CV_fitRec$lambda.min)
      
      CV_fitDea <- cv.glmnet(X_Dea, Y_Dea, alpha = 0, lambda = lambdas)
      OPT_Dea <- c(OPT_Dea, CV_fitDea$lambda.min)
      
    }
    
    LambdaTrans <- mean(OPT_Trans)
    LambdaRec <- mean(OPT_Rec)
    LambdaDea <- mean(OPT_Dea)
    
    return(list(LambdaTrans, LambdaRec, LambdaDea))
    
  }
  
  #setLambda----
  mySetLambda_optim <- function(dataset=paramComp, J=varLagpCV){
    
    lambdas <- 10^seq(5, -5, -.1)
    
    #     dataTrans <- myFindLastJ(dataset$transmission, J, all=TRUE)
    #     dataRec <- myFindLastJ(dataset$recovery, J, all=TRUE)
    #     dataDea <- myFindLastJ(dataset$death_rate, J, all=TRUE)
    
    #     Y_Trans <- dataTrans$y
    #     X_Trans <- dataTrans %>% select(-y) %>% data.matrix()
    #     Y_Rec <- dataRec$y
    #     X_Rec <- dataRec %>% select(-y) %>% data.matrix()
    #    Y_Dea <- dataDea$y
    #    X_Dea <- dataDea %>% select(-y) %>% data.matrix()
    
    #     OPT_Trans <- c()
    #    OPT_Rec <- c()
    #     OPT_Dea <- c()
    #     for(i in 1:30){
      
    #       CV_fitTrans <- cv.glmnet(X_Trans, Y_Trans, alpha = 0, lambda = lambdas)
    #       OPT_Trans <- c(OPT_Trans, CV_fitTrans$lambda.min)
      
    #       CV_fitRec <- cv.glmnet(X_Rec, Y_Rec, alpha = 0, lambda = lambdas)
    #      OPT_Rec <- c(OPT_Rec, CV_fitRec$lambda.min)
      
    #      CV_fitDea <- cv.glmnet(X_Dea, Y_Dea, alpha = 0, lambda = lambdas)
    #       OPT_Dea <- c(OPT_Dea, CV_fitDea$lambda.min)
      
    #    }
    
    #     LambdaTrans <- mean(OPT_Trans)
    #    LambdaRec <- mean(OPT_Rec)
    #    LambdaDea <- mean(OPT_Dea)
 
    LambdaTrans <- foreach(i=c("transmission","recovery","death_rate"), .combine = 'cbind',
                          .packages = c("tidyverse", "glmnet")) %dopar% {
                             set <- dataset[[i]]
                             x_preds<-tibble()
                             N<-length(set)+1
                             for (i in (J+1):N) {
                               x<-c()
                               for (j in 1:J) {
                                 a<-set[i-j]
                                 x<-c(x, a)
                               }
                               
                               x_preds<-x_preds %>%
                                 bind_rows(set_names(x, paste0("V", 1:J)))  
                             }
                             dataTrans <- as_tibble(bind_cols(y=set[(J+1):(N-1)], x_preds[1:(N-J-1),]))
                             Y_Trans <- dataTrans$y
                             X_Trans <- dataTrans %>% select(-y) %>% data.matrix()
                             OPT <- c()
                             for(i in 1:30) {
                               OPT <- c(OPT,cv.glmnet(x=X_Trans,y=Y_Trans, alpha=0, lambda = lambdas)$lambda.min)
                             }
                             mean(OPT)
                           }  
      
      
    return(LambdaTrans)
    
  }
  
  #modelTrainer----
  myModelTrainer <- function(parComp=paramComp, J=varLagP, setK=bestK){
    
    model_transmission<-lmridge(y~., data=myFindLastJ(parComp$transmission, J, all=TRUE), K=setK[[1]])
    model_recovery<-lmridge(y~., data=myFindLastJ(parComp$recovery, J, all=TRUE), K=setK[[2]])
    model_death<-lmridge(y~., data=myFindLastJ(parComp$death_rate, J, all=TRUE), K=setK[[3]])
    
    return(list(modelTransm=model_transmission
                , modelRecov=model_recovery
                , modelDeath=model_death))
  }
  
}

#MY RELEASES-------------------------------

myTD_SIRD_model<-function(prov="Torino", optJ=7, days=15, cut_point=FALSE){
  
  param_Comp <- myParam_comp(region=prov, model=TRUE, cut_point=cut_point)
  
  best_K <- mySetLambda(param_Comp, optJ)
  
  models <- myModelTrainer(parComp=param_Comp, J=optJ, setK=best_K)
  model_Transmission <- models$modelTransm
  model_Recovery <- models$modelRecov
  model_Death <- models$modelDeath
  
  #Province parameters and data
  population<-myFindPopulation(reg=prov)
  dataset_Predicted<-myCreateDataset(r=prov, cut=cut_point, provCorr=F)[,1:4]
  
  region_P<-myParam_comp(region=prov, model=FALSE, cut_point=cut_point)
  trans_vector<-region_P$transmission
  recov_vector<-region_P$recovery
  death_vector<-region_P$death_rate
  
  trans_Preds<-c(rep(NA,times=optJ),unname(predict.lmridge(model_Transmission,
                                                           myFindLastJ(trans_vector,optJ,all=TRUE))))
  recov_Preds<-c(rep(NA,times=optJ),unname(predict.lmridge(model_Recovery,
                                                           myFindLastJ(recov_vector,optJ,all=TRUE))))
  death_Preds<-c(rep(NA,times=optJ),unname(predict.lmridge(model_Death,
                                                           myFindLastJ(death_vector,optJ,all=TRUE))))
  
  trans_Preds[trans_Preds<0|is.nan(trans_Preds)|is.infinite(trans_Preds)] <-0
  recov_Preds[recov_Preds<0|is.nan(recov_Preds)|is.infinite(recov_Preds)] <-0
  death_Preds[death_Preds<0|is.nan(death_Preds)|is.infinite(death_Preds)] <-0
  
  enne<-nrow(dataset_Predicted)
  
  for (n in enne:(enne+days-1)) {
    #parameters at n
    new_beta<-unname(stats::predict(model_Transmission, myFindLastJ(trans_vector, optJ)))
    if (new_beta<0) {
      new_beta<-0
    }
    new_g_rec<-unname(stats::predict(model_Recovery, myFindLastJ(recov_vector, optJ)))
    new_g_death<-unname(stats::predict(model_Death, myFindLastJ(death_vector, optJ)))
    #variables at n+1
    new_I<-round((1+new_beta*(dataset_Predicted$susceptible[n]/population)-new_g_rec-new_g_death)
                 *dataset_Predicted$cases[n],0)
    new_R<-round(dataset_Predicted$recovered[n]+new_g_rec*dataset_Predicted$cases[n],0)
    new_D<-round(dataset_Predicted$deaths[n]+new_g_death*dataset_Predicted$cases[n],0)
    
    #update
    trans_vector<-c(trans_vector, new_beta)
    recov_vector<-c(recov_vector, new_g_rec)
    death_vector<-c(death_vector, new_g_death)
    
    trans_Preds<-c(trans_Preds, new_beta)
    recov_Preds<-c(recov_Preds, new_g_rec)
    death_Preds<-c(death_Preds, new_g_death)
    
    dataset_Predicted<-bind_rows(dataset_Predicted, bind_cols(cases=new_I
                                                              , recovered=new_R
                                                              , deaths=new_D
                                                              , susceptible=population-new_I-new_R-new_D) )
  }
  
  dataset_Predicted$new_cases<-c(0,-diff(dataset_Predicted$susceptible,lag=1))
  
  dataset_Predicted$new_cases[dataset_Predicted$new_cases<0]<-0
  
  r0_vector <- c(trans_vector/(recov_vector+death_vector),0)
  r0_Preds <- c(trans_Preds/(recov_Preds+death_Preds),0)
  
  r0_vector[r0_vector<0|is.nan(r0_vector)|is.infinite(r0_vector)]<-0
  r0_Preds[r0_Preds<0|is.nan(r0_Preds)|is.infinite(r0_Preds)]<-0
  
  return(bind_cols(dataset_Predicted
                   , "Beta_Predicted"=c(trans_Preds,0)
                   , "Recov_Predicted"=c(recov_Preds,0)
                   , "Death_Predicted"=c(death_Preds,0)
                   , "R0_Predicted"=r0_Preds
                   , "transmission"=c(trans_vector,0)
                   , "recovery"=c(recov_vector,0)
                   , "death_rate"=c(death_vector,0)
                   , "R0"=r0_vector))
}

#----------------------------

mySIRD_plot<-function(results, Province="Torino", j=7, d=15, cut_point=FALSE){
  
  population <- myFindPopulation(reg=Province)
  
  end_1 <- nrow(results)
  
  end_2 <- end_1-1
  
  Preds <- results[1:end_2,]
  
  if (isFALSE(cut_point)) {
    n <- end_1-d
    len <- n-1
    real_Val <- results[1:n,1:5]
    real_Par <- Preds[(j+1):len,10:13]
  } else {
    real_Val <- myCreateDataset(r=Province)
    real_Par <- myParam_comp(region=Province, model=FALSE, cut_point=FALSE)
    n <- nrow(real_Val)
    len <- n-1
  }
  
  date<-seq(as.Date("24 Feb 2020","%d %b %Y"), length.out = n+d, by=1)
  
  height <-seq(0 ,max(population-results$susceptible))
  
  SIRDgraph <- plot_ly(type="scatter",mode="markers")%>%
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
    add_trace(x=-100000, y=-10000000,mode = "markers", name="<b>Predictions</b>",
              marker=list(color="white")) %>%
    add_trace(x=date[1:end_1],  y=population-results$susceptible,mode = "lines", name="Total Cases",
              line=list(color="green",width = 4))%>%
    add_trace(x=date[1:end_1],  y=results$new_cases,mode = "lines", name="New Cases",
              line=list(color="yellow",width = 4))%>%
    add_trace(x=date[1:end_1], y=results$cases,mode = "lines",type="scatter",name="Infected",
              line = list(color = "red", width = 4))%>%
    add_trace(x=date[1:end_1],  y=results$recovered,mode = "lines", name="Recovered", 
              line=list(color="blue",width = 4))%>%
    add_trace(x=date[1:end_1],  y=results$deaths, mode = "lines", name="Deaths",
              line=list(color="grey",width = 4))%>%
    ##real data
    add_trace(x=-100000,  y=-100000000, name="   ",
              mode="markers", marker=list(color="white", line=list(color="white",width=1)))%>%
    add_trace(x=-100000,  y=-1000000, name="<b>Real Data</b>",
              mode="markers", marker=list(color="white", line=list(color="white",width=1)))%>%
    add_trace(x=date[1:n],  y=population-real_Val$susceptible, name="Total Cases", 
              mode="markers", marker=list(color="green", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=real_Val$new_cases, name="New Cases",
              mode="markers", marker=list(color="yellow", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=real_Val$cases,name="Infected",
              mode="markers", marker=list(color="red", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=real_Val$recovered, name="Recovered",
              mode="markers", marker=list(color="blue", line=list(color="black",width=1)))%>%
    add_trace(x=date[1:n],  y=real_Val$deaths, name="Deaths",
              mode="markers", marker=list(color="grey", line=list(color="black",width=1)))%>%
    layout(title = sprintf('<b> SIRD Model - %s </b>',Province),
           xaxis = list(title = "Days",type='date',tickformat = "%d %B", tickangle=45,range=c(date[1],date[n+d])), ###
           yaxis = list(title = "Individuals",range=c(0,max(population-results$susceptible))),
           legend=list(x=100,y=0.5))
  
  type_title<-list(xref = "paper",yref = "paper",
                   yanchor = "bottom",xanchor = "center",align = "center",x = 0.5,y = 1.1,
                   showarrow = F)
  
  height <- seq(0 ,max(real_Par$R0),by=0.01)
  
  base_plot <- plot_ly(type="scatter",mode="markers")%>%
    add_trace(x =rep(date[10],times=length(height)) , 
              y =height,
              mode = "lines",line=list(color="grey", width=1, dash="dot"),
              text="School closure", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[14],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", width=1, dash="dot"),
              text="Northern regions lockdown", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[17],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", width=1, dash="dot"),
              text="National lockdown", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[27],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", width=1, dash="dot"),
              text="Only key activities", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[71],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", width=1, dash="dot"),
              text="Phase 2: reopening", showlegend=F, hoverinfo='text')%>%
    add_trace(x =rep(date[85],times=length(height)) , 
              y =height,
              mode = "lines", line=list(color="grey", width=1, dash="dot"),
              text="Phase 2: Lifted lockdown within regions", showlegend=F, hoverinfo='text')%>%
    layout(xaxis = list(type='date',tickformat = "%d-%m",tickangle=45, 
                        range=c(date[j+1],date[len+d])))
  
  
  beta_plot <- base_plot%>%
    add_trace(x=date[(j+1):len], y=real_Par$transmission,mode = "markers", showlegend=F,
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(j+1):end_2], y=Preds$Beta_Predicted[(j+1):end_2],mode = "lines", showlegend=F,
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(annotations= c(text = "<b>Transmission rate</b>",type_title),
           yaxis=list(range=c(0,max(Preds$Beta_Predicted[(j+1):end_2]))))
  
  recovery_plot<-base_plot %>%
    add_trace(x=date[(j+1):len], y=real_Par$recovery,mode = "markers", showlegend=F,
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(j+1):end_2], y=Preds$Recov_Predicted[(j+1):end_2],mode = "lines", showlegend=F,
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(annotations= c(text = "<b>Recovery rate</b>",type_title),
           yaxis=list(range=c(0,max(Preds$Recov_Predicted[(j+1):end_2]))))
  
  mortality_plot<-base_plot%>%
    add_trace(x=date[(j+1):len], y=real_Par$death_rate,mode = "markers", showlegend=F,
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    add_trace(x=date[(j+1):end_2], y=Preds$Death_Predicted[(j+1):end_2],mode = "lines", showlegend=F,
              line=list(color="black",width=1),name="Model Predictions")%>%
    layout(annotations= c(text = "<b>Mortality rate</b>",type_title),
           yaxis=list(range=c(0,max(Preds$Death_Predicted[(j+1):end_2]))))
  
  r0_plot<-base_plot%>%
    #plot real values
    add_trace(x=date[(j+1):len], y=real_Par$R0,mode = "markers", showlegend=F,
              marker=list(color="chartreuse", line=list(color="black",width=1)),name="Real Values")%>%
    #plot predictions
    add_trace(x=date[(j+1):end_2], y=Preds$R0_Predicted[(j+1):end_2],mode = "lines", showlegend=F,
              line=list(color="black",width=1),name="Model Predictions")%>%
    add_segments(x =date[1] , xend =date[len+d] , y =1 , yend = 1,showlegend=F,line=list(color="grey",dash="dash"), name="R0=1")%>%
    layout(yaxis=list(range=c(0,max(real_Par$R0))),
           annotations= c(text = "<b>R0</b>",type_title))
  
  param_plots <- subplot(beta_plot,r0_plot,recovery_plot,mortality_plot, nrows = 2, margin=0.1)
  
  final_figure <- subplot(SIRDgraph, param_plots, nrows = 2, margin=0.1)%>%
    layout(showlegend=T,title = sprintf('<b> SIRD Model - %s </b>',Province),
           xaxis = list(title = "Days"), ###
           yaxis = list(title = "Individuals"),
           legend=list(x=100,y=1,xanchor="left", yanchor="top"),
           height=800)%>%
    config(modeBarButtonsToRemove = c("lasso2d", "select2d",  "toggleSpikelines",
                                      "hoverClosestCartesian","hoverCompareCartesian",
                                      "autoScale2d"))
  
  return(final_figure)
}

SIRDParameterDataset <- function() {
  Param_Dataset <- data.frame()
  for (i in unique(mortiProvinces$provincia)) {
    param_prov <- myParam_comp(region=i, model=FALSE, cut_point = FALSE)
    n <- nrow(param_prov)
    param_prov <- rbind(param_prov[c(1:3),],zoo::rollmean(param_prov,7),param_prov[c((n-2):n),])
    attrib <- pcmTOTData %>%
      filter(denominazione_provincia==i)%>%
      select(denominazione_regione, denominazione_provincia,long.x, lat.x, data) %>%
      arrange(data)
    attrib <- attrib[1:n,]
    Param_Dataset <- rbind.data.frame(Param_Dataset, 
                                      cbind.data.frame(
                                        province_name=attrib$denominazione_provincia
                                        ,date=attrib$data
                                        ,long=attrib$long.x
                                        ,lat=attrib$lat.x
                                        ,"Transmission rate"=param_prov$transmission
                                        ,"Recovery rate"=param_prov$recovery
                                        ,"Mortality rate"=param_prov$death_rate
                                        ,"Basic reproduction number"=param_prov$R0))
  }
  return(Param_Dataset)
}


SIRD_Map <- function(database, var) {
  
  colnames(database)[5:8] <- c("Transmission_rate","Recovery_rate","Mortality_rate","Basic_reproduction_number")
  
  if (var=="Recovery Rate") {
    database <- database[which(database$Recovery_rate>0),]
  }
  
  # geojson file for Italy
  geo_italy <- geojsonio::geojson_read("~/Covid-19-Dashboard/data/geo_italy.geojson", what = "sp")
  
  #
  pal <- leaflet::colorNumeric(c("blue","yellow","red"), NULL)  # viridis   YlOrRd  RdYlBu
  
  # merge geojson with our data
  db_geo <- sp::merge(geo_italy,database,by.x ="prov_name",by.y="province_name")
  
  db_geo$Recovery_rate <- round(db_geo$Recovery_rate,3)
  db_geo$Transmission_rate <- round(db_geo$Transmission_rate,3)
  db_geo$Mortality_rate <- round(db_geo$Mortality_rate,3)
  db_geo$Basic_reproduction_number <- round(db_geo$Basic_reproduction_number,3)
  
  if (var=="Recovery Rate") {
   
     labels <- sprintf(
      "<strong>Province: %s</strong><br/> %s: %g<br/>Date: %s",
      db_geo$prov_name, var, round(db_geo$Recovery_rate,3),db_geo$date
    ) %>% lapply(htmltools::HTML)
     
     # labels1 <- sprintf(
     #   "<strong>Province: %s</strong><br/> No Data Available",
     #   db_geo$prov_name
     # ) %>% lapply(htmltools::HTML)
    # ifelse(is.na(values),labels1,labels)
     
    provMap <- leaflet(db_geo) %>%
      setView(lat=41.8719, lng=12.5674,zoom=6 ) %>%
      # fitBounds(lng1 = 15,lat1 =43 ,lng2 = 9,lat2 =36 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Recovery_rate),
                  label = labels) %>%
      addLegend(pal = pal, values = ~Recovery_rate, opacity = 0.7, title = "Recovery Rate",
                position = "topright",na.label = "No Data")
  }
  
  else if (var=="Transmission Rate") {
    
    labels <- sprintf(
      "<strong>Province: %s</strong><br/> %s: %g<br/>Date: %s",
      db_geo$prov_name, var, round(db_geo$Transmission_rate,3),db_geo$date
    ) %>% lapply(htmltools::HTML)
    
    provMap <- leaflet(db_geo) %>%
      setView(lat=41.8719, lng=12.5674,zoom=6 ) %>%
      # fitBounds(lng1 = 15,lat1 =43 ,lng2 = 9,lat2 =36 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Transmission_rate),
                  label = labels) %>%
      addLegend(pal = pal, values = ~Transmission_rate, opacity = 0.7, title = "Transmission Rate",
                position = "topright",na.label = "No Data")
  }
  
  else if (var=="Mortality Rate") {
    labels <- sprintf(
      "<strong>Province: %s</strong><br/> %s: %g<br/>Date: %s",
      db_geo$prov_name, var, round(db_geo$Mortality_rate,3),db_geo$date
    ) %>% lapply(htmltools::HTML)
    
    provMap <- leaflet(db_geo) %>%
      setView(lat=41.8719, lng=12.5674,zoom=6 ) %>%
      # fitBounds(lng1 = 15,lat1 =43 ,lng2 = 9,lat2 =36 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Mortality_rate),
                  label = labels) %>%
      addLegend(pal = pal, values = ~Mortality_rate, opacity = 0.7, title = "Mortality Rate",
                position = "topright",na.label = "No Data")
  }    
  
  else if (var=="Basic Reproduction Number") {
    labels <- sprintf(
      "<strong>Province: %s</strong><br/> %s: %g<br/>Date: %s",
      db_geo$prov_name, var, round(db_geo$Basic_reproduction_number,3),db_geo$date
    ) %>% lapply(htmltools::HTML)
    
    provMap <- leaflet(db_geo) %>%
      setView(lat=41.8719, lng=12.5674,zoom=6 ) %>%
      # fitBounds(lng1 = 15,lat1 =43 ,lng2 = 9,lat2 =36 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(Basic_reproduction_number),
                  label = labels) %>%
      addLegend(pal = pal, values = ~Basic_reproduction_number, opacity = 0.7, title = "Basic Reproduction Number",
                position = "topright",na.label = "No Data")
  } 
  
  #Map plot----
  return(provMap)
  
}
