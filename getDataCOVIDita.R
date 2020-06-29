# #Install packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, readxl, writexl, RPostgreSQL, RPostgres, zoo)
# 
# #Load Packages
# library(tidyverse)
# library(readxl)
# library(writexl)

#Usefull Functions
`%notin%` <- Negate(`%in%`)

Path<-"C:/Users/Rijin/Documents/Covid-19-Dashboard/data/"

italy_data <- function(){
  pcm_data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv") 
  
  pcm_data$data<-as.Date(pcm_data$data, "%Y-%m-%d")
  # pcm_data <- pcm_data[which(pcm_data$data<=ip_date),]
  last_day <- max(pcm_data$data)
  last_day_bf <- last_day - 1
  
  active <- pcm_data$totale_positivi[which(pcm_data$data==last_day)]
  active_diff <- (active - pcm_data$totale_positivi[which(pcm_data$data==last_day_bf)])
    
  recovered <- pcm_data$dimessi_guariti[which(pcm_data$data==last_day)]
  recovered_diff <- (recovered - pcm_data$dimessi_guariti[which(pcm_data$data==last_day_bf)])
    
  death <- pcm_data$deceduti[which(pcm_data$data==last_day)]
  death_diff <- (death - pcm_data$deceduti[which(pcm_data$data==last_day_bf)])
  
  confirmed <- active+recovered+death
  confirmed_diff <- pcm_data$nuovi_positivi[which(pcm_data$data==last_day)]
  
  info_box_data <- data.frame(confirmed,confirmed_diff,active,active_diff,recovered,recovered_diff,death,death_diff)
  
  info_box_data <<- info_box_data

}


#ProtezioneCivile - Regioni
downAndSaveDataPMC <- function(pcmRegLink="https://raw.github.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
                              , pcmProvLink="https://raw.github.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
                              , pcmNazLink="https://raw.github.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
                              , dataPath=Path){
  
  options(warn=-1)
  #------
  pcmRegData <- tryCatch({
    readr::read_csv(pcmRegLink)
  }, error = function(e) {
    readr::read_csv(paste0(dataPath, "PCM/dpc-covid19-ita-regioni.csv"))
  })
  
  pcmRegData <- pcmRegData[,which(!is.na(colnames(pcmRegData)))]
  
  pcmRegData$denominazione_regione[pcmRegData$denominazione_regione=="P.A. Trento"] <- "Trentino Alto Adige"
  pcmRegData$denominazione_regione[pcmRegData$denominazione_regione=="P.A. Bolzano"] <- "Trentino Alto Adige"
  
  pcmRegData <- pcmRegData %>%
    group_by(
      denominazione_regione
      , data
    ) %>% 
    summarize_if(is.numeric, sum, na.rm = TRUE)
  
  pcmRegData$data<-as.Date(pcmRegData$data, "%Y-%m-%d")

  #-----
  pcmProvData <- tryCatch({
    readr::read_csv(pcmProvLink)
  }, error = function(e) {
    readr::read_csv(paste0(dataPath, "PCM/dpc-covid19-ita-province.csv"))
  })
  
  pcmProvData <- pcmProvData[which(pcmProvData$denominazione_provincia!="fuori Regione/P.A."),]
  pcmProvData <- pcmProvData[which(pcmProvData$denominazione_provincia!="In fase di definizione/aggiornamento"),]
  pcmProvData <- pcmProvData[which(pcmProvData$denominazione_provincia!="In fase di definizione"),]
  pcmProvData <- pcmProvData[,c(colnames(pcmProvData)[1:(length(colnames(pcmProvData))-2)])]
  
  pcmProvData$denominazione_regione[pcmProvData$denominazione_regione=="P.A. Trento"] <- "Trentino Alto Adige"
  pcmProvData$denominazione_regione[pcmProvData$denominazione_regione=="P.A. Bolzano"] <- "Trentino Alto Adige"
  
  pcmProvData <- pcmProvData[,which(!is.na(colnames(pcmProvData)))]
  
  pcmProvData <- pcmProvData %>%
    ungroup()%>%
    select(
      denominazione_regione
      , denominazione_provincia
      , data
      , lat
      , long
      , totale_casi
    ) %>%
    group_by(
      denominazione_regione
      , denominazione_provincia
      , data
    ) %>% 
    summarize_if(is.numeric, sum, na.rm = TRUE)
 
  pcmProvData <- pcmProvData[which(pcmProvData$denominazione_provincia!="denominazione_provincia"),]
  
  pcmProvData$denominazione_provincia <- iconv(pcmProvData$denominazione_provincia,from="UTF-8",to="ASCII//TRANSLIT")

  pcmProvData$data<-as.Date(pcmProvData$data, "%Y-%m-%d")

  #-----
  datISTAT <- readr::read_csv(paste0(dataPath, "datISTAT", ".csv"))
  #-----
  pcmProvRegData <- pcmProvData %>%
    dplyr::left_join(pcmRegData, by = c("denominazione_regione" = "denominazione_regione"
                                        , "data" = "data")) %>%
    dplyr::left_join(datISTAT, by = c("denominazione_provincia" = "provincia"))
  
  pcmProvRegData$prevIndex <- round((pcmProvRegData$totale_casi.x/pcmProvRegData$popolazione)*100000)
  #-----
  pcmNazData <- tryCatch({
    readr::read_csv(pcmNazLink)
  }, error = function(e) {
    readr::read_csv(paste0(dataPath, "PCM/dpc-covid19-ita-andamento-nazionale.csv"))
  })
  
  pcmNazData <- pcmNazData[,c(colnames(pcmNazData)[1:(length(colnames(pcmNazData))-2)])]
  pcmNazData$data<-as.Date(pcmNazData$data, "%Y-%m-%d")
  
  pcmProvRegNazData <- pcmProvRegData %>%
    dplyr::left_join(pcmNazData, by = c("data" = "data"))
  #-----
  popPerRegione <- pcmProvRegNazData %>%
    ungroup() %>%
    select(
      denominazione_regione
      , data
      , popolazione
    ) %>%
    group_by(
      denominazione_regione
      , data
    ) %>%
    summarize_if(is.numeric, sum, na.rm = TRUE)
  
  colnames(popPerRegione) <- c("denominazione_regione", "data", "popolReg")

  pcmTOTData <- pcmProvRegNazData %>%
    dplyr::left_join(popPerRegione, by = c("denominazione_regione" = "denominazione_regione"
                                           , "data" = "data"))

  pcmTOTData$guarPrevIdxReg <- round((pcmTOTData$dimessi_guariti.x/pcmTOTData$popolReg)*100000)
  pcmTOTData$decePrevIdxReg <- round((pcmTOTData$deceduti.x/pcmTOTData$popolReg)*100000)
  pcmTOTData$actualPrevIdxReg <- round((pcmTOTData$totale_casi.y/pcmTOTData$popolReg)*100000)
  #-------
  pcmTOTData$ricSinPrevIdxReg <- round((pcmTOTData$ricoverati_con_sintomi.x/pcmTOTData$popolReg)*100000)
  pcmTOTData$terIntPrevIdxReg <- round((pcmTOTData$terapia_intensiva.x/pcmTOTData$popolReg)*100000)
  pcmTOTData$isolDomPrevIdxReg <- round((pcmTOTData$isolamento_domiciliare.x/pcmTOTData$popolReg)*100000)
  pcmTOTData$totPosPrevIdxReg <- round((pcmTOTData$totale_positivi.x/pcmTOTData$popolReg)*100000)
  #-----
  pcmTOTData <<- pcmTOTData
  
}

#--------------------------------------------------------

#GDRIVE dati provinciali
##  https://gargle.r-lib.org/articles/get-api-credentials.html#oauth-client-id-and-secret

downDatiSQL <- function(psw="1029qpwo"){
  
  con <- RPostgreSQL::dbConnect(RPostgres::Postgres()
                                , dbname = "COVID19"
                                , host="unisid-dm-demm.unisid.unimi.it"
                                , port="5432"
                                , user="getTweet"
                                , password=psw)
  
  mortiProvinces <- RPostgreSQL::dbGetQuery(conn = con
                                            , statement = "SELECT DISTINCT *
                                                          FROM public.mortiprovincia")
  
  mortiProvinces$data <- zoo::as.Date(as.numeric(mortiProvinces$data), origin="1970/01/01")
  
  for(col in c("nuovi_casi"
               , "casi_att_positivi"
               , "decessi"
               , "decessi_tot"
               , "tasso_letalita"
               , "tasso_incidenza_100_mila")){
    mortiProvinces[[col]] <- as.numeric(mortiProvinces[[col]])
  }
  
  for(i in 1:nrow(mortiProvinces)){
    if(mortiProvinces$provincia[i]=="Reggio Calabria"){
      mortiProvinces$provincia[i] <- "Reggio di Calabria"
    } else if(mortiProvinces$provincia[i]=="L Aquila"){
      mortiProvinces$provincia[i] <- "L'Aquila"
    } else if(mortiProvinces$provincia[i]=="Reggio nell Emilia"){
      mortiProvinces$provincia[i] <- "Reggio nell'Emilia"
    }
  }
  
  mortiProvinces <- as_tibble(mortiProvinces)
  mortiProvinces <<- mortiProvinces[order(mortiProvinces$data),]
}

#--------------------------------------------------------

#ISTAT dati comunali
downAndSaveDataISTAT <- function(psw="1029qpwo"
                                 , pcmTotData=pcmTOTData
                                 , mortiProv=mortiProvinces){

  # con <- RPostgreSQL::dbConnect(RPostgres::Postgres()
  #                               , dbname = "COVID19"
  #                               , host="unisid-dm-demm.unisid.unimi.it"
  #                               , port="5432"
  #                               , user="getTweet"
  #                               , password=psw)
  # 
  # deathDates <- RPostgreSQL::dbGetQuery(conn = con
  #                                           , statement = "SELECT DISTINCT *
  #                                                         FROM public.deathDates")
  # 
  # deathDates$data <- zoo::as.Date(as.numeric(deathDates$data), origin="1970/01/01")

  
  # Read ISTAT updated file
  deathDates <- readr::read_csv(paste0(Path, "/ISTAT/ISTAT_Updated_Till_2020-04-30", ".csv"))
  
  
  
  unique(mortiProv$provincia)[unique(mortiProv$provincia)%notin%unique(deathDates$province)]
  #unique(mortiProv$provincia)[unique(mortiProv$provincia)%notin%unique(pcmTOTData$denominazione_provincia)]
  
  for(i in 1:nrow(deathDates)){
    if(deathDates$province[i]=="Reggio Calabria"){
      deathDates$province[i] <- "Reggio di Calabria"
    } else if(deathDates$province[i]=="L Aquila"){
      deathDates$province[i] <- "L'Aquila"
    } else if(deathDates$province[i]=="Reggio nell Emilia"){
      deathDates$province[i] <- "Reggio nell'Emilia"
    }
  }
  
  deathDates <- deathDates[order(deathDates$data),]
  #-------
  deathDates <- as_tibble(deathDates)
  mortiProv <- as_tibble(mortiProv)
  
  dateDeaths <- unique(deathDates$data)[which(unique(deathDates$data)%in%unique(mortiProv$data))]
  
  deathDates$weekDeaths <- rep(NA, nrow(deathDates))
  
  for(from in seq(length(dateDeaths)-1)){
    to <- from+1
    fromDate <- dateDeaths[from]
    toDate <- dateDeaths[to]
    
    for(prov in unique(mortiProv$provincia)){
      covidDeaths <- mortiProv$decessi[which(mortiProv$data>fromDate
                              &mortiProv$data<=toDate
                              &mortiProv$provincia==prov)]
      deathDates$weekDeaths[which(deathDates$data>fromDate
                                  &deathDates$data<=toDate
                                  &deathDates$province==prov)] <- sum(covidDeaths)
    }
  }
  
  
  
  deathDates <- left_join(deathDates
                          , mortiProv
                          , by=c("province" = "provincia"
                                 , "data" = "data"))
  
  regToJoin <- pcmTotData %>%
    select(
      denominazione_regione
      , denominazione_provincia
    ) %>%
    distinct(
      denominazione_regione
      , denominazione_provincia
    )
  
  deathDatesReg <<- left_join(deathDates
                          , regToJoin
                          , by=c("province" = "denominazione_provincia")) 

}

#-----------------------------------

cumulDeathsProv <- function(mortiProv=mortiProvinces, pcmDataTot=pcmTOTData){
  
  cumDeathsProv <- mortiProv %>%
    dplyr::left_join(pcmDataTot[,c("denominazione_provincia", "data", "popolazione")], by = c("provincia" = "denominazione_provincia"
                                           , "data" = "data"))
  
  cumDeathsProv$deathRates <- round((cumDeathsProv$decessi_tot/cumDeathsProv$popolazione)*100000, digits = 2)
  
  # #add column Regione
  {
    listlazio <- c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
    listmarche <- c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
    listsardegna <- c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
    listpuglia <- c("Bari", "Barletta-Andria-Trani", "Brindisi", "Lecce", "Taranto","Foggia")
    listpiemonte <- c("Alessandria", "Asti", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli","Biella")
    listvalledaosta <- c("Aosta")
    listtrentinoaltoadige <- c("Bolzano", "Trento")
    listabruzzo <- c("Chieti", "L Aquila", "Pescara", "Teramo")
    listbasilicata <- c("Matera", "Potenza")
    listcalabria <- c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
    listsicilia <- c("Agrigento","Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
    listemiliaromagna <- c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell Emilia", "Rimini")
    listmolise <- c("Campobasso", "Isernia")
    listtoscana <- c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
    listumbria <- c("Perugia", "Terni")
    listliguria <- c("Imperia","Savona")
    listlombardia <- c("Cremona")
    # #lombardia and liguria not reported becuase of missing data 
    # 
    # #add column Regione to our dataframe
    cumDeathsProv$Regione <-ifelse (cumDeathsProv$provincia %in% listlazio, "Lazio",
                              ifelse (cumDeathsProv$provincia %in% listsardegna, "Sardegna",
                                      ifelse (cumDeathsProv$provincia %in% listpuglia, "Puglia",
                                              ifelse (cumDeathsProv$provincia %in% listvalledaosta, "Valle d aosta",
                                                      ifelse (cumDeathsProv$provincia %in% listtrentinoaltoadige, "Trentino alto adige",
                                                              ifelse (cumDeathsProv$provincia %in% listpiemonte, "Piemonte",
                                                                      ifelse (cumDeathsProv$provincia %in% listmarche, "Marche",
                                                                              ifelse (cumDeathsProv$provincia %in% listabruzzo, "Abruzzo",
                                                                                      ifelse (cumDeathsProv$provincia %in% listbasilicata, "Basilicata",
                                                                                              ifelse (cumDeathsProv$provincia %in% listcalabria, "Calabria",
                                                                                                      ifelse (cumDeathsProv$provincia %in% listsicilia, "Sicilia",
                                                                                                              ifelse (cumDeathsProv$provincia %in% listemiliaromagna, "Emilia-Romagna",
                                                                                                                      ifelse (cumDeathsProv$provincia %in% listmolise, "Molise",
                                                                                                                              ifelse (cumDeathsProv$provincia %in% listtoscana, "Toscana",
                                                                                                                                      ifelse (cumDeathsProv$provincia %in% listumbria, "Umbria",
                                                                                                                                              ifelse(cumDeathsProv$provincia %in% listliguria,"Liguria",
                                                                                                                                                     ifelse(cumDeathsProv$provincia %in% listlombardia,"Lombardia",NA)))))))))))))))))
  }
  
  cumDeathsProv<<-cumDeathsProv
  
}

customSentence <- function(numItems, type) {
  paste("Feedback & Suggestions")
}

customSentence_share <- function(numItems, type) {
  paste("Share it!")
}

dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence_share) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}
