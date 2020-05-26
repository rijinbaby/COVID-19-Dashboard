#Install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

#Load Packages
library(tidyverse)

prevIDXBound <- function(dataset, rangeDays){
  
  options(warn=-1)
  
  #NationalIDX----
  ItaPrevIDX <- pcmTOTData %>%
    ungroup() %>%
    select(data
           , totale_casi.x
           , popolazione) %>%
    group_by(data) %>%
    summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    filter(data<=rangeDays)
  
  ItaPrevIDX$prevIDX <- round((ItaPrevIDX$totale_casi.x/ItaPrevIDX$popolazione)*100000, digits = 1)
  
  #ProvinceBound----
  datePrevIDX<-pcmTOTData%>%
    filter(data==rangeDays)
  
  
  lowerPR<-pcmTOTData %>%
    filter(data==rangeDays) %>%
    filter(prevIndex%in%sort(datePrevIDX$prevIndex, decreasing = F)[1]) %>%
    ungroup() %>%
    select(denominazione_provincia)
  
  upperPR<-pcmTOTData %>%
    filter(data==rangeDays) %>%
    filter(prevIndex%in%sort(datePrevIDX$prevIndex, decreasing = T)[1]) %>%
    ungroup() %>%
    select(denominazione_provincia)
  
  lowerPrevIDX<-pcmTOTData%>%
    filter(data<=rangeDays)%>%
    filter(denominazione_provincia%in%lowerPR$denominazione_provincia) %>%
    ungroup() %>%
    select(data
           , prevIndex) %>%
    group_by(data) %>%
    summarize_if(is.numeric, mean, na.rm = TRUE)
  
  lowerPrevIDX$prevIndex <- round(lowerPrevIDX$prevIndex, digits = 1)
  
  upperPrevIDX<-pcmTOTData%>%
    filter(data<=rangeDays)%>%
    filter(denominazione_provincia%in%upperPR$denominazione_provincia) %>%
    ungroup() %>%
    select(data
           , prevIndex) %>%
    group_by(data) %>%
    summarize_if(is.numeric, mean, na.rm = TRUE)
  
  upperPrevIDX$prevIndex <- round(upperPrevIDX$prevIndex, digits = 1)
  
  totalPrevIDX <- as.data.frame(matrix(nrow=nrow(ItaPrevIDX)+1, ncol=4))
  colnames(totalPrevIDX) <- c("data", "natIDX", "upIDX", "lowIDX")
  
  totalPrevIDX$data <- c(as.Date(ItaPrevIDX$data[1])-1, ItaPrevIDX$data)
  totalPrevIDX$natIDX <- c(0, ItaPrevIDX$prevIDX)
  totalPrevIDX$upIDX <- round(c(0, upperPrevIDX$prevIndex), digits = 1)
  totalPrevIDX$lowIDX <- round(c(0, lowerPrevIDX$prevIndex), digits = 1)
  
  totalPrevIDX <<- totalPrevIDX
  upperPR <<- as.character(upperPR[1,])
  lowerPR <<- as.character(lowerPR[1,])
}
