body <- dashboardBody(
  
  tags$head(tags$style(HTML('
                          /* body */
                            .content-wrapper, .right-side {
                            background-color: black;
                            }
                          ')
                       , 'body {color:blue;}')),
  
  tabItems(
  # "Provinces map"----
  tabItem(tabName = "ProvinceMap",
          tags$h2("Province Map"),
          fluidRow(
                   box(leafletOutput("provinceMap")
                       , width = 9, height = 470
                       , status = "info"
                       #, background = "aqua"
                       , collapsible = F, solidHeader = T
                   ),

            column(width = 3,
                   box(width = NULL, height = 202
                       , status = "info"
                       #, background = "aqua"
                       , selectInput(inputId="Province"
                                   , label="Choose a variable to display"
                                   , choices=c("Province cumulative cases"
                                               , "Province cumulative rates")
                                   , selected="Province cumulative cases")
                       
                       , sliderInput(inputId="selectDate1"
                                   , label="End date:"
                                   , min = rangeDate[1]
                                   , max = rangeDate[2]
                                   , value = rangeDate[2]
                                   , timeFormat="%Y-%m-%d")
                       
                   ),

                   box(width = NULL, height = 250
                       , status = "info"
                       #, background = "aqua"
                       , h5(class = "text-muted"
                          , br()
                          , uiOutput(outputId = "textPres")
                          , style = "color : black;"
                       )
                   ))
          )
          
          ,fluidRow(
            box(plotlyOutput("Drates_TS")
                , width = 09, height = 425, status = "warning"
                , collapsible = F, solidHeader = T)
            
            ,column(width = 3,
                   box(width = NULL, height = 425
                         , status = "info"
                         #, background = "aqua"
                         , h5(class = "text-muted"
                              , br()
                              , uiOutput(outputId = "textDrates")
                              , style = "color : black;"
                         )
                         , downloadButton("ProvList1", "Province list")
                   )
              )
          )
          
          ,fluidRow(
            box(plotlyOutput("N_TS")
                , width = 9, height = 425, status = "warning"
                , collapsible = F, solidHeader = T),
            
            column(width = 3,
                   box(width = NULL, height = 101
                       , status = "warning"
                       #, background = "aqua"
                       , sliderInput(inputId="selectDate2"
                                     , label="End date:"
                                     , min = rangeDate[1]
                                     , max = rangeDate[2]
                                     , value = rangeDate[2]
                                     , timeFormat="%Y-%m-%d")
                   )
                   
                   ,box(plotlyOutput("BoxPLTy")
                        , width = NULL, height = 303, status = "warning"
                        , collapsible = F, solidHeader = T)
            )
          )
  ),
  
  # "Time Series"----
  tabItem(tabName = "TimeSeries",
          tags$h2("Time Series")
          
          ,fluidRow(
            box(plotlyOutput("P_TS")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            ),
            
            column(width = 3,
                   box(width = NULL, height = 202
                       , status = "info"
                       #, background = "aqua"
                       , selectInput(inputId='inProv'
                                     , label='Province:'
                                     , selected=c("Torino","Cremona", "Piacenza", "Bergamo", "Milano")
                                     , choices=list(
                                       "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
                                       , "Valle d'Aosta"=c("Aosta", "")
                                       , "Liguria"=c("Genova", "Imperia", "La Spezia", "Savona")
                                       , "Lombardia"=c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese")
                                       , "Trentino Alto Adige"=c("Bolzano", "Trento")
                                       , "Veneto"=c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
                                       , "Friuli Venezia Giulia"=c("Gorizia", "Pordenone", "Trieste", "Udine")
                                       , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
                                       , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
                                       , "Umbria"=c("Perugia", "Terni")
                                       , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
                                       , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
                                       , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
                                       , "Molise"=c("Campobasso", "Isernia")
                                       , "Campania"=c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno")
                                       , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
                                       , "Basilicata"=c("Matera", "Potenza")
                                       , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
                                       , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
                                       , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
                                       )
                                     , multiple=TRUE
                                     , selectize=TRUE)
                       , selectInput(inputId='inVarP'
                                     , label='Variable:'
                                     , selected=c("Cumulative cases")
                                     , choices=c("Cumulative cases", "Cumulative rates")
                                     , multiple=FALSE
                                     , selectize=TRUE)
                       )
                   
                   , box(width = NULL, height = 202
                        , status = "info"
                        #, background = "aqua"
                        , h5(class = "text-muted"
                            , br()
                             , uiOutput(outputId = "textPLOT1")
                             , style = "color : black;"
                        )
                        , downloadButton("PLOT1", "Read more")
                   )
                   
                   )
          )
          
          ,fluidRow(
            box(plotlyOutput("R_TS1")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            )
          
            # tabBox(
            #   width = 9, height = 425,
            #   # The id lets us use input$tabset1 on the server to find the current tab
            #   tabPanel("Province values", plotlyOutput("R_TS1"))
            #   ,tabPanel("Region Values", 'plotlyOutput("R_TS2")')
            # )
            
            ,column(width = 3,
                   box(width = NULL, height = 202
                       , status = "info"
                       #, background = "aqua"
                       , selectInput(inputId='inReg'
                                     , label='Regions:'
                                     , selected="Lombardia"
                                     , choices=sort(unique(pcmTOTData$denominazione_regione))
                                     , multiple=FALSE
                                     , selectize=TRUE)
                       , selectInput(inputId='inVarR'
                                     , label='Variable:'
                                     , selected=c("Cumulative cases")
                                     , choices=c("Cumulative cases", "Cumulative rates")
                                     , multiple=FALSE
                                     , selectize=TRUE)
                   )
                   
                   , box(width = NULL, height = 202
                         , status = "info"
                         #, background = "aqua"
                         , h5(class = "text-muted"
                              , br()
                              , uiOutput(outputId = "textPLOT2")
                              , style = "color : black;"
                         )
                         , downloadButton("PLOT2", "Read more")
                   )
          )
          )
          
          ,fluidRow(
            box(plotlyOutput("R_TS2")
                , width = 6, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            )
            
            ,box(plotlyOutput("R_TS3"),
                width = 6, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            )

          )
          
          ,fluidRow(
            box(plotlyOutput("D_TS")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            ),
            
            column(width = 3,
                   box(width = NULL, height = 202
                       , status = "info"
                       #, background = "aqua"
                       , selectInput(inputId='inProvD'
                                     , label='Province:'
                                     , selected="Torino"
                                     , choices=list(
                                       "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
                                       , "Valle d'Aosta"=c("Aosta", "")
                                       , "Liguria"=c("Genova", "Imperia", "La Spezia", "Savona")
                                       , "Lombardia"=c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese")
                                       , "Trentino Alto Adige"=c("Bolzano", "Trento")
                                       , "Veneto"=c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
                                       , "Friuli Venezia Giulia"=c("Gorizia", "Pordenone", "Trieste", "Udine")
                                       , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
                                       , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
                                       , "Umbria"=c("Perugia", "Terni")
                                       , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
                                       , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
                                       , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
                                       , "Molise"=c("Campobasso", "Isernia")
                                       , "Campania"=c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno")
                                       , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
                                       , "Basilicata"=c("Matera", "Potenza")
                                       , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
                                       , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
                                       , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
                                     )
                                     , multiple=FALSE
                                     , selectize=TRUE)
                   )
                       
                   , box(width = NULL, height = 202
                         , status = "info"
                         #, background = "aqua"
                         , h5(class = "text-muted"
                              , br()
                              , uiOutput(outputId = "textPLOT3")
                              , style = "color : black;"
                         )
                         , downloadButton("PLOT3", "Read more")
                         , downloadButton("ProvList", "Province list")
                   )
                   
              )
            
          )
  
  ),
  
  # "Models"----
  tabItem(tabName = "SIRDModels",
          tags$h2("SIRD Models")
          
          ,fluidRow(
            box(plotlyOutput("SIRDp")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            )
            
            
            
            , column(width = 3,
                   box(width = NULL, height = 425
                       , status = "info"
                       #, background = "aqua"
                       
                       , selectInput(inputId="SIRprovCV"
                                     , label='Province:'
                                     , selected="Torino"
                                     , choices=list(
                                       "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
                                       , "Valle d'Aosta"=c("Aosta", "")
                                       , "Liguria"=c("Imperia", "")
                                       , "Lombardia"=c("Cremona", "")
                                       , "Trentino Alto Adige"=c("Bolzano", "Trento")
                                       , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
                                       , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
                                       , "Umbria"=c("Perugia", "Terni")
                                       , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
                                       , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
                                       , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
                                       , "Molise"=c("Campobasso", "Isernia")
                                       , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
                                       , "Basilicata"=c("Matera", "Potenza")
                                       , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
                                       , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
                                       , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
                                     )
                                     )
                       
                       , sliderInput(inputId="LagDaysCV"
                                     , label="Lag Days:"
                                     , min = 5
                                     , max = 15
                                     , value = optimal_J
                       )
                       
                       # , sliderInput(inputId="CutDayCV"
                       #               , label="Day of Prediction:"
                       #               , min = as.Date("2020-03-10")
                       #               , max = last_day
                       #               , value = last_day-5
                       #               , timeFormat="%d %B"
                       # )
                       , h5(class = "text-muted"
                            , br()
                            , uiOutput(outputId = "textSIRD1")
                            , style = "color : black;"
                       )
                      
                   )
                  
            )
          )
          #---
          ,fluidRow(
            box(plotlyOutput("SIRDts")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            )
            , column(width = 3,
                     box(width = NULL, height = 425
                         , status = "info"
                         #, background = "aqua"
                         
                         , h5(class = "text-muted"
                              , br()
                              , uiOutput(outputId = "textSIRD2")
                              , style = "color : black;"
                         )
                         
                         , downloadButton("readSIRD", "Read more")
                         , downloadButton("ProvListSIRD", "Province list")
                         
                     )
            )
          )
          #---
          ,fluidRow(
            box(leafletOutput("SIRDMap")
                , width = 9, height = 425
                , status = "info"
                #, background = "aqua"
                , collapsible = F, solidHeader = T
            ),
            
            column(width = 3,
                   box(width = NULL, height = 425
                       , status = "info"
                       #, background = "aqua"
                       , selectInput(inputId="varMapSIRD"
                                     , label="Choose a variable to display:"
                                     , choices=c("Basic reproduction number"
                                                 ,"Transmission rate"
                                                 ,"Recovery rate"
                                                 ,"Mortality rate")
                                     , selected="Basic reproduction number")
                       
                       , sliderInput(inputId="selectDateSIRD"
                                     , label="Date:"
                                     , min = rangeDate[1]
                                     , max = max(SIRDParam_Dataset$date, na.rm = T)
                                     , value = max(SIRDParam_Dataset$date, na.rm = T)  #as.Date("2020-05-02") #
                                     , timeFormat="%Y-%m-%d")
                       , h5(class = "text-muted"
                            , br()
                            , uiOutput(outputId = "textSIRD3")
                            , style = "color : black;"
                       )
                   )
                   
            )
          )
          
  )
  
  
)
)
