body <- dashboardBody(
  

# "html tag" --------------------------------------------------------------

  tags$head(tags$style(HTML('
                          /* body */
                              
                              /* header */
                            .main-header .logo {
                             font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;font-size: 24px;
                              position: fixed;
                               }

                            /* sidebar */
                          .skin-blue .main-sidebar {
                            background-color: black;}

                            '
                     ))
            ),
  
# .content-wrapper, .right-side {
#   background-color: white;
# }
  
  # .box {margin: 2px;
  #   padding:2px;}
  
  # body {color:black;}
  
  # .content {
  #   padding-top: 60px;
  # }
  
  
  # .leaflet-container {
  #   background: #FFF;
  # }
  
  
  tabItems(
  # "Provinces map - HOME" ----------------------------------------------------
  
    tabItem(tabName = "ProvinceMap",
            
            # tags$h2("Province Map"),
            fluidRow(
              
              # column(width = 3,"Italy Overview"),
              # infoBox("Italy Overview",color = "light-blue"),
              valueBoxOutput("total_box",width =3 )
              ,valueBoxOutput("active_box",width =3)
              ,valueBoxOutput("Recovered_box",width =3)
              ,valueBoxOutput("deceased_box",width =3 )
              
              ,tabBox( title = "Covid-19 Parameter",width = 6,height = "700px",
                      tabPanel("Cumulative Case",withSpinner(leafletOutput("provinceMap",height = "690px"))),
                      tabPanel("Cumulative Rate",withSpinner(leafletOutput("provinceMap1",height = "690px")))
              )
              
              # ,column(width = 6
              # ,box(width = NULL, height = 100
              #      ,sliderInput(inputId="selectDate1"
              #              , label="Select Mapping Date:"
              #              , min = rangeDate[1]
              #              , max = rangeDate[2]
              #              , value = rangeDate[2]
              #              #, timeFormat="%Y-%m-%d"
              #              , timeFormat="%d %b"
              #              # ,width = 6
              # )))
             
              ,box(withSpinner(plotlyOutput("N_TS"))
                   , width = 6
                   , height = "425px"
                   # , status = "warning"
                   , collapsible = F
                   , solidHeader = F
                   # ,style='padding-right:1px;padding-left:1px;'
                   # ,title="XXXX"
              )
              
              # ,column(width = 6,
                ,box(width = 6
                   , height = "300px"
                   , status = "info"
                   # , background = "light-blue"
                   , h5(class = "text-muted"
                       , br()
                       #, uiOutput(outputId = "textPres")
                       , uiOutput(outputId = "textDrates")
                       , style = "color : black;"
                  )
                  , downloadButton("ProvList1", "Data Scource")
                  , downloadButton("PLOT_2", "Read More")
              )
              # )
              
            )
              
              # box(leafletOutput("provinceMap")
              #     , width = 9, height = 440
              #     , status = "info"
              #     #, background = "aqua"
              #     , collapsible = F, solidHeader = F
              # ),
              
              # column(width = 3,
              #        box(width = NULL, height = 175
              #            , status = "info"
              #            #, background = "aqua"
              #            , selectInput(inputId="Province"
              #                          , label="Choose a variable to display"
              #                          , choices=c("Province cumulative cases"
              #                                      , "Province cumulative rates")
              #                          , selected="Province cumulative cases")
              #            
              #            , sliderInput(inputId="selectDate1"
              #                          , label="End date:"
              #                          , min = rangeDate[1]
              #                          , max = rangeDate[2]
              #                          , value = rangeDate[2]
              #                          , timeFormat="%Y-%m-%d")
              #            
              #        ),
              #        
              #        box(width = NULL, height = 245
              #            , status = "info"
              #            #, background = "aqua"
              #            , h5(class = "text-muted"
              #                 , br()
              #                 , uiOutput(outputId = "textPres")
              #                 , style = "color : black;"
              #            )
              #        ))
            
            
            ,fluidRow(

              box(withSpinner(plotlyOutput("Drates_TS"))
                  , width = 12
                  , height = "450px"
                  # , status = "warning"
                  , collapsible = F
                  , solidHeader = F
                  # ,style='padding-right:1px;padding-left:1px;'
                  # ,title="XXXX"
                )


            )
    ),
        
    
  # "About" -------------------------------------------------------------------
  
  
  tabItem(tabName = "About",
          tags$h2("COVID-Pro: A province-based analysis for Italy"),
          fluidRow(
            box(width = NULL#, height = 245
                , status = "info"
                #, background = "aqua"
                , h5(class = "text-muted"
                     , br()
                     , uiOutput(outputId = "textPres")
                     # , uiOutput(outputId = "textDrates")
                     , style = "color : black;"
                )
                # , downloadButton("ProvList1", "Province list")
            )
          )
  ),
  
    # "Time Series"----
    
  tabItem(tabName = "TimeSeries",
            # tags$h2("Time Series")
            
          fluidRow(
            
          tabBox( title = "Time Series Data",width = 9,height = "425px",
                   tabPanel("Cumulative Case",withSpinner(plotlyOutput("P_TS"))),
                   tabPanel("Cumulative Rate",withSpinner(plotlyOutput("P_TS2")))
          )
                   
            
              # box(plotlyOutput("P_TS")
              #     , width = 9, height = 425
              #     , status = "info"
              #     #, background = "aqua"
              #     , collapsible = F, solidHeader = T
              # ),
              
              # ,column(width = 3,
              #        box(width = NULL, height = 202
              #            , status = "info"
              #            #, background = "aqua"
              #            , selectInput(inputId='inProv'
              #                          , label='Province:'
              #                          , selected=c("Torino","Cremona", "Piacenza", "Bergamo", "Milano")
              #                          , choices=list(
              #                            "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
              #                            , "Valle d'Aosta"=c("Aosta", "")
              #                            , "Liguria"=c("Genova", "Imperia", "La Spezia", "Savona")
              #                            , "Lombardia"=c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese")
              #                            , "Trentino Alto Adige"=c("Bolzano", "Trento")
              #                            , "Veneto"=c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
              #                            , "Friuli Venezia Giulia"=c("Gorizia", "Pordenone", "Trieste", "Udine")
              #                            , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
              #                            , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
              #                            , "Umbria"=c("Perugia", "Terni")
              #                            , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
              #                            , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
              #                            , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
              #                            , "Molise"=c("Campobasso", "Isernia")
              #                            , "Campania"=c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno")
              #                            , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
              #                            , "Basilicata"=c("Matera", "Potenza")
              #                            , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
              #                            , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
              #                            , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
              #                          )
              #                          , multiple=TRUE
              #                          , selectize=TRUE)
              #            , selectInput(inputId='inVarP'
              #                          , label='Variable:'
              #                          , selected=c("Cumulative cases")
              #                          , choices=c("Cumulative cases", "Cumulative rates")
              #                          , multiple=FALSE
              #                          , selectize=TRUE)
                     # )
                     
                     , box(width = 3, height = "460px"
                           , status = "info"
                           #, background = "aqua"
                           , h5(class = "text-muted"
                                , br()
                                , uiOutput(outputId = "textPLOT1")
                                , style = "color : black;"
                           )
                           , downloadButton("ProvList1_2", "Data Scource")
                           , downloadButton("PLOT1", "Read more")
                     )
            
              # )
            )
            
            # ,fluidRow(
            #   box(plotlyOutput("R_TS1")
            #       , width = 9, height = 425
            #       , status = "info"
            #       #, background = "aqua"
            #       , collapsible = F, solidHeader = T
            #   )
              
              # tabBox(
              #   width = 9, height = 425,
              #   # The id lets us use input$tabset1 on the server to find the current tab
              #   tabPanel("Province values", plotlyOutput("R_TS1"))
              #   ,tabPanel("Region Values", 'plotlyOutput("R_TS2")')
              # ),
              
              # column(width = 3,
                      # box(width = NULL, height = 202
                      #     , status = "info"
                      #     #, background = "aqua"
                      #     , selectInput(inputId='inReg'
                      #                   , label='Regions:'
                      #                   , selected="Lombardia"
                      #                   , choices=sort(unique(pcmTOTData$denominazione_regione))
                      #                   , multiple=FALSE
                      #                   , selectize=TRUE)
                      #     , selectInput(inputId='inVarR'
                      #                   , label='Variable:'
                      #                   , selected=c("Cumulative cases")
                      #                   , choices=c("Cumulative cases", "Cumulative rates")
                      #                   , multiple=FALSE
                      #                   , selectize=TRUE)
                      # ),
                      
            #           box(width = NULL, height = 202
            #                 , status = "info"
            #                 #, background = "aqua"
            #                 , h5(class = "text-muted"
            #                      , br()
            #                      , uiOutput(outputId = "textPLOT2")
            #                      , style = "color : black;"
            #                 )
            #                 , downloadButton("PLOT2", "Read more")
            #           )
            #   )
            # )
            
            ,fluidRow(
              column(width = 12,
              tabBox( width = 6,height = "460px", #title = "P1",
                      tabPanel("Cumulative Case",withSpinner(plotlyOutput("R_TS2"))),
                      tabPanel("Cumulative Rate",withSpinner(plotlyOutput("R_TS2_2")))
              )
              
              ,tabBox( width = 6,height = "460px",#title = "P2",
                      tabPanel("Daily Case",withSpinner(plotlyOutput("R_TS3"))),
                      tabPanel("Daily Rate",withSpinner(plotlyOutput("R_TS3_2")))
              )
              # ,box(width=1,height = "450px")
              )
              # box(plotlyOutput("R_TS2")
              #     , width = 6, height = 425
              #     , status = "info"
              #     #, background = "aqua"
              #     , collapsible = F, solidHeader = T
              # )
              # 
              # ,box(plotlyOutput("R_TS3"),
              #      width = 6, height = 425
              #      , status = "info"
              #      #, background = "aqua"
              #      , collapsible = F, solidHeader = T
              # )
              
            )
          
          # ,fluidRow(
          #   column(width = 12,
          #          box(height=15))
          # )
          
            ,fluidRow(
              column(width = 12
                     
                     , box(width = 3, height = 425
                           , status = "info"
                           , h5(class = "text-muted"
                                , br()
                                , uiOutput(outputId = "textPLOT3")
                                , style = "color : black;"
                           )
                           , downloadButton("PLOT3", "ISTAT Information")
                           , downloadButton("ProvList", "COVID-19 Source")
                     )
                     
              ,box(withSpinner(plotlyOutput("D_TS"))
                  , width = 9, height = 425, collapsible = F, solidHeader = T
                  # , status = "info"
              ),
              
              # column(width = 3
                     # ,box(width = NULL, height = 152
                     #     # , status = "info"
                     #     #, background = "aqua"
                     #     , selectInput(inputId='inProvD'
                     #                   , label='Select One Province:'
                     #                   , selected="Torino"
                     #                   , choices=list(
                     #                     "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
                     #                     , "Valle d'Aosta"=c("Aosta", "")
                     #                     , "Liguria"=c("Genova", "Imperia", "La Spezia", "Savona")
                     #                     , "Lombardia"=c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese")
                     #                     , "Trentino Alto Adige"=c("Bolzano", "Trento")
                     #                     , "Veneto"=c("Belluno", "Padova", "Rovigo", "Treviso", "Venezia", "Verona", "Vicenza")
                     #                     , "Friuli Venezia Giulia"=c("Gorizia", "Pordenone", "Trieste", "Udine")
                     #                     , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
                     #                     , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
                     #                     , "Umbria"=c("Perugia", "Terni")
                     #                     , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
                     #                     , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
                     #                     , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
                     #                     , "Molise"=c("Campobasso", "Isernia")
                     #                     , "Campania"=c("Avellino", "Benevento", "Caserta", "Napoli", "Salerno")
                     #                     , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
                     #                     , "Basilicata"=c("Matera", "Potenza")
                     #                     , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
                     #                     , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
                     #                     , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
                     #                   )
                     #                   , multiple=FALSE
                     #                   , selectize=TRUE)
                     # )
                     
              )
              
            )
            
    ),
    
    # "Models"----
    tabItem(tabName = "SIRDModels"
            # ,tags$h2("SIRD Models")
            
            ,fluidRow(
              box(withSpinner(plotlyOutput("SIRDp"))
                  , width = 9, height = 425
                  # , status = "info"
                  #, background = "aqua"
                  , collapsible = F, solidHeader = T
              )
              
              
              
              , column(width = 3,
                       box(width = NULL, height = 425
                           , status = "info"
                           #, background = "aqua"
                           
                           # , selectInput(inputId="SIRprovCV"
                           #               , label='Province:'
                           #               , selected="Torino"
                           #               , choices=list(
                           #                 "Piemonte"=c("Alessandria", "Asti", "Biella", "Cuneo", "Novara", "Torino", "Verbano-Cusio-Ossola", "Vercelli")
                           #                 , "Valle d'Aosta"=c("Aosta", "")
                           #                 , "Liguria"=c("Imperia", "")
                           #                 , "Lombardia"=c("Cremona", "")
                           #                 , "Trentino Alto Adige"=c("Bolzano", "Trento")
                           #                 , "Emilia Romagna"=c("Bologna", "Ferrara", "Forli-Cesena", "Modena", "Parma", "Piacenza", "Ravenna", "Reggio nell'Emilia", "Rimini")
                           #                 , "Toscana"=c("Arezzo", "Firenze", "Grosseto", "Livorno", "Lucca", "Massa Carrara", "Pisa", "Pistoia", "Prato", "Siena")
                           #                 , "Umbria"=c("Perugia", "Terni")
                           #                 , "Marche"=c("Ancona", "Ascoli Piceno", "Fermo", "Macerata", "Pesaro e Urbino")
                           #                 , "Lazio"=c("Frosinone", "Latina", "Rieti", "Roma", "Viterbo")
                           #                 , "Abruzzo"=c("Chieti", "L'Aquila", "Pescara", "Teramo" )
                           #                 , "Molise"=c("Campobasso", "Isernia")
                           #                 , "Puglia"=c("Bari", "Barletta-Andria-Trani", "Brindisi", "Foggia", "Lecce", "Taranto")
                           #                 , "Basilicata"=c("Matera", "Potenza")
                           #                 , "Calabria"=c("Catanzaro", "Cosenza", "Crotone", "Reggio di Calabria", "Vibo Valentia")
                           #                 , "Sicilia"=c("Agrigento", "Caltanissetta", "Catania", "Enna", "Messina", "Palermo", "Ragusa", "Siracusa", "Trapani")
                           #                 , "Sardegna"=c("Cagliari", "Nuoro", "Oristano", "Sassari", "Sud Sardegna")
                           #               )
                           # )
                           
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
              box(withSpinner(plotlyOutput("SIRDts"))
                  , width = 9, height = 425
                  # , status = "info"
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
            #---  tabPanel("Cumulative Case",withSpinner(leafletOutput("provinceMap",height = "690px"))),
            ,fluidRow( 
              tabBox( width = 9,height = "470px"
                             ,tabPanel("Transmission Rate",withSpinner(leafletOutput("SIRDMap",height = "460px")))
                             ,tabPanel("Recovery Rate",withSpinner(leafletOutput("SIRDMap2",height = "460px")))
                             ,tabPanel("Mortality Rate",withSpinner(leafletOutput("SIRDMap3",height = "460px")))
                             ,tabPanel("Basic Reproduction Number",withSpinner(leafletOutput("SIRDMap4",height = "460px")))
                     )
              
              # box(leafletOutput("SIRDMap")
              #     , width = 9, height = 425
              #     # , status = "info"
              #     #, background = "aqua"
              #     , collapsible = F, solidHeader = T
              # ),
              
              ,column(width =3,
                      box(width = NULL,height = 125,title = "Selected Date:", status = "info",
                      verbatimTextOutput("s_d")))
              ,column(width = 3,
                     box(width = NULL, height = 360
                         , status = "info"
                         #, background = "aqua"
                         # , selectInput(inputId="varMapSIRD"
                         #               , label="Choose a variable to display:"
                         #               , choices=c("Basic reproduction number"
                         #                           ,"Transmission rate"
                         #                           ,"Recovery rate"
                         #                           ,"Mortality rate")
                         #               , selected="Basic reproduction number")
                         
                         # , sliderInput(inputId="selectDateSIRD"
                         #               , label="Date:"
                         #               , min = rangeDate[1]
                         #               , max = max(SIRDParam_Dataset$date, na.rm = T)
                         #               , value = max(SIRDParam_Dataset$date, na.rm = T)  #as.Date("2020-05-02") #
                         #               , timeFormat="%Y-%m-%d")
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