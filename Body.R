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
              
              valueBoxOutput("total_box",width =3 )
              ,valueBoxOutput("active_box",width =3)
              ,valueBoxOutput("Recovered_box",width =3)
              ,valueBoxOutput("deceased_box",width =3 )
              
              ,tabBox( title = "Covid-19 Parameter",width = 6,height = "700px",
                      tabPanel("Cumulative Case",withSpinner(leafletOutput("provinceMap",height = "690px"))),
                      tabPanel("Cumulative Rate",withSpinner(leafletOutput("provinceMap1",height = "690px")))
              )
              
              ,box(withSpinner(plotlyOutput("N_TS"))
                   , width = 6
                   , height = "425px"
                   # , status = "warning"
                   , collapsible = F
                   , solidHeader = F
              )
              
              # ,column(width = 6,
                ,box(width = 6
                   , height = "310px"
                   , status = "info"
                   # , background = "light-blue"
                   , h5(class = "text-muted"
                       , br()
                       , uiOutput(outputId = "textDrates")
                       , style = "color : black;"
                  )
                  , downloadButton("ProvList1", "Data Scource")
                  , downloadButton("PLOT_2", "Read More")
              )
            )
            
            ,fluidRow(

              box(withSpinner(plotlyOutput("Drates_TS"))
                  , width = 12
                  , height = "450px"
                  # , status = "warning"
                  , collapsible = F
                  , solidHeader = F
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
                )
           )
  ),
  
    # "Time Series"----
    
  tabItem(tabName = "TimeSeries",
          fluidRow(
              tabBox( title = "Time Series Data",width = 9,height = "425px",
                    tabPanel("Cumulative Case",withSpinner(plotlyOutput("P_TS"))),
                    tabPanel("Cumulative Rate",withSpinner(plotlyOutput("P_TS2")))
               )
          
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
        )
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
              )
             )
          
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
                     
              )
              
            )
            
    ),
    
    # "Models"----
    tabItem(tabName = "SIRDModels"
            
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
                           
                           , sliderInput(inputId="LagDaysCV"
                                         , label="Lag Days:"
                                         , min = 5
                                         , max = 15
                                         , value = optimal_J
                           )
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
            
            ,fluidRow(
              column(width = 12,
                      box(width = NULL, height = 135
                          # , title = "The below plots displays the current parameters values across provinces of Italy for the date selected"
                          , status = "info"
                          , h5(class = "text-muted"
                               , br()
                               , uiOutput(outputId = "textSIRD3")
                               , style = "color : black;"
                          )
                      )
              )
              
              # ,column(width =3,
              #        box(width = NULL,height = 135,title = "Selected Date for the Plot:", status = "info"
              #            # ,verbatimTextOutput("s_d")
              #            ))
            )
            
            ,fluidRow(
              tabBox( width = 6,height = "700px"
                      ,tabPanel("Transmission Rate",withSpinner(leafletOutput("SIRDMap",height = "690px")))
                      ,tabPanel("Recovery Rate",withSpinner(leafletOutput("SIRDMap2",height = "690px")))
                )
              
              ,tabBox( width = 6,height = "700px"
                      ,tabPanel("Mortality Rate",withSpinner(leafletOutput("SIRDMap3",height = "690px")))
                      ,tabPanel("Basic Reproduction Number",withSpinner(leafletOutput("SIRDMap4",height = "690px")))
              )
     
         )
    
    )
  )
)