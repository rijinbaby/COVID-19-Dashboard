sidebar <- dashboardSidebar(
            width = "270px", sidebarMenu(style = "position: fixed; overflow: visible;",

# prov map ----------------------------------------------------------------

              menuItem("Province Map", tabName = "ProvinceMap", icon = icon("map")#, startExpanded = TRUE
                       
                )

# Time Series --------------------------------------------------------------

              ,menuItem("Time Series", tabName = "TimeSeries", icon = icon("chart-line")
              #, badgeLabel = "new", badgeColor = "green" )
              )

# SIRD --------------------------------------------------------------------

             ,menuItem("SIRD Models", tabName = "SIRDModels", icon = icon("binoculars"))

# about -------------------------------------------------------------------

             ,menuItem("About", tabName = "About", icon = icon("user"))

# sidebar input -----------------------------------------------------------

,sliderInput(inputId="selectDate1"
             , label="Select Mapping Date:"
             , min = rangeDate[1]
             , max = rangeDate[2]
             , value = rangeDate[2]
             #, timeFormat="%Y-%m-%d"
             , timeFormat="%d %b"
             ,width = "260px"
)

, selectInput(inputId='inReg'
              , label='Region:'
              , selected=FALSE
              , choices=list(
                "Select a Region"=c("Abruzzo","Basilicata","Calabria","Campania","Emilia-Romagna","Friuli Venezia Giulia","Lazio","Liguria","Lombardia","Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana","Trentino Alto Adige","Umbria","Valle d'Aosta","Veneto")
              )
              , multiple=FALSE
              , selectize=FALSE
              ,width = "260px")
# "Abruzzo","Basilicata","Calabria","Campania","Emilia-Romagna","Friuli Venezia Giulia","Lazio","Liguria","Lombardia","Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana","Trentino Alto Adige","Umbria","Valle d'Aosta","Veneto"


, selectInput(inputId='inProv'
              , label='Province:'
              # , selected=c("ALL")
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
              , selectize=TRUE
              ,width = "260px"
            )
# ,dashboardFooter(left_text ="**If the Provice filter is selected then the Region filter is irresponsive. To work with Region filter clear the Province values selecter"
#                  , width = "100%", height = "160px", italic = TRUE, bold = TRUE,
#                  style = "text-align:center; align: center; padding: 0px; margin: 0px;")
      
    ) # sidebarmenu
          
)  # dashboardSidebar








# ,selectInput(inputId='inReg'
#              , label='Regions:'
#              #, selected="Lombardia"
#              , choices=sort(unique(pcmTOTData$denominazione_regione))
#              , multiple=FALSE
#              , selectize=TRUE
#              ,width = "260px"
# )



#  ,selectInput(inputId="Province"
#                     , label="Choose Mortality Parameter"
#                     , choices=c("Cumulative Case"
#                                 , "Cumulative Rate")
#                     , selected="Cumulative Case"),
# 
