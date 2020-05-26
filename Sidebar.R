sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Province Map", tabName = "ProvinceMap", icon = icon("map"))
  
  ,menuItem("Time Series", tabName = "TimeSeries", icon = icon("chart-line")
           #, badgeLabel = "new", badgeColor = "green"
           )
  
  ,menuItem("SIRD Models", tabName = "SIRDModels", icon = icon("binoculars"))
  
))


