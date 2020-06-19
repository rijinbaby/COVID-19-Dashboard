
ProvinceMap <- function(database, var) {
  options(warn=-1)
  
  # geojson file for Italy
  geo_italy <- geojsonio::geojson_read("~/Covid-19-Dashboard/data/geo_italy.geojson", what = "sp")
  
  #
  pal <- leaflet::colorNumeric(c("blue","yellow","red"), NULL)  # viridis   YlOrRd  RdYlBu
  
  # merge geojson with our data
  db_geo <- sp::merge(geo_italy,database,by.x ="prov_name",by.y="denominazione_provincia")
  
  labels <- sprintf(
    "<strong>%s</strong><br/> Cumulative Case: %g <br/> Cumulative Rate: %g",
    db_geo$prov_name, db_geo$totale_casi.x, db_geo$prevIndex
  ) %>% lapply(htmltools::HTML)
  
  # options = leafletOptions(zoomControl = FALSE)
  if(var=="prevIndex")
  {
    provMap <- leaflet(db_geo) %>%
      # setView(lat=41.8719, lng=12.5674,zoom=6 ) %>%
      # fitBounds(lng1 = 15,lat1 =43 ,lng2 = 9,lat2 =36 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(prevIndex),
                  label = labels) %>%
      addLegend(pal = pal, values = ~prevIndex, opacity = 0.7, title = "Cumulative Rate",position = "topright")
  }
  else if(var=="totale_casi.x")
  {
    provMap <- leaflet(db_geo) %>%
      # setView(lat=41.8719, lng=12.5674,zoom=6) %>%
      # fitBounds(lng1 = 9,lat1 =36 ,lng2 = 15,lat2 =43 ) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(totale_casi.x),
                  label = labels) %>%
      # ~paste0("Cumulative Case", ": ", formatC(totale_casi.x, big.mark = ","))
      addLegend(pal = pal, values = ~totale_casi.x, opacity = 0.7, title = "Cumulative Case",position = "topright")
      
    # provMap <- clearBounds(provMap)
  }

    
  #Map plot----
  return(provMap)
    
}

