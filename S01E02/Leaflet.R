library(leaflet)
library(rgdal)
library(sf)
#http://rstudio.github.io/leaflet/

#Comas <-readOGR("S01E02/shp", "Comas")
# Préférer sf à rgdal qui crée undata frame enrichi d'une colonne géométrie
Comas <- st_read("S01E02/shp/Comas.shp")



#Comas <- readOGR("S01E02/geojson/comas.GeoJSON") # tres tres long

pal <- colorNumeric(palette = "Reds",domain = Comas$Rsdncpr)

leaflet(Comas) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addLayersControl(baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Esri")) %>%
  addPolygons(fillOpacity = 0.8,color = ~pal(Rsdncpr), weight = 2, 
              popup = ~sprintf("<b>%s</b><br/>
              Individus : %s<br/>
              Logements : %s<br/>
              Individus des résidences principales : %s<br/>
              Résidences principales : %s",Comas,Indivds,Logmnts, Indvdrp, Rsdncpr)) %>%
   addLegend("bottomright", pal = pal, values = ~Rsdncpr,title = "Res. Pr",opacity = 1)



X <- Comas@data
X$indice <- as.numeric(row.names(X))
X$test <- X$indice/X$IDComas
Comas@data <- X