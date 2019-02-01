library(tidyverse)
library(readxl)
library(sf)
library(GISTools)
library(maptools)
library(reshape2)
library(ggmap)
library(leaflet)

# 01 #####################################################################################
# Problemas con los sistemas de referencia
## Lo veremos con más detalle, por ahora cargamos los datos y los filtramos para 
## quedarnos solo con los homicidios
crimes_2018 <- read_delim("Crimes_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
homicidios <- crimes_2018 %>% 
  filter(`Primary Type` == "HOMICIDE", Arrest == "true")
mapa_district <- st_read("PoliceDistrict.shp")
## Visualizamos los resultados
ggplot() + geom_sf(data = mapa_district) + 
  geom_point(data=homicidios, color = "dodgerblue4", size =2, 
             aes(x = Longitude, y = Latitude)) +
  labs(subtitle = "Homicidios en Chicago - Año 2018") + 
  theme_light(base_size = 8) +
  theme(plot.subtitle = element_text(size=10, face = "italic")) +
  theme(legend.position = "none")

# 02 #####################################################################################
# Mapas de los SECTORES policiales de Chicago
mapa_beats <- st_read("Police_Beats.geojson") # Leer datos en formato geojson
ggplot() + geom_sf(data = mapa_beats, color = "Blue") 

# 03 #####################################################################################
# Exploramos el dataset
summary(mapa_beats)

# 04 #####################################################################################
# Mapas de los DISTRITOS policiales de Chicago
mapa_district <- st_read("PoliceDistrict.shp") # Leer datos en formato shp (ojo con el CRS)
ggplot() + geom_sf(data = mapa_district, color = "Red2") # Grafico el mapa

# 05 #####################################################################################
# Datos de delitos en Chicago 2018
crimes_2018 <- read_delim("Crimes_2018.csv", ";", escape_double = FALSE, trim_ws = TRUE)
head(crimes_2018)

# 06 #####################################################################################
## Convertir csv a shp
# Primero lo convertimos en objeto espacial
homicidios_posicion <- st_as_sf(homicidios, coords = c("Longitude", "Latitude"), crs = 4326)
# Después lo guardo en un fichero
st_write(homicidios_posicion, "homicidios_posicion.shp", layer_options = "GEOMETRY=AS_XY")

# 07 #####################################################################################
############ Codificación y mapeo con cartociodad ############

# Instalación de Cartociudad
## library(devtools)
## install_github("rOpenSpain/caRtociudad", force = TRUE)

library(caRtociudad)
calle <- "Avenida de la universidad 1, 03202 elche"
direccion <- cartociudad_geocode(calle)

# Construcción del mapa base
## Primero almacenamos las coordenadas del centro de la ciudad
elche_lat <- 38.26464
elche_lon <- -0.6951413

# Segundo fijamos el área del mapa base 
## Indicamos el centro del mapa (c(lat,lng)) y el nivel de zoom (5)
mapaelche <- cartociudad_get_map(c(elche_lat, elche_lon), 5) 

# 08 #####################################################################################
# Tercero representamos los puntos en el mapa base
ggmap(mapaelche) +
  geom_point(data = direccion, color = "Red2", size =1, 
             aes(x = lng, y = lat))

# 09 #####################################################################################
# Filtramos para ver solo los homicidios
homicidios <- crimes_2018 %>% 
  filter(`Primary Type` == "HOMICIDE", Arrest == "true")

# Sectores policiales de Chicago. Procede de un geojson
ggplot() + geom_sf(data = mapa_beats) + 
  geom_point(data=homicidios, color = "dodgerblue4", size =1, 
             aes(x = Longitude, y = Latitude))

# 10 #####################################################################################
# Distritos policiales de Chicago. Procede de un shp
ggplot() + geom_sf(data = mapa_district) + 
  geom_point(data=homicidios, color = "dodgerblue4", size =1, 
             aes(x = Longitude, y = Latitude))

# 11 #####################################################################################
# Mapa de los delitos de Chicago 2018. Distribución puntual
ggplot() + geom_point(data=crimes_2018, aes(x = Longitude, y = Latitude))

# 12 #####################################################################################
# Mapa de los HOMICIDIOS de Chicago 2018. Distribución puntual
homicidios_plano <- st_read("homicidios_posicion.shp")
ggplot() + geom_sf(data=mapa_beats) + geom_sf(data = homicidios_plano) 

# 13 #####################################################################################
## Visualización de datos
# La librería leaflet
v_homicidios <- leaflet() %>% 
  addTiles() %>% 
  setView(-87.6231177, 41.881832, zoom=12) %>% 
  addMarkers(crimes_2018$Longitude, crimes_2018$Latitude,
             clusterOptions = markerClusterOptions())
v_homicidios

# 13 #####################################################################################
## Analizando los datos
ggplot() + geom_sf(data = mapa_beats) + stat_density2d(aes(x=Longitude, y=Latitude, fill = ..level..,
                                                                   alpha = ..level..), size = 2, bins = 5, data = homicidios, geom = "polygon"
) + 
  labs(subtitle = "KDE de los homicidios en Chicago") + 
  theme_light(base_size = 8) +
  theme(plot.subtitle = element_text(size=12, face = "italic")) +
  theme(legend.position = "none")

# 14 #####################################################################################
## Analizando y visualizando los datos
homicidios$Date <- lubridate::mdy_hms(homicidios$Date)
homicidios$diasem <- lubridate::wday(homicidios$Date, week_start = 1)
ggplot() + geom_sf(data = mapa_beats) + stat_density2d(aes(x=Longitude, y=Latitude, fill = ..level..,
                                                           alpha = ..level..), size = 2, bins = 5, data = homicidios, geom = "polygon"
) + 
  facet_grid(~ diasem) +
  labs(subtitle = "KDE de los homicidios en Chicago por día de la semana") + 
  theme_light(base_size = 8) +
  theme(plot.subtitle = element_text(size=12, face = "italic")) +
  theme(legend.position = "none")

# 15 #####################################################################################
## Agregando puntos a polígonos

beats_delitos <- st_read("delitos_beats.geojson")
ggplot() + geom_sf(data = beats_delitos, aes(fill = NUMPOINTS), color = NA)



