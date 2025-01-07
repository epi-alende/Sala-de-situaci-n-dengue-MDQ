###

# Este script permite crear el mapa animado de evolución histórica de casos y guardarlo como gif.
# También permite construir el mapa interactivo en leaflet. 

###
library(tidyverse)
#library(ggmap)
library(ggplot2)
#library(tmap)
library(sf)
library(scales)
library(sp)
library(readxl)
library(gganimate)
library(gifski)
library(av)
library(hrbrthemes)
library(leaflet)

#Antes de comenzar a trabajar establezco el working directory donde se va a guardar el gif
compu <- "D:/INFO USUARIO NO BORRAR/Downloads/Sala de situación dengue Rosario"


# Una vez que contamos con toda la información geográfica de los casos podemos leer la base y el archivo con las areas barriales
base <- read_xlsx("baseSS.xlsx")
areas_barriales <- read_excel("areas barriales.xlsx")
# Uno los df para incluir la variable ID_AREA_BA en la base
base <- base %>%
  left_join(areas_barriales, by = c("distrito", "barrio", "vecinal"))
#Leo los archivos shp de distritos y barrios. Ambos shp usan coordenadas planas Projected CRS: POSGAR 94 / Argentina 5
distritos <- st_read("distritos_descentralizados.shp") # Para el mapa animado se usan estas coordenadas
barrios <- st_read("areas_barriales_20180611_polygons.shp")
#Creo data frame con coordenadas de puntos de casos de dengue
datos <- base %>% select(puntoX, puntoY, distrito, Clasificacion, `Fecha consulta`) %>%
  rename(longitud = "puntoX", latitud = "puntoY") %>% na.omit()
#Creo shp a partir del dataframe datos. 
puntos_sf <- st_as_sf(datos, coords = c("longitud", "latitud"), crs = 22185) # CRS 22185 corresponde a POSGAR 94 / Argentina 5
st_write(puntos_sf, "dengue_puntos.shp")
#Leo el shp de dengue_puntos
datos <- st_read("dengue_puntos.shp")
#Se pudieron geolocalizar 10824 puntos de 10869 eventos totales. 

## MAPA ANIMADO EVOLUCION HISTORICA
mapa_evol <- ggplot() +
  geom_sf(data = distritos, fill = "white", aes(geometry = geometry)) +
  geom_sf(data = datos, aes(geometry = geometry, color = Clsfccn, ), size = 1.5) +
  geom_sf(data = distritos, fill = "white", alpha = 0.01, aes(geometry = geometry)) +
  labs(title = "Evolución histórica de casos brote dengue 2023-2024, Rosario", x = "Longitud", y = "Latitud",
       color = "Clasificación") +
  scale_color_manual(values = c("Confirmado" = "red", "Descartado" = "green", "Probable" = "blue", "Sospechoso" = "yellow")) +
  theme_ipsum() 
# Le doy animación al gráfico con gganimate
anim <- mapa_evol +labs(title = 'Fecha: {current_frame}')+ gganimate::transition_manual(datos$Fchcnsl, cumulative = T) 
# Renderizo y guardo el gif en el wd
anim_save(filename = "mapa_dengue_gif_312023a2024_.gif",animation = anim, fps = 5)
anim

## MAPA INTERACTIVO CON LEAFLET
#Transformo los shp de distritos y barrios a coordenadas geográficas
Dist <- st_transform(distritos, crs = '+proj=longlat +datum=WGS84')
Barr <- st_transform(barrios, crs = '+proj=longlat +datum=WGS84')
st_crs(Dist)
st_crs(Barr)

# Realizo un conteo de los casos por distrito y clasificación 
dengue_distritos <- base %>% select(distrito, Clasificacion) %>%
  count(distrito, Clasificacion) %>% na.omit() %>%
  pivot_wider(names_from = Clasificacion, values_from = n)
# Realizo un conteo de los casos por distrito y Serotipo
dengue_serotipos <- base %>% select(distrito, Serotipo) %>%
  count(distrito, Serotipo) %>% na.omit() %>%
  pivot_wider(names_from = Serotipo, values_from = n) %>%
  replace_na(list(`DEN 1` = 0, `DEN 2` = 0, `DEN 3` = 0, `Sin serotipo` = 0))
# Calculo la incidencia acumulada por distrito
# Para calcular la incidencia acumulada necesito la población por distrito
dengue_incacumD <- base %>% filter(Clasificacion == "Confirmado") %>% count(distrito, Clasificacion)
Pobxdist <- data.frame(distrito = c("CENTRO", "NOROESTE", "NORTE", "OESTE", "SUDOESTE", "SUR"),
                    pob = c(260084, 182787, 146788, 142432, 123425, 155720))
dengue_incacumD <- merge(dengue_incacumD, Pobxdist, all.x = TRUE)
dengue_incacumD$n[is.na(dengue_incacumD$n)] <- 0
dengue_incacumD <- dengue_incacumD %>% mutate("Incidencia acumulada" = round(n/pob*100000, digits = 1)) %>%
  select(distrito,"Incidencia acumulada")
# Realizo un conteo de casos por area barrial y clasificacion
dengue_barrios <- base %>% select(ID_AREA_BA, Clasificacion) %>%
  count(ID_AREA_BA, Clasificacion) %>% na.omit() %>%
  pivot_wider(names_from = Clasificacion, values_from = n) %>%
  replace_na(list("Confirmado" = 0, "Sospechoso" = 0, "Probable" = 0, "Descartado" = 0))
# Transformo la variable Nombre a factor y las celdas en mayúsculas para poder hacer la unión con los df
Dist <- Dist %>% mutate(Nombre = factor(toupper(Nombre)))
# Uno el archivo shapefile con los dataframes construidos 
Dist <- Dist %>%
  full_join(dengue_distritos, by = c("Nombre" = "distrito"))
Dist <- Dist %>%
  full_join(dengue_serotipos, by = c("Nombre" = "distrito"))
Dist <- Dist %>%
  full_join(dengue_incacumD, by = c("Nombre" = "distrito"))
Barr <- Barr %>% mutate(ID_AREA_BA = factor(ID_AREA_BA))
Barr <- Barr %>%
  full_join(dengue_barrios, by = c("ID_AREA_BA"))

#Grafico el mapa interactivo con leaflet
mapa <- leaflet() %>%
  addTiles() %>%
  setView(lng = -60.7, lat = -32.95, zoom = 11.5) %>%
  addPolygons(data = Dist, color = "yellow", stroke = 1, opacity = 0.8, group = "Serotipos",
              popup = paste0(Dist$Nombre, "<hr>", "DEN 1 ", Dist$`DEN 1`, "<br>", "DEN 2 ", Dist$`DEN 2`, "<br>", "DEN 3 ", Dist$`DEN 3`, "<br>", "Serotipo no dtdo. ", Dist$`Sin serotipo`)) %>%
  addPolygons(data = Barr, color = "green", stroke = 1, opacity = 0.8, group = "Casos por Area Barrial",
              popup = paste0(Barr$ID_AREA_BA, "<hr>", "Confirmados ", Barr$Confirmado, "<br>", "Probables ", Barr$Probable, "<br>", "Sospechosos ", Barr$Sospechoso, "<br>", "Descartados ", Barr$Descartado)) %>%
  addPolygons(data = Dist, color = "blue", stroke = 1, opacity = 0.8, group = "Inc. Acumulada Distritos",
              popup = paste0(Dist$Nombre, "<hr>", "Incidencia acumulada ", "<br>", Dist$`Incidencia acumulada`, " x 100.000hab")) %>%
  addPolygons(data = Dist, color = "red", stroke = 1, opacity = 0.8, group = "Casos por Distrito",
              popup = paste0(Dist$Nombre, "<hr>", "Confirmados ", Dist$Confirmado, "<br>", "Probables ", Dist$Probable, "<br>", "Sospechosos ", Dist$Sospechoso, "<br>", "Descartados ", Dist$Descartado)) %>%
  addLayersControl(baseGroups = c("StreetMap", "Stamen"), overlayGroups = c("Casos por Distrito", "Inc. Acumulada Distritos", "Casos por Area Barrial", "Serotipos"),
                   position = "topright")
mapa
saveRDS(mapa, file = "mapa.Rds")

### FIN ###