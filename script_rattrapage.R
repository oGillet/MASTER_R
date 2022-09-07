library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

library(osrm)
library(osmdata)

## install 'webshot' package
# library(devtools)
# install_github("wch/webshot")

library(htmlwidgets)
library(webshot)

library(ggpubr)

# Function ----------------------------------------------------------------
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}


saveMap <- function(m, name){
  saveWidget(m, "temp.html", selfcontained = FALSE)
  webshot("temp.html", file = paste0('images/',name,'.png'), cliprect = "viewport") 
  cat("Map export to", paste0('images/',name,'.png\n'))
}

results <-  list()

# Location ----------------------------------------------------------------
longitude <- c(0.09282786290535061)
latitude <- c(49.49488634748982)
place <- c("Positon")

df <- data.frame("place"=place,
                 "latitude"=latitude,
                 "longitude"=longitude)

df_sf <- sf::st_as_sf(df, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)

map <- leaflet() %>%
  setView(lng = longitude[1], lat = latitude[1], zoom = 13) %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addMarkers(data = df_sf,
             icon = icons(iconUrl="https://cdn-icons-png.flaticon.com/512/4781/4781517.png",
                          iconWidth = 30, 
                          iconHeight = 30))

map
saveMap(map, 'location')
results[['location']] <- map

# Bus ---------------------------------------------------------------------
database <- osmdata::getbb("Le Havre") %>% 
  osmdata::opq() %>% 
  osmdata::add_osm_feature(key = "amenity", value = c("restaurant", "fastfood"))

restaurant <- osmdata::osmdata_sf(database)
restaurant <- restaurant$osm_points

map <- leaflet() %>%
  setView(lng = longitude[1], lat = latitude[1], zoom = 14) %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addCircles(data = restaurant,
             color = "blue",
             fill = "blue",
             opacity = 1, 
             group = "restaurant") %>% 
  addCircles(data = df_sf,
             color = "red",
             fill = "red",
             opacity = 1, 
             group = "Ma position") %>% 
  addLayersControl(
    overlayGroups = c("Ma position", "Station de bus"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegendCustom(colors = c("red","blue"), labels = c("Ma position", "restaurant"), sizes = c(20,20))

map
saveMap(map, 'restaurant')
results[['restaurant']] <- map

# Buffer ---------------------------------------------------------------------
df_sf_buffer_1000 <- st_buffer(df_sf %>% st_transform(2154),1000) %>% st_transform(4326)

map <- leaflet() %>%
  fitBounds(min(st_coordinates(df_sf_buffer_1000)[,1]),
            min(st_coordinates(df_sf_buffer_1000)[,2]),
            max(st_coordinates(df_sf_buffer_1000)[,1]),
            max(st_coordinates(df_sf_buffer_1000)[,2]))%>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = df_sf_buffer_1000,
              fillOpacity = 0.2, 
              group = "Zone tampon (1000m)", color = 'red')%>%
  addCircles(data = restaurant,
             color = "blue",
             fill = "blue",
             opacity = 1, 
             group = "restaurant") %>%
  addCircles(data = df_sf,
             color = "yellow",
             fill = "yellow",
             opacity = 1, 
             group = "Ma position") %>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("Ma position", "Zone tampon (1000m)","restaurant"),
    options = layersControlOptions(collapsed = FALSE)
  )  %>%
  addLegendCustom(colors = c("red","yellow","blue"), labels = c("Zone tampon","Ma position", "Restaurant"), sizes = c(20,20))

map
saveMap(map, 'buffer')
results[['buffer']] <- map

# Intersects ---------------------------------------------------------------------
restaurant_nextToMe <- restaurant %>%
  filter(st_intersects(., df_sf_buffer_1000, sparse = FALSE))

map <- leaflet() %>%
  addProviderTiles("Stamen.Toner") %>% 
  addPolygons(data = df_sf_buffer_1000,
              fillOpacity = 0.2, 
              group = "Zone tampon", color = 'red')%>%
  addCircles(data = restaurant_nextToMe,
             color = "blue",
             fill = "blue",
             opacity = 1, 
             group = "Restaurant à proximité") %>%
  addCircles(data = df_sf,
             color = "yellow",
             fill = "yellow",
             opacity = 1, 
             group = "Ma position") %>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("Ma position", "Restaurant à proximité", "Zone tampon"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegendCustom(colors = c("red","yellow","blue"), labels = c("Zone tampon","Ma position", "Restaurant"), sizes = c(20,20))

map
saveMap(map, 'intersects')
results[['intersects']] <- map

# Distance ----------------------------------------------------------------
distances.mx <- as.numeric((st_distance(df_sf,restaurant_nextToMe)))
distances.df <- data.frame(place=rep("Station de bus", length(distances.mx)),distance=distances.mx)
restaurant_nextToMe$distance <- distances.df$distance

box.plot_distance <- ggplot(data = distances.df, aes(x=place, y=distance)) + 
  geom_boxplot(color='#999999', fill='#E69F00',alpha=0.5) +
  stat_summary(fun=mean, geom="point", color='red', size=4)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.2)+
  labs(title="Distances entre ma position et les restaurants",
       subtitle = 'Zone tampon de 1000 mètres',
       x="", y = "Distance (m)")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        title = element_text(size=9))

results[['box.plot_distance']] <- box.plot_distance

curve.plot_distance <- ggplot(distances.df, aes(x=distance)) + 
  geom_density(color='#E69F00')+    
  labs(title="Courbe de distribution des distances",
       subtitle = 'Zone tampon de 1000 mètres',
       x="Distance (m)", y = "Density")+
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        title = element_text(size=9))

results[['curve.plot_distance']] <- curve.plot_distance


# temps de parcours  -------------------------------------------------------------------------

routeToMin <- osrmRoute(src = df_sf, dst = restaurant_nextToMe[which.min(restaurant_nextToMe$distance),], returnclass = "sf", osrm.profile = "foot")
routeToMin_coords <- as.data.frame(st_coordinates(routeToMin))
routeToMax <- osrmRoute(src = df_sf, dst = restaurant_nextToMe[which.max(restaurant_nextToMe$distance),], returnclass = "sf", osrm.profile = "foot")
routeToMax_coords <- as.data.frame(st_coordinates(routeToMax))

map <- leaflet() %>%
  addProviderTiles("Stamen.Toner") %>% 
  addCircles(data = restaurant_nextToMe[which.max(restaurant_nextToMe$distance),],
             color = "blue",
             fill = "blue",
             opacity = 1, 
             group = "Station la plus éloignée") %>%
  addPolylines(data = routeToMax_coords,
               lng = ~X, lat = ~Y,
               color = "blue",
               opacity = 1, 
               group = "Route vers la station la plus éloignée") %>%
  addCircles(data = restaurant_nextToMe[which.min(restaurant_nextToMe$distance),],
             color = "red",
             fill = "red",
             opacity = 1, 
             group = "Station la plus proche") %>%
  addPolylines(data = routeToMin_coords,
               lng = ~X, lat = ~Y,
               color = "red",
               opacity = 1, 
               group = "Route vers la station la plus proche") %>%
  addCircles(data = df_sf,
             color = "yellow",
             fill = "yellow",
             opacity = 1, 
             group = "Ma position") %>% 
  # Layers control
  addLayersControl(
    overlayGroups = c("Ma position", 
                      "Station la plus proche","Route vers la station la plus éloignée",
                      "Station la plus éloignée", "Route vers la station la plus proche",
                      "Zone tampon"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegendCustom(colors = c("red","yellow","blue"), labels = c("Station la plus proche","Ma position", "Station la plus éloignée"), sizes = c(20,20))

map
saveMap(map, 'roads')
# results[['roads']] <- map
results[['roadmax']] <- routeToMax
results[['roadmin']] <- routeToMin
save(results, file='results_rattrapage.RData')
