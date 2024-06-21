# a comparison between sp and sf package for plotting grid points
library(geoR)
library(sp)
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)

# getting map of comilla for plotting, id for comilla is 11
map_district <- readRDS('BGD_adm2.rds')
map_comilla <- subset(map_district, map_district$OBJECTID == 11)

# transforming coordinate to UTM using EPSG = 32646 for WGS=84, UTM Zone = 46N
utm_comilla <- spTransform(map_comilla, CRS("+init=epsg:32646"))

# re-scale UTM into km
# box <- bbox(utm_comilla)/1000
border <- utm_comilla@polygons
border <- border[[1]]@Polygons[[1]]@coords
border <- as.data.frame(border/1000)
colnames(border) <- c('east', 'north')

# start with large by value, limit the number of new_point to test if it is working
# with powerful computer, this can re-fine upto 2 or 1
loc_new <- pred_grid(border, by = 1)
loc_new_in <- locations.inside(loc_new, border)
colnames(loc_new_in) <- c('east', 'north')

# plot to see the point locations
psp <- ggplot(border, aes(east, north)) + geom_polygon(color = 'grey', fill = "white") + 
  geom_point(data = loc_new_in, aes(x = east, y = north), color = '#219ebc', size = 0.5) + 
  theme_bw() + ggtitle('1km Grid by sp package')

# getting map of comilla
map <- readRDS('BGD_adm2.rds') %>%  st_as_sf(., crs = 4326) %>% 
  filter(NAME_2 == 'Comilla') %>% select(name = NAME_2)

# find UTM code
lon <- st_coordinates(map)[1, 'X']
utm_zone <- floor((lon + 180)/6) + 1
utm_crs <- paste0("EPSG:", 32600 + utm_zone)

utm <- st_transform(map, crs = utm_crs)

grid <- st_make_grid(utm, cellsize = 1000, what = "centers") %>% 
  st_sf(geometry = .) %>% st_intersection(., utm)

psf <- ggplot() +  geom_sf(data = utm, fill = 'white') +
  geom_sf(data = grid, color = "#219ebc", size = 0.5) +
  theme_bw() + ggtitle('1km Grid by sf package')

psp + psf