# https://mern.gouv.qc.ca/territoire/portrait/portrait-donnees-mille.jsp
library(sf)
qc <- read_sf("qc/region_admin_poly.shp")
qc <- st_transform(qc, crs = 4326)

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
url <- 'https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/situation-coronavirus-quebec/'

covid <- read_html(url) %>% html_nodes("table") %>% 
                            html_table() %>% .[[1]] %>% 
                            as_tibble() %>%
                            slice(-1, -n()) %>% 
                            rename(region = 1, Case = 2) %>%
                            separate(region, c("RES_CO_REG", "Name"), " - ") %>%
                            mutate(Case = str_replace_all(Case,'\U00A0', '')) %>%
                            mutate_at('Case', as.integer) 

mtl <- tibble(name = 'Montreal', Long = -73.58781, Lat = 45.50884)
mtl <- st_as_sf(mtl, coords = c("Long", 'Lat'), remove = FALSE, 
                      crs = 4326, agr = "constant")

library(ggplot2)
library(ggrepel)

qc <- left_join(qc %>% select(RES_NM_REG, RES_CO_REG, geometry), covid %>% select(-Name))

p <- ggplot() + geom_sf(data = qc, aes(fill = log10(Case))) + 
   scale_fill_viridis_c(labels = c(1, 10, 100, 1000)) + theme_bw() + 
   theme(plot.title = element_text(hjust = .5),
         plot.subtitle = element_text(hjust = .5),
         plot.caption = element_text(hjust = .5)) +
   guides(fill = guide_colourbar(barwidth = 20, barheight = .8))

p1 <- p + labs(title = "COVID-19 in Quebec",
               subtitle = paste(Sys.Date()),
               caption = "Source: Quebec.ca", 
               fill = "Cases") + theme(legend.position = "none")

p2 <- p + labs(title = "COVID-19 near Montreal",
               subtitle = paste(Sys.Date()),
               caption = "Source: Quebec.ca", 
               fill = "Cases") + 
          geom_sf(data = mtl) + xlab('') +  ylab('') + 
          geom_label_repel(data = mtl, aes(x = Long, y = Lat, label = name),
                           fontface = "bold", nudge_x = -.3, nudge_y = -.2) + 
          coord_sf(xlim = c(-75, -72.5), ylim = c(45, 47), expand = FALSE)


library(ggpubr)
ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom")

### interactive
library(tmap)
# tmap_mode("plot")
tmap_mode("view")
tm_shape(qc) +  tm_polygons("Case", title = "COVID19 Case", 
                            style = "fixed",
                            breaks = c(1, 10, 100, 300, 1000, Inf)) +
                tm_layout(aes.palette = list(seq = "-viridis"))
              

### montreal data and viz
# http://donnees.ville.montreal.qc.ca/dataset/polygones-arrondissements
mtl <- read_sf("limadmin-shp/LIMADMIN.shp")

url <- 'https://santemontreal.qc.ca/en/public/coronavirus-covid-19/#c36391'
cvmtl <- read_html(url) %>% html_nodes("table") %>% 
                            html_table() %>% .[[1]] %>% 
                            as_tibble() %>%
                            slice(-c(n() - 1, n())) %>% 
                            rename(NOM = 1, Case = 2) %>%
                            mutate(NOM = str_replace(NOM,'\\*','')) %>%
                            mutate(Case = str_replace_all(Case,',','')) %>%
                            mutate_at('Case', as.integer) 

cvmtl$NUM <- c(24, 9, 71, 7, 27, 72, 11, 1, 10, 3,
               17, 18, 6, 23, 74, 16, 75, 2, 5, 13,
               22, 8, 19, 25, 76, 15, 14, 77, 21, 12,
               20, 26, 4)

cvmtl <- cvmtl %>% select(-NOM)

mtl <- mtl %>% select(NUM, NOM, TYPE) %>% left_join(cvmtl) %>% select(-NUM)

library(tmap)
# tmap_mode("plot")
tmap_mode("view")
tm_shape(mtl) +  tm_polygons("Case", title = "COVID19 Case", 
                            style = "fixed",
                            breaks = c(0, 10, 50, 100, 300, Inf)) +
                            tm_layout(aes.palette = list(seq = "-viridis"))
