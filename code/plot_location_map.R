rm(list=ls())
setwd("C:/DFO-MPO/Work/Sockeye_paper/code")
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

crs = "+proj=laea +lat_0=52 +lon_0=260 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs "

sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = crs) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))

a <- st_as_sf(data.frame(plot_id = 1, lat = -122, long = 52.5), 
              coords = c("lat", "long"), crs = 4326)
ggplot() +
  geom_sf(data = sphere, fill = "aliceblue",lwd = 3) +
  geom_sf(data = world,lwd = 0)+
  geom_sf(data = a, shape = 22, 
          size = 3, color = "darkred", fill = "pink",alpha = 0.5,stroke = 1) +
  #geom_rect(aes(xmin = 1804417, xmax = -2304417, ymin = 3124417, ymax = 1404417), color = "darkred", fill = NA)  +
  coord_sf(crs = crs) +
  theme_minimal() #+
 # theme(panel.grid.major = element_line(colour = "darkblue",linetype = 2))
ggsave("../plot/plot_global.png",w = 3, h = 3)  

# png(file="plot_figure1_location_map_global.png",h=1000,w=1000,bg=NA)
# m<-map('world',plot=FALSE)
# map('world',proj='azequalarea',orient=c(41,-74,0), boundary=TRUE,fill=T,col="grey")
# map.grid(c(-135,-122,48,56),col="brown3", label=FALSE, lty=1,lwd=1)
# map.grid(m,col="darkblue", label=FALSE, lty=2, pretty=FALSE)
# dev.off()
