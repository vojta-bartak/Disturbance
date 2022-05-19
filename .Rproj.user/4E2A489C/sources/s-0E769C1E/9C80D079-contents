library(sf)
library(tmap)

points <- st_read("Vrstva_bodu/Body_Sumava_2021_clanek.shp")

tmap_mode("view")
tm_shape(points) + tm_dots("Subtyp_fin")
