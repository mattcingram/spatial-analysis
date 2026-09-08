################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 20260908
# steps here: basic mapping of different data types; retrieving background maps 
# from web
#
################################################################################

############################
# if returning to project, load last working data file:
# e.g.:
load("./data/working/working20230626_processing.RData")

############################
# Thematic maps with ggplot2
############################

# with geom_sf()

# basic choropleth of ghana with vote share measure (p_share)
# note: "choropleth" (with no "L")
# note: these are "basic", but they are much nicer than core "plot" function in R

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share))
print(g)

# reverse coding of mapped variable so that darker shades 
# indicate higher values, and clean up background

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +  # reverse coding
  theme_minimal()   # minimal layout/formatting
g

# note: next steps polish and add data to this basic map

############################
# just roads
############################

png(file="./figures/roads_plot.png", height=6, width=6, units="in", res=300)
plot(roads.sf$geometry)
dev.off()

g <- ggplot(data=roads.sf) +
  #geom_sf(aes(fill=p_share)) +
  #scale_fill_continuous(trans = 'reverse') +
  #geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(color="black") +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/roads_ggplot.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

############################
# just capitals
############################

png(file="./figures/capitals_plot.png", height=6, width=6, units="in", res=300)
plot(capitals.sf$geometry)
dev.off()

g <- ggplot(data=capitals.sf) +
  #geom_sf(aes(fill=p_share)) +
  #scale_fill_continuous(trans = 'reverse') +
  #geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(color="black") +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/capitals_ggplot.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

###########################
# make grid over country shapefile
###################################

grd <- st_make_grid(polygons, n = 50)
# visualize the grid
plot(grd)

############################
# nicer maps
############################

# with geom_sf(), can re-check which lake is Lake volta
# should be object 4 within lakes object
ggplot(data=lakes.sf[4,]) +
  geom_sf() 
# or
plot(lakes.sf)
plot(lakes.sf[4,])

# add Lake Volta to the previous base map of Ghana
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

# add roads

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

# add electricity grid
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=electricity.sf, color="yellow", inherit.aes = FALSE) +
  #coord_sf(default_crs=sf::st_crs(4326)) +     # set a crs that applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

# add district capitals
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=electricity.sf, color="yellow", inherit.aes = FALSE) +
  geom_sf(data=capitals.sf, color="green") +
  #coord_sf(default_crs=sf::st_crs(4326)) +  # to set a crs that applies to all layers
  #xlim(-3.5, 1.5) +  # to limit x range
  #ylim(4.5, 11.5) +  # to limit y range
  theme_minimal()
g


png(file="./figures/map_p_share.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# combined graph for book of areas, lines, and points; with no fill variable

# base R

plot(st_geometry(shp.sf))
plot(st_geometry(roads.sf), add=TRUE, col="red")
plot(st_geometry(capitals.sf), add=TRUE, col="blue")

# ggplot

g <- ggplot(data=shp.sf) +
  geom_sf(color="black") +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=capitals.sf, color="blue") +
  #coord_sf(default_crs=sf::st_crs(4326)) +  # to set a crs that applies to all layers
  #xlim(-3.5, 1.5) +  # to limit x range
  #ylim(4.5, 11.5) +  # to limit y range
  theme_minimal()
g

png(file="./figures/map_combined.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

########################################################################

# add raster data: 
# so that can place Ghana map on background satellite map of region

# get satellite images; here are some examples
register_google(key="AIzaSyBjzmzxEM8x1LMZMVg_MrnHAtyUWTwmBDg")
region <- get_map("ghana", maptype = "satellite", zoom = 6, source = "google")
region2 <- get_map("ghana", maptype = "satellite", zoom = 7, source = "google")
region3 <- get_map("ghana", maptype = "terrain", zoom = 7, source = "google")
region4 <- get_map("ghana", maptype = "hybrid", zoom = 7, source = "google")

# can also use different source, e.g., ggspatial and rnaturalearth packages
# from ggspatial
region_gg <- annotation_map_tile(type = "osm", zoom = 7)

# after extracting with get_map, save each as .RData file

save(region, file="./data/original/region.RData")
save(region2, file="./data/original/region2.RData")
save(region3, file="./data/original/region3.RData")
save(region4, file="./data/original/region4.RData")

# can re-load each image; make sure ggmap installed so that image recognized
load("./data/original/region.RData")
load("./data/original/region2.RData")
load("./data/original/region3.RData")
load("./data/original/region4.RData")

# check
ggmap(region)
ggmap(region4)

g <- ggmap(region2) + 
  geom_sf(data=shp.sf, aes(fill=p_share), inherit.aes = FALSE) +
  scale_fill_continuous(trans = 'reverse') +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=electricity.sf, color="yellow", inherit.aes = FALSE) +
  geom_sf(data=capitals.sf, color="green", inherit.aes = FALSE) +
  #coord_sf(default_crs=sf::st_crs(4326)) +  # to set a crs that applies to all layers
  #xlim(-3.5, 1.5) +  # to limit x range
  #ylim(4.5, 11.5) +  # to limit y range
  theme_minimal()
g

# save image to file
png(file="./figures/map_p_share1.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# or
ggsave("./figures/map_p_share2.png", plot=g, 
       height=6, width=6, units="in", dpi=300)

# combined map for book with region2

g <- ggmap(region2) +
  geom_sf(data=shp.sf, color="black", inherit.aes = FALSE) +
  geom_sf(data=roads.sf, color="red", inherit.aes = FALSE) +
  geom_sf(data=capitals.sf, color="blue", inherit.aes = FALSE) +
  coord_sf(default_crs=sf::st_crs(4326)) +  # to set a crs that applies to all layers
  #xlim(-3.5, 1.5) +  # to limit x range
  #ylim(4.5, 11.5) +  # to limit y range
  theme_minimal()
g

ggsave("./figures/map_combined_ggmap.png", plot=g, 
       height=6, width=6, units="in", dpi=300)

# alternate combined map with ggspatial object 
g <- ggplot() +
    region_gg + # this is the Open Street Maps object
    geom_sf(data = shp.sf, 
            color="black",#, inherit.aes = FALSE
            alpha=0.5) +
    theme_minimal()
g

png(file="./figures/map_combined_ggspatial_osm.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# remove large ggmap objects (take up memory)
rm(region, region2, region3, region4)

# keep these mapping tools in mind for visualization of 
# other statistical results

# not saving data/image now because did not add any new data or processing

#######################################################
# other thematic maps

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=p_share)) +
  scale_fill_continuous(low="white", high="black", name="vote %") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_p_share.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# solar panels 

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=Count_)) +
  scale_fill_continuous(low="white", high="black", name="y") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  # high = "black",  # colors the maximum values
   # midpoint = 0, name="y"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_y.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# gini

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=gini_2008)) +
  scale_fill_continuous(low="white", high="black", name="gini") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_gini.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# literacy

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=literacy)) +
  scale_fill_continuous(low="white", high="black", name="literacy") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_literacy.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# pop density

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=Pop_Densit)) +
  scale_fill_continuous(low="white", high="black", name="pop density") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_popdensity.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# road density

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=Density_RD)) +
  scale_fill_continuous(low="white", high="black", name="road density") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_roaddensity.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=grid_densi)) +
  scale_fill_continuous(low="white", high="black", name="grid density") +  
  #scale_fill_gradient2(
  #  low = "lightgrey",   # used if no negative values
  #  mid = "white",       # colors exactly 0
  #  high = "black",  # colors the maximum values
  #  midpoint = 0, name="gini"
  #) +
  theme_minimal()   # minimal layout/formatting
g

png(file="./figures/map_basic_griddensity.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

#end
