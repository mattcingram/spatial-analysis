################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last update: 20260710
# steps here: load data and process
#
##################################################################

### Load Data Files

##################################################################

# Note: 'shapefile' is common format for spatial data
# other formats: geojson (.geojson), geopackage (.gpkg)

# Load shapefile with district data
# note structure of shapefile (minimum of 3 files: .shp, .dbf, .shx)
# don't need to call all three separately; all will load automatically

# two main packages to load shapefiles: rgdal and sf
# rgdal used to be common but is retiring
# sf is dominant package now
# however, good to know both in case you come across other projects using rgdal
# here, briefly look at readOGR() from rgdal pkg, then focus on sf pkg

# using rgdal:
shp <- readOGR(dsn="./data/original", 
               layer="20170226_Districts")

# can make quick plot to check that loaded correctly
png(file="./figures/map_shp_basic.png", height=6, width=6, units="in", res=300)
plot(shp)
dev.off()

#inspect
names(shp)
class(shp)
str(shp)
str(shp@data)
summary(shp)

# Variable descriptions from authors

###Districts 2008
#Count_ is Total Solar
#pov_p_2008 is Percent in poverty
#gini_2008 is Gini Index
#ferat_2008 is Female Ratio
#p_share is NDC 2008 (MI note: NDC vote share)
#p_shvol is NDC vote share volatility
#p_turn is Turnout 2008
#p_tuvol is Turnout volatility
#p_ethfr is Ethnic Fractionalization
#Count_3 is World Bank projects
#Density_RD is Road density
#Pop_Densit is population density
#Count_4 is health facilities
#literacy is literacy
#grid_densi is grid density
#grid_perCa is grid per capita
#Count_5 is Lake Volta dummy

# reverse % turnout (p_turn) to get % nonvoters
shp$turninv<- (1-shp$p_turn)


###########################################################################
# note from original authors' code:
# "In ArcMap, I used Intersect between districts and electric grid.
# Then, I used Dissolve for grid based on districts. I projected to 1984 WCS Mercator projection system.
# Then, I used "Calculate Geometry" to calculate line lengths.
# Read in the resultant data below.

# NOTE MI 2023-04: do not need to do this because grid per capita is already in 
# data from authors
# however, have code here for how to merge in non-spatial data and combine with
# spatial data
# as long as unit labels are the same in both data objects, can merge;
# variables do not have to have same name:

# Example
newer.grid.length<- read.csv("./data/original/Grid_line_lengths_2.csv")
str(newer.grid.length)  
# note: only have data for 147 of 170 districts
summary(newer.grid.length)
shp.merged <- merge(shp, newer.grid.length,by.x = "DIST_2008",
                    by.y= "DIST_2008", all.x=TRUE)
summary(shp.merged)
# have 23 missing values (NAs)
# replace NA with zero
shp.merged@data$length[is.na(shp.merged@data$length)] <- 0
summary(shp.merged)

# now have new variable merged with geographic features of shapefile

# remove objects since don't need them here
rm(shp.merged, newer.grid.length)

###############################################
# note on merging:
# when merging data from other sources, e.g., Afrobarometer round 4 (2008),
# could encounter problems with how districts are identified
# e.g., district IDs may change, number of districts may change
afrobar3 <- read_sav("./data/original/afrobarometer/gha_r3_data.sav") #2005
afrobar4 <- read_sav("./data/original/afrobarometer/gha_r4_data.sav") #2008
afrobar5 <- read_sav("./data/original/afrobarometer/gha_r5_data.sav") #2012

# district variable:
str(table(afrobar3$district)) # only 102 districts

table(afrobar4$DISTRICT)
str(table(afrobar4$DISTRICT)) # only 105 of 170 districts, but most do not match shp

str(table(afrobar5$DISTRICT)) # 180 districts, but many many different from shp

str(shp$DIST_2008)
table(shp$DIST_2008)

table(shp$DIST_2008)[1:20]
table(afrobar4$DISTRICT)[1:20]

# e.g., Accra Metropolis in shp is "A M A" in afrobar4, or there are some units
# that might be hard to reconcile, e.g., ADANSI NORTH and ADANSI SOUTH in shp,
# but ADANSI EAST and ADANSI WEST in afrobar4

# these are common issues in merging any two data files, and are not unique to 
# spatial analysis

# UNICEF MICS 2006 HH data
mics2006hh <- read_sav("./data/original/UNICEF_MICS_2006/hh.sav")
# from questionnaire in annex of final report (https://mics.unicef.org/surveys), hh7a has district info

str(table(mics2006hh$HH7A))

# Afrint data from Ghana statistics
afrint <- read_sav("./data/original/AFRINT2008/Afrint village level data 2002 and 2008.sav")

###################################################################
###################################################################

# other data 
# note: these files are NOT from authors

# Ghana Lakes shapefile from World Bank
# source: https://wbwaterdata.org/dataset/ghana-lakes
lakes <- readOGR(dsn="./shapefiles/lakes_worldbank", 
                 layer="Lakes")
plot(lakes)

# Ghana roads file from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0039327
roads <- readOGR(dsn="./shapefiles/ghana_roads_worldbank", 
                 layer="GHA_roads")
plot(roads)

# Ghana electricity grid from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0041733
electricity <- readOGR(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")

# Ghana district capitals from World Bank
# source: https://datacatalog.worldbank.org/search/dataset/0039930
capitals <- readOGR(
  dsn="./shapefiles/district_capitals_worldbank", 
  layer="GHA_District_Capitals")

plot(capitals)

#########################################################
# preferred: use sf package
# from here on, focus on using sf package, though still occasionally mention code from rgdal pkg
# read shapefiles as sf objects

shp.sf          <- st_read(dsn="./data/original", 
                  layer="20170226_Districts")

# reverse % turnout (p_turn) to get % nonvoters
shp.sf$turninv<- (1-shp.sf$p_turn)

lakes.sf        <- st_read(dsn="./shapefiles/lakes_worldbank", 
                    layer="Lakes")
roads.sf        <- st_read(dsn="./shapefiles/ghana_roads_worldbank", 
                    layer="GHA_roads")
electricity.sf  <- st_read(
  dsn="./shapefiles/ghana-electricity-transmission-network_worldbank", 
  layer="Ghana Electricity Transmission Network")
capitals.sf     <- st_read(
  dsn="./shapefiles/district_capitals_worldbank", 
  layer="GHA_District_Capitals")

districtsODI <- st_read(
  dsn="./shapefiles/GhanaODI_Districts170", 
  layer="Ghana_Districts_170")

# with sf objects, can plot vars quickly to inspect
class(shp.sf)
names(shp.sf)
str(shp.sf)
# just plot shape, no data
plot(shp.sf$geometry)
# or
png(file="./figures/map_shp.sf_basic.png", height=6, width=6, units="in", res=300)
plot(shp.sf$geometry)
dev.off()

# basic plot of one variable
mycolors <- function(n) hcl.colors(n, "Viridis")
png(file="./figures/map_shp.sf_basic_p_share.png", height=6, width=6, units="in", res=300)
plot(shp.sf["p_share"], pal = mycolors)
dev.off()

png(file="./figures/map_roads.sf_basic.png", height=6, width=6, units="in", res=300)
plot(roads.sf$geometry)
dev.off()

png(file="./figures/map_shp.sf_basic.png", height=6, width=6, units="in", res=300)
plot(capitals.sf$geometry)
dev.off()

#####################################
# COORDINATE REFERENCE SYSTEMS (CRS)
# To work with all files together, need two things: 
# (1) need to make sure make sure COORDINATE REFERENCE SYSTEM (CRS) match 
# across objects
# (2) need to make sure we are using an appropriate projected CRS
# CRS accounts for the coordinate locations of the data, and the 
# projection accounts for the way 3-D spherical surfaces are represented on 2D surface and in 2D calculations
# 

# notes:
# for comparative work, a universal transverse mercator (UTM) projected crs 
# is good because they are specific to global regions, and are metric
# for US work across the whole country, UTM is still good; for state/local work, state plane coordinate system
# (or SPCS) is better

# check if crs matches across objects
st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(shp) == st_crs(capitals)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(lakes) == st_crs(capitals)
st_crs(roads) == st_crs(electricity)
st_crs(roads) == st_crs(capitals)
st_crs(electricity) == st_crs(capitals)
# only roads, electricity, and capitals are the same

# check to see what crs is with rgdal pkg
proj4string(shp)
crs(shp)
#NA, so core shapefile does not have a CRS

proj4string(lakes)
st_crs(lakes)

proj4string(roads)
st_crs(roads)

proj4string(electricity)
st_crs(electricity)

proj4string(capitals)
st_crs(capitals)

# existing CRS in some objects is WGS84, which is not an appropriate one

#### NEED TO CHANGE TO UTM projected CRS; for Ghana, this is UTM Zone 30N, or EPSG: 32630


shp.sf <- st_transform(shp.sf, crs = 32630) # this is code for Ghana, UTM Zone 30N: EPSG: 32630

# make all the same as roads, capitals, or electricity
proj4string(shp) <- CRS("+init=epsg:32630")
# or
#proj4string(shp) <-CRS("+init=epsg:24383")

# lakes already has a CRS, but unmatched one
# cannot use proj4string; need to change one CRS to another
# check here to find correct projection string for WGS 84, 
# which is what shp, roads, and electricity now have:
# https://spatialreference.org/ref/epsg/4326/
lakes <- spTransform(lakes, CRS=
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# just to be safe, convert all to same CRS; spTransform is from rgdal pkg
shp <- spTransform(shp, CRS=
                     CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
roads <- spTransform(roads, CRS=
                       CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
electricity <- spTransform(electricity, CRS=
                             CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
capitals <- spTransform(capitals, CRS=
                             CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# re-check CRS
st_crs(shp) == st_crs(lakes) 
st_crs(shp) == st_crs(roads)
st_crs(shp) == st_crs(electricity)
st_crs(shp) == st_crs(capitals)
st_crs(lakes) == st_crs(roads)
st_crs(lakes) == st_crs(electricity)
st_crs(lakes) == st_crs(capitals)
st_crs(roads) == st_crs(electricity)
st_crs(roads) == st_crs(capitals)
st_crs(electricity) == st_crs(capitals)
# all TRUE

# could have also transformed all sp objects to sf objects
# and then checked crs
shp.sf <- st_as_sf(shp)
lakes.sf <- st_as_sf(lakes)
roads.sf <- st_as_sf(roads)
electricity.sf <- st_as_sf(electricity)
capitals.sf <- st_as_sf(capitals)


#################################
# sf objects  
# focus here
# could convert to sf objects and re-transform and re-check
# or could simply check the previously loaded sf objects 

# first, just check crs of sf objects
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
# as was case initially with sp objects, only roads, electricity,
# and capitals match

# need to transform:

# can set to crs of another sf object
# however, cannot do this if the crs is missing (NA)
# check this with shp.sf, which had NA for crs
# shp.sf <- st_transform(shp.sf, st_crs(roads.sf))
# st_crs(shp.sf)


# if NA, can specify crs
shp.sf <- st_set_crs(shp.sf, 32630)  # if want to change, use st_transform()
st_crs(shp.sf)

shp.sf <- st_transform(shp.sf, crs = 32630)
lakes.sf <- st_transform(lakes.sf, crs = 32630)
roads.sf <- st_transform(roads.sf, crs = 32630)
capitals.sf <- st_transform(capitals.sf, crs = 32630)
electricity.sf <- st_transform(electricity.sf, crs = 32630)

districtsODI <- st_transform(districtsODI, crs = 32630) 
#districtsODI <- st_transform(districtsODI, crs = 32618) 

# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
st_crs(districtsODI) == st_crs(capitals.sf) 
# all TRUE



# check
st_crs(shp.sf) == st_crs(lakes.sf) 
st_crs(shp.sf) == st_crs(roads.sf) 
st_crs(shp.sf) == st_crs(electricity.sf) 
st_crs(shp.sf) == st_crs(capitals.sf) 
st_crs(lakes.sf) == st_crs(roads.sf) 
st_crs(lakes.sf) == st_crs(electricity.sf) 
st_crs(lakes.sf) == st_crs(capitals.sf) 
st_crs(roads.sf) == st_crs(electricity.sf) 
st_crs(roads.sf) == st_crs(capitals.sf) 
st_crs(electricity.sf) == st_crs(capitals.sf) 
# all TRUE



# can make quick plot to check that loaded correctly
plot(shp)
plot(lakes, add=TRUE, col="black")
plot(roads, add=TRUE, col="red")
plot(electricity, add=TRUE, col="yellow")


# subset within lakes
plot(lakes.sf)  # lake 4 is Volta
plot(lakes.sf[4,])

# if needed, can crop roads to fit country shp; different form of spatial subset
#roads.sf.crop <- roads.sf[shp.sf,]

# now that have appropriate CRS (UTM 30N) and all CRS match, can use to merge data using geography


##############################################

# matching districts from two shapefiles: shp.sf and districtsODI 

districtsODI_pts <- st_centroid(districtsODI)
districtsODI_pts_sub <- subset(districtsODI_pts, select=c(DIST_CODE))

districtsODI_pts_sub_df <- data.frame(cbind(districtsODI_pts_sub$DIST_CODE,
                                            st_coordinates(districtsODI_pts_sub)))
colnames(districtsODI_pts_sub_df) <- c("DIST_CODE", "x", "y")


ggplot() +
  geom_sf(data=shp.sf, aes(fill=NULL)) +
  geom_sf(data=districtsODI_pts_sub, alpha=0.5) +
  coord_sf() + 
  theme_minimal()

merge1 <-st_join(shp.sf, districtsODI_pts_sub, join = st_intersects)

districtsODI$distmatch <-  districtsODI$DISTRICT %in% shp.sf$DIST_2008
table(districtsODI$distmatch)
# 28 from districtsODI that don't match
districtsODI$DISTRICT[districtsODI$distmatch==FALSE]

[1] "DANGBE WEST"                          "A M A"                               
[3] "TEMA"                                 "ZABZUGU TATALI"                      
[5] "YENDI"                                "TAMALE METRO"                        
[7] "TOLON KUMBUGU"                        "SISSALA  WEST"                       
[9] "TALENSI NABDAM"                       "BAWKU WEST"                          
[11] "NKORANZA NORTH"                       "HOHOE"                               
[13] "K M A"                                "EWUTU SENYA"                         
[15] "JUABESO"                              "MPOHOR WASSA EAST"                   
[17] "AOWIN / SUAMAN"                       "SUHUM / KRABOA COATAR"               
[19] "TWIFO HEMAN / HEMAN / LOWER DENKYIRA" "ASANTE AKIM NORTH"                   
[21] "AHAFO ANO N0RTH"                      "TECHIMAN"                            
[23] "NKORANZA SOUTH"                       "DANGBE EAST"                         
[25] "NORTH DAYI"                           "HO"                                  
[27] "MAMPRUSI WEST"                        "ADAKLU ANYIGBE" 

districtsODI$DISTRICT[districtsODI$distmatch==FALSE][1] %in% shp.sf$DIST_2008

shp.sf$DIST_2008

# clean by hand:
# first, shp.sf
shp.sf$DIST_2008_clean <- trimws(tolower(shp.sf$DIST_2008))
# remove punctuation
shp.sf$DIST_2008_clean <- gsub("[[:punct:]]", " ", shp.sf$DIST_2008_clean)
shp.sf$DIST_2008_clean <- stringr::str_squish(shp.sf$DIST_2008_clean)
shp.sf$DIST_2008_clean


# second, districtsODI
districtsODI$DISTRICT_clean <- districtsODI$DISTRICT |>
  tolower() |>
  trimws() |>
  stringr::str_replace_all("[[:punct:]]", " ") |>
  stringr::str_squish()

districtsODI$DISTRICT_clean

# re-check district match
districtsODI$distmatch_clean <-  districtsODI$DISTRICT_clean %in% shp.sf$DIST_2008_clean
table(districtsODI$distmatch_clean)
# 25 from districtsODI that don't match
districtsODI$DISTRICT_clean[districtsODI$distmatch_clean==FALSE]

shp.sf$DIST_2008_clean

districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="ho"] <- "ho municipal"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="hohoe"] <- "hohoe municipal"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="k m a"] <- "kma"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="dangwe west"] <- "dangme west"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="dangbe east"] <- "dangme east"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="tamale metro"] <- "tamale metropolis"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="zabzugu tatali"] <- "zabzugu tatale"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="yendi"] <- "yendi municipal"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="a m a"] <- "accra metropolis"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="tolon kumbugu"] <- "tolon kumbungu"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="twifo heman heman lower denkyira"] <- "twifo lower denkyira"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="nkoranza north"] <- "nkoranza"
districtsODI$DISTRICT_clean[districtsODI$DISTRICT_clean=="a m a"] <- "accra metropolis"


districtsODI$distmatch_clean <-  districtsODI$DISTRICT_clean %in% shp.sf$DIST_2008_clean
table(districtsODI$distmatch_clean)
# 25 from districtsODI that don't match
districtsODI$DISTRICT_clean[districtsODI$distmatch_clean==FALSE]

shp.sf$DIST_2008_clean




# using stringdist pkg
p_load(stringdist)
library(stringdist)
similarity <- stringsim(districtsODI$DISTRICT_clean, shp.sf$DIST_2008_clean, method = "jw")
similarity
table(similarity)

p_load(fuzzyjoin)
library(fuzzyjoin)
matched_df <- stringdist_join(
  districtsODI, shp.sf, 
  by = c("DISTRICT_clean" = "DIST_2008_clean"),
  mode = "left",
  method = "jw",       # Jaro-Winkler method
  max_dist = 0.2,      # Match threshold (lower means closer match)
  distance_col = "dist_score"
)

matched_df_unique <- unique(matched_df)
names(matched_df)
str(matched_df)


#############################################

# MERGING NON_SPATIAL DATA with SPATIAL DATA
# assume have another data set that want to merge with this one

# simple example
# imagine part of the replication data is in a separate file

names(shp.sf)

area_df <- data.frame(st_area(shp.sf))
colnames(area_df) <- c("area_msq")
area_df <- cbind(shp.sf$DIST_2008, area_df)
area_df <- st_drop_geometry(area_df)
str(area_df)

write.csv(area_df, file="./data/working/area_df.csv", row.names = FALSE)

area_df <- read.csv(file="./data/working/area_df.csv", header=TRUE, sep=",")

temp <- sp::merge(shp.sf, area_df, by="DIST_2008", all.x=TRUE)
#plot(temp["area_msq"])

g <- ggplot(temp) +
  geom_sf(aes(fill=as.numeric(area_msq)), show.legend = FALSE) +
  scale_fill_viridis_c(option = "viridis", na.value="white") +
  theme_minimal() 
g

png(filename = "./figures/merge_polygons_points_manual.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

#############################################

# merging spatial polygons by spatial points

area_df <- data.frame(st_area(shp.sf))
colnames(area_df) <- c("area_msq")
centroids <- st_coordinates(st_centroid(shp.sf))
area_points <- st_as_sf(cbind(centroids, area_df), 
                        coords=c("X", "Y"), crs=st_crs(shp.sf))
str(area_points)

temp <-st_join(shp.sf, area_points, join = st_intersects) # st_nearest_feature
#plot(temp["area_msq"])

g <- ggplot(temp) +
  geom_sf(aes(fill=as.numeric(area_msq)), show.legend = FALSE) +
  scale_fill_viridis_c(option = "viridis", na.value="white") +
  theme_minimal() 
g

png(filename = "./figures/merge_polygons_points_missing.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

temp <-st_join(shp.sf, area_points, join = st_nearest_feature) # 
#plot(temp["area_msq"])

g <- ggplot(temp) +
  geom_sf(aes(fill=as.numeric(area_msq)), show.legend = FALSE) +
  scale_fill_viridis_c(option = "viridis", na.value="white") +
  theme_minimal() 
g

png(filename = "./figures/merge_polygons_points_nearest_feature.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# plot with ggmapinset showing central district is odd shape and centroid falls outside
p_load(ggmapinset)
library(ggmapinset)

g <- ggplot() +
  geom_sf(data=shp.sf, fill=NA) +
  geom_sf(data=shp.sf[145,]) +
  geom_sf(data=area_points, colour="black", cex=1) +
  theme_minimal() +
  geom_inset_frame(target.aes = list(fill = "white")) + 
  geom_sf_inset(data = shp.sf, fill=NA, map_base = "none") +
  geom_sf_inset(data = shp.sf[145,], map_base = "none", show.legend = FALSE) +
  geom_sf_inset(data=area_points, colour="black", map_base = "none", cex=1) +
  coord_sf_inset(
    configure_inset(
      centre = st_sfc(st_point(c(710000, 890000)), crs = 32630), 
      radius = 50, 
      units = "mi",
      scale = 2,
      translation = c(100, -100)
    )
  )
g


png(filename = "./figures/merge_polygons_points_magnify.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

##############################################


# RASTER DATA
# if have enough memory (i.e., not working remotely on Binder), load full world raster
r2012w <- terra::rast("./raster/broxton/2012/2012.tif")

# if have limited memory, load Ghana subset of Broxton MODIS data 
# instead of the world file above; this subset was created using these steps
# then replace world object with ghana object below after line 384 (where cropped file is saved)
r2012g <- terra::rast("./raster/broxton/broxton2012_ghana.tif")

# check plots
plot(r2012w)
plot(r2012g, col=topo.colors(5), legend=FALSE, add=TRUE)  # palette options: terrain.colors, rainbow, cm.colors
                                            # topo.colors, heat.colors

# check if raster and shp have same crs
st_crs(shp.sf) == st_crs(r2012w)
st_crs(shp.sf) == st_crs(r2012g)
# TRUE 

# make sure have metric versions of each
# note: shp.sf already metric since CRS for UTM Zone 30N is metric, but do it again here
# just to be explicit
shp.sf_metric <- st_transform(shp.sf, crs = 32630) # this is code for Ghana, UTM Zone 30N: EPSG: 32630
r2012w_metric <- project(r2012w, "EPSG:32630", method = "bilinear")
r2012g_metric <- project(r2012g, "EPSG:32630", method = "bilinear")

# crop: this subsets the larger raster object according to the bounding box of the shp obj
r2012wc <- terra::crop(r2012w, shp.sf)
dim(r2012wc)
# 772 x 535
# that is, this raster version of Ghana has 413,020 grid cells (772 * 535) each 500m in size

plot(r2012wc)
plot(shp.sf$geometry, add=TRUE)

# save this cropped raster
writeRaster(r2012wc, "./raster/broxton/broxton2012_ghana.tif")


# convert to df to graph
r2012wc_df <- as.data.frame(r2012wc, xy = TRUE)
colnames(r2012wc_df)
colnames(r2012wc_df)[3] <- "cover_value" 

# transform continuous values into categorical bins (from brown to green)
r2012wc_df2 <- r2012wc_df %>%
  mutate(cover_cat = cut(cover_value, 
                              breaks = c(0, 20, 40, 60, 80, 100), 
                              labels = c("Very Low (Brown)", "Low", "Moderate", "High", "Very High (Green)"),
                              include.lowest = TRUE))

# plot
g <- ggplot() +
  geom_raster(data = r2012wc_df2, aes(x = x, y = y, fill = cover_cat),   show.legend = FALSE) +
  scale_fill_manual(values = c("#A66A38", "#D9A066", "#E3D081", "#7AAB50", "#2D6320"),
                    name = "Vegetation Cover") +
  coord_quickmap() +
  theme_minimal()
g

png(filename = "./figures/r2012wc_df_landcover.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# to plot empty grid, convert raster to df, then plot df
# already converted to df, so can just plot

# plot raster as grid, using df version of raster
g <- ggplot(r2012wc_df2) +
  aes(x=x, y=y,fill=NA) +
  geom_tile(colour="grey10", fill="white", show.legend = FALSE) +
  #scale_fill_terrain_c(na.value = "transparent") +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012wc_df_landcover_gridlines.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# zoom in to upper left corner

g <- ggplot(r2012wc_df2) +
  aes(x=x, y=y,fill="") +
  geom_tile(colour="grey10", fill="white", show.legend = FALSE) +
  #scale_fill_terrain_c(na.value = "transparent") +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal() +
  geom_magnify(
    from = c(-2.5, -2.3, 10.2, 10.4), # coordinates of area to magnify (xmin, xmax, ymin, ymax)
    to = c(-2, 1, 5, 8), # coordinates of area to place inset
    colour = "red"
  )
g

png(filename = "./figures/r2012wc_df_landcover_gridlines_magnify.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()



# mask: takes the cropped object and now sets all values to NA if outside the shp obj
# that is, this turns this into a raster of ghana
r2012gm <- terra::mask(r2012wc, shp.sf)
plot(r2012gm)


# check dimensions
dim(r2012gm)
# 772 x 535
# same as cropped file, so the dimensions still include the NA cells


# aggregate grids to 10km cells so that they are more visible
# this means aggregating cells by a factor of 20
r2012gm_10k <- aggregate(r2012gm, fact = 20, fun = mean, na.rm = TRUE)
# if plot, can see that cells are larger, so resolution looks worse (pixelated)
plot(r2012gm_10k)

# plot using tidyterra pkg
g <- ggplot() +
  geom_spatraster(data = r2012gm_10k) +
  scale_fill_terrain_c(na.value = "transparent") +
  theme_minimal()

png(filename = "./figures/r2012gm_10k_rast_terrain.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()


# double to cell size 20km
r2012gm_20k <- aggregate(r2012gm, fact = 40, fun = mean, na.rm = TRUE)
plot(r2012gm_20k)

g <- ggplot() +
  geom_spatraster(data = r2012gm_20k) +
  scale_fill_terrain_c(na.value = "transparent") +
  theme_minimal()

png(filename = "./figures/r2012gm_20k_rast_terrain.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()



# to plot empty grid, convert raster to df, then plot df

#convert raster to df
r2012gm_10k.df <- as.data.frame(r2012gm_10k, xy=TRUE)
str(r2012gm_10k.df)

# change numeric name in col 3 to alphanum so it can be passed to ggplot
colnames(r2012gm_10k.df)[3] <- c("veg")

# plot raster as grid, using df version of raster
g <- ggplot(r2012gm_10k.df) +
  aes(x=x, y=y,fill=veg) +
  geom_tile(colour="grey10", show.legend = FALSE) +
  scale_fill_terrain_c(na.value = "transparent") +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012gm_df_terrain.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# plot raster as grid, using df version of raster
g <- ggplot(r2012gm_10k.df) +
  aes(x=x, y=y,fill="") +
  geom_tile(colour="grey10", fill="white", show.legend = FALSE) +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012gm_df_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# large 20k grid cells, too

#convert raster to df
r2012gm_20k.df <- as.data.frame(r2012gm_20k, xy=TRUE)
str(r2012gm_20k.df)

# change numeric name in col 3 to alphanum so it can be passed to ggplot
colnames(r2012gm_20k.df)[3] <- c("veg")

# plot raster as grid, using df version of raster
g <- ggplot(r2012gm_20k.df) +
  aes(x=x, y=y,fill=veg) +
  geom_tile(colour="grey10", show.legend = FALSE) +
  scale_fill_terrain_c(na.value = "transparent") +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012gm_20k_df_terrain.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# plot raster as grid, using df version of raster
g <- ggplot(r2012gm_20k.df) +
  aes(x=x, y=y,fill="") +
  geom_tile(colour="grey10", fill="white", show.legend = FALSE) +
  #scale_fill_gradientn(colours = terrain.colors(100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012gm_20k_df_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()



# can also rasterize the shp object to add variables to raster cells
# first, convert shp to spatvector
shp_sv <- vect(shp)

#rasterize shp by unit ID
shp.raster <- terra::rasterize(shp_sv, r2012gm_10k, 
                                  field = c("DIST_2008"),
                                  fun = min,   # note: using min here to avoid changing any IDs
                                  #fun = mean#,
                                  background=NA
)

# convert to df
# convert to df
shp.raster.df <- as.data.frame(shp.raster, xy=TRUE)
names(shp.raster.df)
str(shp.raster.df)

# merge data
shp.raster.df2 <- merge(shp.raster.df, shp, by="DIST_2008")

# plot any variable from original shapefile, but will now show as raster

g <- ggplot(shp.raster.df2) +
  aes(x=x, y=y,fill=p_share) +
  geom_tile(colour="grey10"#, show.legend = FALSE
            ) +
  scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  coord_fixed() + # keeps cells square 
  theme_minimal()
g

png(filename = "./figures/r2012gm_df_viridis.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()


##############################################
# hexagonal grid

# change shp to metric crs first
shp.sf_metric <- st_transform(shp.sf, crs = 32630) # this is code for Ghana, UTM Zone 30N: EPSG: 32630
                                                  # UTM is good for country or regional projections
                                                  # metric, so also good for converting grids with metric options
hex <- st_make_grid(shp.sf_metric$geometry,
                        cellsize = 20000, ## meters; equiv to 5k
                        what = 'polygons',
                        square = FALSE # if FALSE, then hexagons; if true, then squares
) |>                      ## |> is the base piping command, equivalent to %>% from magrittr 
  st_as_sf()

hex2 <- hex[c(unlist(st_contains(shp.sf_metric, hex)), 
                        unlist(st_overlaps(shp.sf_metric, hex))) ,]
plot(hex2)

str(hex2)
dim(hex2)

# pass id var to hex2 using st_join

hex3 <- st_join(hex2, shp.sf_metric, join = st_within)
str(hex3) # all vars available

# pass using areal weighted interpolation
hex3 <- st_interpolate_aw(
  shp.sf_metric["p_share"], 
  to = hex2, 
  extensive = FALSE # FALSE for rates; TRUE for counts
)

plot(hex3$geometry)

# plot empty hexagons

g <- ggplot(hex3) +
  geom_sf(fill=NA, colour="grey10", show.legend = FALSE
  ) +
  #scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_hex20_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()


# plot same version of p_share as above

g <- ggplot(hex3) +
  geom_sf(aes(fill=p_share), #colour="grey10"#, 
          show.legend = FALSE
  ) +
  scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_hex20_p_share.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

#######################
# re-do with larger hexagons

hex_40 <- st_make_grid(shp.sf_metric$geometry,
                    cellsize = 40000, ## meters
                    what = 'polygons',
                    square = FALSE # if FALSE, then hexagons; if true, then squares
) |>
  st_as_sf()

#hex2_40 <- st_intersection(hex_40, shp.sf_metric) # intersection does not work
hex2_40 <- hex_40[c(unlist(st_contains(shp.sf_metric, hex_40)), 
              unlist(st_overlaps(shp.sf_metric, hex_40))) ,]
plot(hex2_40$x)

str(hex2_40)
dim(hex2_40)
# pass id var to hex2 using st_join

hex3_40 <- st_join(hex2_40, shp.sf_metric, join = st_within)
str(hex3_40) # all vars available

# pass using areal weighted interpolation
hex3_40 <- st_interpolate_aw(
  shp.sf_metric["p_share"], 
  to = hex2_40, 
  extensive = FALSE # FALSE for rates; TRUE for counts
)

plot(hex3_40$geometry)

# plot empty hexagons

g <- ggplot(hex3_40) +
  geom_sf(fill=NA, colour="grey10", show.legend = FALSE
  ) +
  #scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_hex40_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()


# plot same version of p_share as above

g <- ggplot(hex3_40) +
  geom_sf(aes(fill=p_share), #colour="grey10"#, 
          show.legend = FALSE
  ) +
  scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_hex40_p_share.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

######################
# re-do now with quad grid using st_make_grid

quad20 <- st_make_grid(shp.sf_metric$geometry,
                       cellsize = 20000, ## meters
                       what = 'polygons',
                       square = TRUE # if FALSE, then hexagons; if true, then squares
) |>
  st_as_sf()

#hex2_40 <- st_intersection(hex_40, shp.sf_metric) # intersection does not work
quad20_2 <- quad20[c(unlist(st_contains(shp.sf_metric, quad20)), 
                    unlist(st_overlaps(shp.sf_metric, quad20))) ,]
plot(quad20_2$x)
str(quad20_2)
dim(quad20_2)

# pass id var to quad20 using st_join

#quad20_3 <- st_join(quad20_2, shp.sf_metric, join = st_intersects)
# similar results as st_interpolate, but joins all vars at once, and does not seem to weight vars

# pass using areal weighted interpolation; use this since weighted
quad20_3 <- st_interpolate_aw(
  shp.sf_metric["p_share"], 
  to = quad20_2, 
  extensive = FALSE # FALSE for rates; TRUE for counts
)
plot(quad20_3['p_share'])
plot(quad20_3$geometry)

# plot empty grid

g <- ggplot(quad20_3) +
  geom_sf(fill=NA, colour="grey10", show.legend = FALSE
  ) +
  #scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_quad20_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# plot similar  version of p_share as above

g <- ggplot(quad20_3) +
  geom_sf(aes(fill=p_share), #colour="grey10"#, 
          show.legend = FALSE
  ) +
  scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_quad20_p_share.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()


#### now larger quad grid

quad40 <- st_make_grid(shp.sf_metric$geometry,
                       cellsize = 40000, ## meters
                       what = 'polygons',
                       square = TRUE # if FALSE, then hexagons; if true, then squares
) |>
  st_as_sf()

#hex2_40 <- st_intersection(hex_40, shp.sf_metric) # intersection does not work
quad40_2 <- quad40[c(unlist(st_contains(shp.sf_metric, quad40)), 
                     unlist(st_overlaps(shp.sf_metric, quad40))) ,]
plot(quad40_2$x)
str(quad40_2)
dim(quad40_2)

# pass id var to quad40 using st_join

#quad40_3 <- st_join(quad40_2, shp.sf_metric, join = st_intersects)
# similar results as st_interpolate, but joins all vars at once, and does not seem to weight vars

# pass using areal weighted interpolation; use this since weighted
quad40_3 <- st_interpolate_aw(
  shp.sf_metric["p_share"], 
  to = quad40_2, 
  extensive = FALSE # FALSE for rates; TRUE for counts
)
plot(quad40_3['p_share'])
plot(quad40_3$geometry)

# plot empty grid

g <- ggplot(quad40_3) +
  geom_sf(fill=NA, colour="grey10", show.legend = FALSE
  ) +
  #scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_quad40_empty.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()

# plot similar  version of p_share as above

g <- ggplot(quad40_3) +
  geom_sf(aes(fill=p_share), #colour="grey10"#, 
          show.legend = FALSE
  ) +
  scale_fill_gradientn(colours = map.pal("viridis", 100)) +  # could also use topo_colors(100), map.pal("viridis", 100), etc.
  #theme(legend.position = "none") +
  theme_minimal()
g

png(filename = "./figures/stgrid_quad40_p_share.png", width = 6, height = 6, units = "in", res = 300)
print(g)
dev.off()



# save new shapefiles with projection file

#shp <- as_Spatial(shp.sf)
writeOGR(shp, dsn="./data/working", layer="ghana_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#lakes <- as_Spatial(lakes.sf)
writeOGR(lakes, dsn="./data/working", layer="lakes_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#roads <- as_Spatial(roads.sf)
writeOGR(roads, dsn="./data/working", layer="roads_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

#electricity <- as_Spatial(electricity.sf)
writeOGR(electricity, dsn="./data/working", layer="electricity_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

writeOGR(capitals, dsn="./data/working", layer="capitals_working",
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

# same writing of objects, 
# but now with sf package

# e.g.
st_write(shp.sf, dsn="./data/working", layer="ghana_working",
         driver = "ESRI Shapefile", append=FALSE) # append=FALSE will overwrite layer

# save working data

save.image(paste("./data/working/working_", Sys.Date(), "_processing.RData", sep=""))

#end
