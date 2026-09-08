################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2026-07-26
# steps here: generate connectivity matrix (W) and graph
#
################################################################################

##########################################################
# if returning to project and want to run this file from scratch, load last working data file:
# e.g.
load("./data/working/working20230626_processing.RData")

# if returning to file and want all output from this file in memory:
load("./data/working/working20260617_W.RData")

##############################################
# assign unit IDs by region

shp.sf2 <- shp.sf %>%
  group_by(REGION) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

################################################
# basic map for visual reference
################################################

g <- ggplot(data = shp.sf) +
  geom_sf(fill = NA, color="grey20") +
  geom_sf_text(aes(label = (as.numeric(row.names(shp.sf)) + 1)), size = 1.5, color = "black") +
  theme_void()
g

png(file="./figures/districts_rowID_unshaded.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# shade first 10 obs
temp <- shp.sf
temp$conditional_fill <- ifelse(row.names(temp) %in% c("0", "1", "2", "3", "4", "5", "6",
                                                       "7", "8", "9"), 1, NA)

g <- ggplot(data = temp) +
  geom_sf(fill = temp$conditional_fill, color="grey20", alpha=0.2) +
  scale_fill_viridis_c(na.value = "transparent") + # Or "white"
  geom_sf_text(aes(label = (as.numeric(row.names(temp)) + 1)), size = 1.5, color = "black") +
  theme_void()
g

png(file="./figures/districts_rowID_shaded_1-10.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

################################################
# Generate measures of connectivity
################################################

# Two main approaches based on: (1) contiguity and (2) distance

# note: this is a KEY STEP
# caution: if going to be estimating direct, indirect, and total effects, then 
# note that row-standardizing W restricts estimates of average total effects
# this may not be a reasonable approach (see Whitten, Williams, and Wimpy 2021, p149)
# here, authors row-standardized so we also row-standardize;
# we also generate other Ws for illustration purposes, and for option to check
# robustness later across different Ws

# a lot of theory can go into constructing W
# will return to this later

#####################################
# contiguity-based 
#####################################

# generate neighbor list objects
nb.q1 <- poly2nb(shp.sf)   # default is queen=TRUE
nb.r1 <- poly2nb(shp.sf, queen=FALSE)   # default is queen=TRUE
# inspect and note structure
nb.q1
nb.r1
# same connectivity structure, i.e., no difference between them in this shp
# can confirm with diffnb
diffnb(nb.q1, nb.r1)
# generates nb object with unmatched connections
# here, 0, so all connections matched

str(nb.q1)
head(nb.q1)
# e.g., unit 1 has 3 neighbors: 125, 128, and 129

#################################
# distance-based nbs
#################################

# needs coordinates object
centroids <- st_centroid(st_geometry(shp.sf), of_largest_polygon = TRUE)
# could use spdep centroid commands, but that generates sfc_POINT object
coords <- st_coordinates(     # st_coordinates() generates matrix with 2 columns
  centroids)
# if SpatialPolygons object:
#coords <- data.frame(coordinates(shp))  # note first entry has row.names=0

# convert to data.frame
coords <- as.data.frame(coords)
colnames(coords) <- c("x", "y")

str(coords)
head(coords)
tail(coords)

# k # of nearest neighbors (knn) connectivity
# single nearest neighbor
k1 <- knn2nb(knearneigh(coords))
# convert to listw (also check later)
wk1 <- nb2listw(k1, style="B", zero.policy=T)  # basic binary coding

# 5 nearest neighbors (k=5)
k5 <- knn2nb(knearneigh(coords, k=5))
# note this is a neighbor list (nb) object, just like nb.q1
# change to listw
wk5 <- nb2listw(k5, style="B", zero.policy=T)  # basic binary coding

# distanec based alternatives:

# any neighbors within specificed distance
# identify neighbors within this distance; since CRS is UTM 30N here, units are meters
# this may vary depending on your CRS
nbs_dist50k <- dnearneigh(coords, d1 = 0, d2 = 50000) # find all neighbors (nbs) witin 50km
#alternative to this, is to set high distance limit to the max dist between any two units
#all.linked <- max(unlist(nbdists(k5, coords))) # max distance between any 2 units
#dist.nb.0.all <- dnearneigh(coords, 0, all.linked, row.names=row.names(shp.sf))

# create distance W based on nbs
w_dist75k <- nb2listw(nbs_dist50k, style = "W", zero.policy = TRUE)

# repeat for 75km
nbs_dist75k <- dnearneigh(coords, d1 = 0, d2 = 75000) # find all neighbors (nbs) witin 75km
w_dist75k <- nb2listw(nbs_dist75k, style = "W", zero.policy = TRUE)


# inverse distance weights (IDW)
# start with nbs within specified distance, e.g., nbs_dist50k
w_idw50k <- nb2listwdist(nbs_dist50k, as(centroids, "Spatial"), type = "idw", style = "W", zero.policy = TRUE)

# option: include only k nearest neighbors (knn)
distances <- nbdists(k5, coords)
# IDW for this object
inv_distances <- lapply(distances, function(x) 1 / x)
# convert to w
w_idw_k5 <- nb2listw(k5, glist = inv_distances, style = "W")


#####################################################
# basic visualization of connectivity

# using queen-1 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main="contiguity-based nb, rook-1")
plot(nb.r1, coords=coords, col="red", add=TRUE)

# using queen-1 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main="contiguity-based nb, queen-1")
plot(nb.q1, coords=coords, col="red", add=TRUE)

# using k1 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, k=", attributes(k1)$`knn-k`, sep=""))
plot(k1, coords=coords, col="red", add=TRUE)

# using k5 W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, k=", attributes(k5)$`knn-k`, sep=""))
plot(k5, coords=coords, col="red", add=TRUE)

# using distance (dnn) W
plot(st_geometry(shp.sf), border="grey", reset=FALSE, 
     main=paste("distance-based nb, 0-", round(all.linked,3), sep=""))
plot(dist.nb.0.all, coords=coords, col="red", add=TRUE)

########################################################
#### Convert neighbor list to spatial weights matrix (W), 

matq1 <- nb2mat(nb.q1, style="W", zero.policy=T) # row-standardized
matq1b <- nb2mat(nb.q1, style="B", zero.policy=T) # basic binary coding

matr1 <- nb2mat(nb.r1, style="W", zero.policy=T) # row-standardized
matr1b <- nb2mat(nb.r1, style="B", zero.policy=T) # basic binary coding

# full matrix
matq1b

# save to latex
print(xtable(matq1b), 
      file="./tables/matq1b.tex",
      include.rownames = FALSE, 
      include.colnames = FALSE, 
      floating = FALSE, 
      hline.after = NULL,
      comment = FALSE)

# upper left 10 obs
matq1b[c(1:10), c(1:10)]
# edit to make row names range from 1-10 rather than 0-9
row.names(matq1b) <- as.numeric(row.names(matq1b))+1

# save to latex
print(xtable(matq1b[c(1:10), c(1:10)], digits=0), 
      file="./tables/matq1b_1-10_rownames.tex",
      include.rownames = TRUE, 
      include.colnames = TRUE, 
      floating = FALSE, 
      hline.after = NULL,
      comment = FALSE)

# save to latex with no row or col names

print(xtable(matq1b[c(1:10), c(1:10)], digits=0), 
      file="./tables/matq1b_1-10_norownames.tex",
      include.rownames = FALSE, 
      include.colnames = FALSE, 
      floating = FALSE, 
      hline.after = NULL,
      comment = FALSE)

# upper triangle
matq1b[upper.tri(matq1b, diag = FALSE)]

# row-standardized version for obs 1-10
print(xtable(matq1[c(1:10), c(1:10)], digits=2), # round to include decimal spaces
      file="./tables/matq1_1-10_norownames.tex",
      include.rownames = FALSE, 
      include.colnames = FALSE, 
      floating = FALSE, 
      hline.after = NULL,
      comment = FALSE)

#########################################################
#### Convert neighbor list to spatial listw objects (needed for spatial models)

# many functions in R require listw objects

# queen-1
wq1 <- nb2listw(nb.q1, style="W", zero.policy=T)  # row-standardized
wq1b <- nb2listw(nb.q1, style="B", zero.policy=T)  # basic binary coding
# rook-1
wr1 <- nb2listw(nb.r1, style="W", zero.policy=T)  # row-standardized
wr1b <- nb2listw(nb.r1, style="B", zero.policy=T)  # basic binary coding

# check structure
wq1b
wr1b
# confirm same connections
str(wq1b)
str(wr1b)


##################################################3
# quick lagDV (Wy)

lagy <- lag.listw(wq1b, shp.sf$Count_)
# first 10 obs
lagy[1:10]

#################################
# nicer maps of connectivity
# note: we'll return to some of these graph depictions of W later in discussion
# of connections between spatial and network analysis

# using queen-1

# create graph object from matrix of list object
g <- graph.adjacency(listw2mat(wq1b))
# generate edgelist
edges <- as.data.frame(get.edgelist(g))

# match coordinates of start and end nodes for each edge
head(coords)
tail(coords)
head(edges)
tail(edges)
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
# if coords from spdep: coords$id <- as.numeric(rownames(coords))+1
str(coords)
summary(coords) # range of id is 1:170
str(edges)
summary(edges) # range of both startnode and endnode is 1:170
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
str(edgecoords1)
head(edgecoords1)
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
head(edgecoords2)

# because merged by "endnode", endnode now appears in column 1, 
# and columns 3-4 correspond to coords for endnode, and cols 5-6 to startnode
# need to sort and reorder
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
head(edgecoords2)
# reorder
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
head(edgecoords2)

colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                                "xend", "yend")
head(edgecoords2)

# graph q1 connectivity with ggplot

g1 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g1

png(file="./figures/map_wq1.png", height=6, width=6, units="in", res=300)
print(g1)
dev.off()

# shorter, uncommented version with r1 W

g <- graph.adjacency(listw2mat(wr1b)) # using binary W to get unweighted presence of edges
edges <- as.data.frame(get.edgelist(g))
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                           "xend", "yend")

g2 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g2

png(file="./figures/map_wr1.png", height=6, width=6, units="in", res=300)
print(g2)
dev.off()

# shorter, uncommented version with k1 W

g <- graph.adjacency(listw2mat(wk1))
edges <- as.data.frame(get.edgelist(g))
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                           "xend", "yend")

g2 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g2

png(file="./figures/map_wk1.png", height=6, width=6, units="in", res=300)
print(g2)
dev.off()

# shorter, uncommented version with k1 W

g <- graph.adjacency(listw2mat(wk5))
edges <- as.data.frame(get.edgelist(g))
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                           "xend", "yend")

g2 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g2

png(file="./figures/map_wk5.png", height=6, width=6, units="in", res=300)
print(g2)
dev.off()

# shorter, uncommented version with dist W

g <- graph.adjacency(listw2mat(wdist)) # using binary W to get unweighted presence of edges
edges <- as.data.frame(get.edgelist(g))
colnames(edges) <- c("startnode", "endnode")
coords$id <- as.numeric(rownames(coords))
edgecoords1 <- merge(edges, coords, by.x="startnode", by.y="id")
edgecoords2 <- merge(edgecoords1, coords, by.x="endnode", by.y="id")
edgecoords2 <- edgecoords2[order(edgecoords2$startnode), ]
edgecoords2 <- edgecoords2[,c(2,1,
                              3,4,
                              5,6)]
colnames(edgecoords2) <- c("startnode", "endnode", "xstart", "ystart",
                           "xend", "yend")

g2 <- ggplot(data=shp.sf) +
  geom_sf(aes(), colour="grey30", fill=NA) +
  geom_point(data=coords[,1:2], aes(x=x, y=y), inherit.aes=FALSE) +
  geom_segment(data=edgecoords2, aes(x=xstart, y=ystart, 
                                     xend=xend, yend=yend,
                                     colour="red"), 
               inherit.aes=FALSE, show.legend = FALSE) +
  theme(axis.text = element_text(family = 'Cairo')) +  # Cairo enable degree symbol
  theme_minimal()
g2

png(file="./figures/map_wdist.png", height=6, width=6, units="in", res=300)
print(g2)
dev.off()


# w_dist50k
# basic plot
plot(w_dist50k, coords)
# nicer plot
network_sf <- as(nb2lines(w_dist50k$neighbours, 
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, color = "black", linewidth = 0.5) +  # better than geom_segment
  geom_point(data = centroids_df, aes(x = x, y = y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
g

png(file="./figures/map_w_dist50k.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# w_dist75k
# basic plot
plot(w_dist50k, coords)
# nicer plot
network_sf <- as(nb2lines(w_dist75k$neighbours, 
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, color = "black", linewidth = 0.5) +  # better than geom_segment
  geom_point(data = centroids_df, aes(x = x, y = y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
g

png(file="./figures/map_w_dist75k.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# w_idw50k
# basic plot
plot(w_idw50k, coords)
# nicer plot
network_sf <- as(nb2lines(w_idw50k$neighbours, wts=w_idw50k$weights,
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, aes(color = network_sf$wt), linewidth = 0.5,
          show.legend=FALSE) +  # better than geom_segment
  scale_color_viridis_c(direction=-1, name="weight") +
  #scale_color_gradient(low="gray90", high="black", name="weight") +
  geom_point(data = centroids_df, aes(x = x, y = y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
g

png(file="./figures/map_w_idw50k.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# w_idw_k5
# basic plot
plot(w_idw_k5, coords)
# nicer plot
network_sf <- as(nb2lines(w_idw_k5$neighbours, , wts=w_idw_k5$weights,
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, aes(color = network_sf$wt), linewidth = 0.5,
          show.legend = FALSE) +  # better than geom_segment
  scale_color_viridis_c(direction=-1, name="weight") +
  #scale_color_gradient(low="gray90", high="black", name="weight") +
  geom_point(data = centroids_df, aes(x = x, y = y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
g

png(file="./figures/map_w_idw_k5.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# plot wq1 and wk1 side by side in one combined graph

grid.arrange(g1, g2, ncol=2)

png(file="./figures/map_wq1-wk1.png", height=6, width=6, units="in", res=300)
grid.arrange(g1, g2, ncol=2)
dev.off()

# remove large graph objects
rm(g1, g2)

###############################################################################
###############################################################################
# W based on line network (e.g., roads); convert network relations to spatial W

p_load(spNetwork, tidygraph, dplyr)

str(capitals.sf)
str(centroids)
str(roads.sf)
str(electricity.sf)

# spNetwork and as_sfnetwork requires line network to be LINESTRING (not MULTILINESTRING or other)
roads.sf_clean <- st_cast(roads.sf, "LINESTRING")
electricity.sf_clean <- st_cast(electricity.sf, "LINESTRING")
roadnet <- as_sfnetwork(roads.sf_clean, directed=FALSE)
elecnet <- as_sfnetwork(electricity.sf_clean, directed=FALSE)
str(roadnet)
# also requires points to be single part points

# don't use capitals data here because incomplete for 2008; only 110 capital cities in World Bank
# data from 2017
# use centroids instead

centroids <- st_centroid(shp.sf[1]) # keeps polygon ID and geometry
centroids.sf_clean <- st_cast(centroids, "POINT")
str(centroids.sf_clean)
coords <- st_coordinates(centroids.sf_clean)

plot(roadnet)
# quick, nicer plot
g <- autoplot(roadnet) + theme_minimal()

png(file="./figures/roadnet.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

g <- autoplot(elecnet) + theme_minimal()

png(file="./figures/elecnet.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# now calculate distance matrix 

# with spNetwork, all done in one step: snap points to lines, calc dist matrix, and convert to W

tic("dist matrix")
w_roadnet <- network_listw(
  origins       = centroids.sf_clean,        
  lines        = roads.sf_clean,   
  method       = "ends",           
  mindist      = 1000, # 1km               
  maxdistance  = 50000,     # 100km        
  dist_func    = "inverse",        
  matrice_type = "W",              
  verbose      = FALSE
)
toc()
# 10 secs on 20260726
# this is a small dataset and small road net; with large data, this could take time

# check structure
str(w_roadnet)
dim(w_roadnet)
plot(w_roadnet, coords)

# see neighbor list (first 15)
w_roadnet$neighbours[1:15]

# get edge list
roadnet_edges <- as.data.frame(listw2lines(w_roadnet, coords))
roadnet_edges # full edge list of 296 edges, matching summary of w_roadnet

# bigger max distance, 75km
tic("dist matrix")
w_roadnet2 <- network_listw(
  origins       = centroids.sf_clean,        
  lines        = roads.sf_clean,   
  method       = "ends",           
  mindist      = 1000, # 1km               
  maxdistance  = 75000,     # 75km        
  dist_func    = "inverse",        
  matrice_type = "W",              
  verbose      = FALSE
)
toc()
# 10 secs on 20260726
plot(w_roadnet2, coords)



#######################################################################
# nicer graphs with ggplot; need to convert listw to line/network object first

network_sf <- as(nb2lines(w_roadnet$neighbours, 
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, color = "black", linewidth = 0.5) +  # better than geom_segment
  geom_point(data = centroids_df, aes(x = X, y = Y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()
g
png(file="./figures/map_w_roadnet_50k.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# 75k

network_sf <- as(nb2lines(w_roadnet2$neighbours, 
                          coords = coords), "sf")
network_sf <- st_set_crs(network_sf, st_crs(shp.sf))
centroids_df <- as.data.frame(coords)

g <- ggplot() +
  geom_sf(data = shp.sf, fill = NA, color = "gray70") +
  geom_sf(data = network_sf, color = "black", linewidth = 0.5) +  # better than geom_segment
  geom_point(data = centroids_df, aes(x = X, y = Y), color = "grey30", size = 1) +
  labs(x=NULL, y=NULL) +
  theme_minimal()

png(file="./figures/map_w_roadnet_75k.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

######################################################
# can also try with pkg sfnetworks
# though W here does not match W with spNetork pkg
# spNetwork pkg natively generates listw object, so prefer that one
dist_matrix <- st_network_cost(
  roadnet, 
  from = centroids.sf_clean, 
  to = centroids.sf_clean#, 
  #direction = "all" # Change to "out" or "in" for directed networks
)

str(dist_matrix)
# drop units
library(units)
dist_matrix <- drop_units(dist_matrix)

# set max distance; here same as 50k above
max_dist <- 50000
neighbor_matrix <- dist_matrix

# if dist > max_dist, set to 0
neighbor_matrix[neighbor_matrix > max_dist] <- 0

# inv distance mat
weights_matrix <- 1 / neighbor_matrix
weights_matrix[is.infinite(weights_matrix) | neighbor_matrix == 0] <- 0

w_roadnet_sfnet <- mat2listw(weights_matrix, zero.policy = TRUE, style = "W")

plot(w_roadnet_sfnet, coords)
w_roadnet_sfnet
# different than using spNetwork; fewer links;
# use spNetwork because natively converts to listw

####################################################################################
# simple illustrations of spatial dependence using q1w

# create sim bbox
bbox <- st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10), crs = st_crs(3857))

# create an artificial grid, 10x10
grid_temp <- st_make_grid(bbox, cellsize = 1, n = c(10, 10))
grid_temp_sf <- st_sf(geometry = grid_temp)
# check
plot(grid_temp_sf) # good

# create a q1 W
neighbors <- poly2nb(grid_temp_sf, queen = TRUE)
weights_list <- nb2listw(neighbors, style = "W")
W_matrix <- as(listw2mat(weights_list), "Matrix")

# define quantities to solve (I - rho*W)_{-1}*e with positive autocorrelation
rho <- .95 # range: -1 to 1
n_cells <- nrow(grid_temp_sf)
identity_matrix <- Diagonal(n_cells)
error_temp <- rnorm(n_cells, mean = 0, sd = 1)

# solve
grid_temp_sf$sim <- as.vector(solve(identity_matrix - rho * W_matrix) %*% error_temp)

summary(grid_temp_sf$sim)

# graph
g <- ggplot(data=grid_temp_sf) +
  geom_sf(aes(fill=sim)) +
  #scale_fill_viridis_c() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0)+
  labs(title = "rho = .9") +
  theme_void()

png(file="./figures/autocorrelation_sim_positive.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# define quantities to solve (I - rho*W)_{-1}*e with negative autocorrelation
rho <- -.95 # range: -1 to 1
n_cells <- nrow(grid_temp_sf)
identity_matrix <- Diagonal(n_cells)
#error_temp <- rnorm(n_cells, mean = 0, sd = 1)

# solve
grid_temp_sf$sim <- as.vector(solve(identity_matrix - rho * W_matrix) %*% error_temp)

summary(grid_temp_sf$sim)

# graph
g <- ggplot(data=grid_temp_sf) +
  geom_sf(aes(fill=sim)) +
  #scale_fill_viridis_c() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0)+
  labs(title = "rho = -.95") +
  theme_void()

png(file="./figures/autocorrelation_sim_negative.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# define quantities to solve (I - rho*W)_{-1}*e with no autocorrelation
rho <- 0 # range: -1 to 1
n_cells <- nrow(grid_temp_sf)
identity_matrix <- Diagonal(n_cells)
error_temp <- rnorm(n_cells, mean = 0, sd = 1)

# use lambda*W*error
grid_temp_sf$sim <- as.vector(solve(identity_matrix - rho * W_matrix) %*% error_temp)

summary(grid_temp_sf$sim)

# graph
g <- ggplot(data=grid_temp_sf) +
  geom_sf(aes(fill=sim)) +
  #scale_fill_viridis_c() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, guide="none")+
  labs(title = "rho = 0") +
  theme_void()

png(file="./figures/autocorrelation_sim_positive.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

####################################
# save working data

save.image(paste("./data/working/working_", Sys.Date(), "_W.RData", sep=""))

# end
