################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2023-06-26
# steps here: exploratory analysis; global moran, local c, and LISA
#
################################################################################

##########################################################
# if returning to project, load last working data file:
# e.g.,
load("./data/working/working20230626_W.RData")


#############################################
# Exploratory Analysis
#############################################

# Two approaches covered here: global and local
# global can help detect general spatial clustering; 
# this can be informative for descriptive and exploratory purposes, and can
# also help motivate modeling strategies different from standard OLS

# however, in practice, likely want to move quickly to local statistics
# these are much more informative in terms of detecting local
# patterns of association
# keep in mind that these local patterns could be product of either
# (a) spatial dependence or (b) heterogeneity (uneven effects of X on Y)


###########################################
# Global Moran'S I 

# global moran of outcome
my1 <- moran.test(shp.sf$Count_, listw=wq1b)
my1
# spatial dependence present: I = 0.304 and is sig at p<.001
# specifically, positive spatial dependence

# global moran of key predictors
mx1 <- moran.test(shp$p_share, listw=wq1b)
mx2 <- moran.test(shp$p_tuvol, listw=wq1b)
mx3 <- moran.test(shp$Density_RD, listw=wq1b)
mx4 <- moran.test(shp$grid_perCa, listw=wq1b)

# build table with global Is and p-values

globalIs <- as.data.frame(rbind(cbind(round(my1$estimate[[1]], 3), #"<.001"), #round(
                                      my1$p.value),
                                cbind(round(mx1$estimate[[1]], 3), #"<.001"), #round(
                                      mx1$p.value),
                                cbind(round(mx2$estimate[[1]], 3), #"<.001"), #round(
                                      mx2$p.value),
                                cbind(round(mx3$estimate[[1]], 3), #"<.001"), #round(
                                      mx3$p.value),
                                cbind(round(mx4$estimate[[1]], 3), #"<.001"), #round(
                                      mx4$p.value)))

globalIs$Variable <- c("solar", "vote share", "turnover volatility", 
                       "road density", "grid percap")
# reorder columns
globalIs <- globalIs[, c(3,1,2)]

# names
colnames(globalIs) <- c("Variable", "I", "p")

str(globalIs)

knitr::kable(globalIs, format="simple", digits=10)
# only shows digist as far as 10, even for sci notation

# can output to file
xtable(globalIs)

print(xtable(globalIs), 
      file="./tables/table_MoransI.tex", 
      floating=FALSE,
      type="latex", digits=4,
      include.rownames=FALSE,
      display=c("s","f","f"))


##########################################################
#
# Local Indicators of Spatial Association (LISAs)
# Local Moran, Local G, and Local C

# Summary:

# Local Moran

# returns value that can be positive or negative
# positive value indicate similarity cluster
# negative value indicates dissimilarity cluster
# Also: these values can be classified into 5 categories:
# similar values clusters: high-high and low-low
# dissimilar values clusters: low-high and high-low
# or not significant

# localG (getis-ord); returns Z-value that identifies similarity clusters 
# high G (positive) = high-value similarity cluster
# low G (negative) = low-value similarity cluster
# note: does not capture dissimilarity clusters or spatial "outliers"
# note2: two types: G and G*; G excludes focal unit in calculating 
# neighborhood average; G* includes focal unit

# localC (Geary's C); 
# can capture similarity and dissimilarity
# based on squared differences, so large values 
# (large, squared differences), capture dissimilarity
# mean=1 if spatially random; values < 1 are similar-values clusters
# small values = similarity clusters (low and high, 
#     based on matching observations on a moran scatter plot) 
# large values (>1) = dissimilarity clusters
# however, squared differences may also 
# match observations close to mean (around 1), so
# these are "other positive" associations

# in practice, can do more than 1 to check stability of results
# Local Moran and Geary C two most informative options
# because they identify both similarity and dissimilarity clusters

##########################################################

# y = solar panels per district ("Count_")

nb <- nb.q1
w <- nb2listw(nb, style="W", zero.policy=T)  # row-standardized

#########################


###############################################
#
# Local Moran
#
# note: for Local Moran, cover two estimation strategies: 
# permutation and saddlepoint
# other estimation strategies available, but permutation is more intuitive 
# and saddlepoint is more conservative
# Tiefelsdorf (2002, 204) calls saddlepoint "approximation method of first choice"
# Bivand and Wong (2018) also favor saddlepoint, and note that permutation 
# approach is less conservative (i.e., more likely to generate large Z-values, or
# false positive, i.e., "false discovery" of local clusters)

#################################################

# permutation approach
lisa_perm <- localmoran_perm(shp.sf$Count_, listw=w, nsim=9999)
summary(lisa_perm)

# question: what is this doing?
# permutation = shuffling

# create LISA cluster identifiers
DV <- shp$Count_
quadrant <- vector(mode="numeric",length=nrow(lisa_perm))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(wq1b, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with permutation method
p <- lisa_perm[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_perm))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4
# non-significant obs will remain coded as zeroes (0)
table(quadrant)

# merge LISA of DV data into shapefile
# merge a single variable from table
shp$lisa_perm.dv <- lisa_perm[,1]
shp$lisa_perm.p.dv <- p
shp$lisa_perm.cl.dv <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_perm.dv <- lisa_perm[,1]
shp.sf$lisa_perm.p.dv <- p
shp.sf$lisa_perm.cl.dv <- as.factor(quadrant)

#########################################################
#
# color version of moran scatterplot
#
#########################################################

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, 
                meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="Solar panels (centered on mean)", 
       y="spatial lag of solar panels (centered on mean)", 
       title="LISA clusters, y (permutation)") +
  geom_smooth(method = "lm", se=FALSE, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  theme_minimal()
  
  #theme(axis.line=element_line(color="black"),
  #      axis.title.x=element_text(size=10,vjust=0.1),
  #      axis.title.y=element_text(size=10,vjust=0.1),
  #      axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
  #      #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
  #      legend.title = element_text(size = 8), # legend title size
  #      legend.text = element_text(size = 8), # legend label size
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
  #      panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
  #      panel.spacing=unit(c(0,0,0,0), "lines"),
  #      plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_yavg_perm_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_perm.cl.dv)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="LISA clusters, y (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_yavg_perm_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

###################################################
# if wanted to, could add satellite background
# review methods from command file on mapping

load("./data/original/region4.RData")

g <- ggmap(region4) +
  geom_sf(data=shp.sf, aes(fill=lisa_perm.cl.dv), inherit.aes = FALSE) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black", inherit.aes = FALSE) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

# remove large object
rm(region4)

##################################
# saddlepoint approach
##################################

# in permutation approach, shuffling values and generating distributions for
# comparison
# with small samples, can lead to false positives
# saddlepoint estimation offers alternative solution; more accurate for smaller samples,
# more conservative, so less likely to generate false positives

## first, simple visualization of saddlepoint
# imagine horse saddle
# trying to optimize two functions --- minimization in one, maximization in other
# i.e., minmax solution

## a simulated, simple illustration of a saddlepoint

# create sim data
x <- seq(-2, 2, length.out = 50)
y <- seq(-2, 2, length.out = 50)

# define the saddle function z = x^2 - y^2
f <- function(x, y) { x^2 - y^2 }
z <- outer(x, y, f)

png(file="./figures/saddlepoint_illustration.png", 
    height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
# 3. Create the 3D surface plot
saddle_plot <- persp(x, y, z, theta = 35, phi = 20, col = "white", # main ="title,
                     box =TRUE,  # FALSE remove box frame
                     axes = TRUE, 
                     border= NA, shade=0.6, # removes mesh, adds shading
                     xlab = "x", ylab = "y", zlab = "z")

# 4. Add a red point at the saddle coordinate (0,0,0)
points(trans3d(0, 0, 0, pmat = saddle_plot), col = "black", pch = 19, cex = 1.5)
dev.off()

# using rgl package
install.packages("rgl")
library(rgl)

persp3d(x, y, z, color = "grey")
points3d(0, 0, 0, color = "red", size = 10)


#######################################################
# now saddlepoint of outcome of interest

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(Count_ ~ 1, shp.sf), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
DV <- shp$Count_
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(wq1b, DV)
clagDV <- lagDV - mean(lagDV)

p <- lisa_s[,5]  # saddlepoint p-value (Pr. (Sad))
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4
# non-significant obs will remain coded as zeroes (0)
table(quadrant)

# merge LISA of DV data into shapefile
# merge a single variable from table
shp$lisa_s.dv <- lisa_s[,4]
shp$lisa_s.p.dv <- p
shp$lisa_s.cl.dv <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.dv <- lisa_s[,4]
shp.sf$lisa_s.p.dv <- p
shp.sf$lisa_s.cl.dv <- as.factor(quadrant)

# using temp
temp <- shp.sf
temp$lisa_s.dv <- lisa_s[,4]
temp$lisa_s.p.dv <- p
temp$lisa_s.cl.dv <- as.factor(quadrant)

# table
localmoran_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, lisa_s.p.dv, lisa_s.cl.dv) |>
  arrange(lisa_s.p.dv)
names(localmoran_tab) <- c("district", "p", "cluster")
localmoran_tab[1:5,]

print.xtable(xtable(localmoran_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localmoran_sad_lowest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)


localmoran_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, lisa_s.p.dv, lisa_s.cl.dv) |>
  arrange(desc(lisa_s.p.dv)) #  descending
names(localmoran_tab) <- c("district", "p", "cluster")
localmoran_tab[1:5,]

print.xtable(xtable(localmoran_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localmoran_sad_highest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)

#########################################################
#
# color version of moran scatterplot
#
#########################################################

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="", y="", title="LISA clusters, y (saddlepoint)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("Solar panels (centered on mean)") + ylab("spatial lag of solar panels (centered on mean)")+
  theme_minimal()
  #theme(axis.line=element_line(color="black"),
  #      axis.title.x=element_text(size=10,vjust=0.1),
  #      axis.title.y=element_text(size=10,vjust=0.1),
  #      axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
  #      #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
  #      legend.title = element_text(size = 8), # legend title size
  #      legend.text = element_text(size = 8), # legend label size
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
  #      panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
  #      panel.spacing=unit(c(0,0,0,0), "lines"),
  #      plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_yavg_sad_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_s.cl.dv)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, Solar Panels (saddlepoint)") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_yavg_sad_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()



##############################################
# could use same process, but now for key Xs, e.g., p_share
##############################################


##############################################
# LISAs for key X, p_share
##############################
# skip permutation approach; can do on your own based on template above

##############################
# saddlepoint approach

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(p_share ~ 1, shp.sf), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
DV <- shp.sf$p_share
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(wq1b, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with saddlepoint method
p <- lisa_s[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4

table(quadrant)

# merge LISA of X data into shapefile
# merge a single variable from table
shp$lisa_s.x1 <- lisa_s[,4]
shp$lisa_s.p.x1 <- p
shp$lisa_s.cl.x1 <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.x1 <- lisa_s[,4]
shp.sf$lisa_s.p.x1 <- p
shp.sf$lisa_s.cl.x1 <- as.factor(quadrant)

# moran scatterplot of p_share

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="", y="", title="LISA clusters, vote share (p_share)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("Vote share (centered on mean)") + ylab("spatial lag of vote share (centered on mean)")+
  theme_minimal()
  #theme(axis.line=element_line(color="black"),
  #      axis.title.x=element_text(size=10,vjust=0.1),
  #      axis.title.y=element_text(size=10,vjust=0.1),
  #      axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
  #      #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
  #      legend.title = element_text(size = 8), # legend title size
  #      legend.text = element_text(size = 8), # legend label size
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
  #      panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
  #      panel.spacing=unit(c(0,0,0,0), "lines"),
  #      plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_pshare_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#########################
# LISA MAP
#########################

g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=lisa_s.cl.x1)) +
  scale_fill_manual(name="LISA cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="LISA Clusters, Vote Share") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_pshare_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


##############################################
# LISAs for key X, literacy
##############################
# skip permutation approach; can do on your own based on template above

##############################
# saddlepoint approach

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(literacy ~ 1, shp.sf), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
DV <- shp.sf$literacy
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(wq1b, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with saddlepoint method
p <- lisa_s[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4

table(quadrant)

# merge LISA of X data into shapefile
# merge a single variable from table
shp$lisa_s.x1 <- lisa_s[,4]
shp$lisa_s.p.x1 <- p
shp$lisa_s.cl.x1 <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.x1 <- lisa_s[,4]
shp.sf$lisa_s.p.x1 <- p
shp.sf$lisa_s.cl.x1 <- as.factor(quadrant)

# using temp
temp <- shp.sf
temp$lisa_x <- lisa_s[,4]
temp$lisa_s.p.x <- p
temp$lisa_s.cl.x <- as.factor(quadrant)


# moran scatterplot of p_share

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="", y="", title="LISA clusters, literacy") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("literacy (centered on mean)") + ylab("spatial lag of literacy (centered on mean)")+
  theme_minimal()
  #theme(axis.line=element_line(color="black"),
  #      axis.title.x=element_text(size=10,vjust=0.1),
  #      axis.title.y=element_text(size=10,vjust=0.1),
  #      axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
  #      #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
  #      legend.title = element_text(size = 8), # legend title size
  #      legend.text = element_text(size = 8), # legend label size
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
  #      panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
  #      panel.spacing=unit(c(0,0,0,0), "lines"),
  #      plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_literacy_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#########################
# LISA MAP
#########################

g <- ggplot(data=temp) +
  geom_sf(aes(fill=lisa_s.cl.x)) +
  scale_fill_manual(name="LISA cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="LISA Clusters, literacy") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_literacy_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


##############################################
# LISAs for key X, pop density
##############################
# skip permutation approach; can do on your own based on template above

##############################
# saddlepoint approach

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(Pop_Densit ~ 1, shp.sf), 
                         nb=nb, style="C"))) # 

# create LISA cluster identifiers
DV <- shp.sf$literacy
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
cDV <- DV - mean(DV) 
lagDV <- lag.listw(wq1b, DV)
clagDV <- lagDV - mean(lagDV)

# LISA significance with saddlepoint method
p <- lisa_s[,5]
quadrant <- vector(mode="numeric",length=nrow(lisa_s))
quadrant[cDV >0 & clagDV>0 & p<=.05] <- 1 
quadrant[cDV <0 & clagDV<0 & p<=.05] <- 2      
quadrant[cDV <0 & clagDV>0 & p<=.05] <- 3
quadrant[cDV >0 & clagDV<0 & p<=.05] <- 4

table(quadrant)

# merge LISA of X data into shapefile
# merge a single variable from table
shp$lisa_s.x1 <- lisa_s[,4]
shp$lisa_s.p.x1 <- p
shp$lisa_s.cl.x1 <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.x1 <- lisa_s[,4]
shp.sf$lisa_s.p.x1 <- p
shp.sf$lisa_s.cl.x1 <- as.factor(quadrant)

# using temp
temp <- shp.sf
temp$lisa_x <- lisa_s[,4]
temp$lisa_s.p.x <- p
temp$lisa_s.cl.x <- as.factor(quadrant)


# moran scatterplot of p_share

df = data.frame(y = cDV, Wy = clagDV, cl=quadrant, sig=p, meany=mean(cDV), meanWy=mean(clagDV))

g <- ggplot(df, aes(x = y, y = Wy))+
  geom_point(colour="black", pch=21, size=3,
             aes(fill = factor(cl))) +
  scale_fill_manual(name = "Cluster",
                    values = c("0" = "white",
                               "1" = "red",
                               "2" = "blue",
                               "3" = "lightblue",
                               "4" = "pink"),
                    labels = c("n.s.", "high-high", "low-low", "low-high", "high-low")) +
  labs(x="", y="", title="LISA clusters, population density") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("population density (centered on mean)") + ylab("spatial lag of population density (centered on mean)")+
  theme_minimal()
  #theme(axis.line=element_line(color="black"),
  #      axis.title.x=element_text(size=10,vjust=0.1),
  #      axis.title.y=element_text(size=10,vjust=0.1),
  #      axis.text= element_text(colour="black", size=10, angle=0,face = "plain"),
  #      #plot.title = element_text(size = 10, lineheight=.8, face="bold", vjust=1), # make title bold and add space
  #      legend.title = element_text(size = 8), # legend title size
  #      legend.text = element_text(size = 8), # legend label size
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # removes background grid
  #      panel.background = element_blank(), # removes grey background; could add black axis lines with #axis.line = element_line(colour = "black")) 
  #      panel.spacing=unit(c(0,0,0,0), "lines"),
  #      plot.margin=unit(c(0,0,0,0), "mm"))  # sets margin around full plot at top, right, bottom, and left; units can also be "lines" or "cm"
g

png(file="./figures/moranplot_popdensity_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#########################
# LISA MAP
#########################

g <- ggplot(data=temp) +
  geom_sf(aes(fill=lisa_s.cl.x)) +
  scale_fill_manual(name="LISA cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="LISA Clusters, population density") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_popdensity_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

##########################################
##########################################
#
# Alternatives to local Moran:
# Local G (Getis-Ord statistic) and Local Geary or Geary's C
# 
##########################################
##########################################

# Reminder:
# localG (getis-ord); returns G and Z-values that identifies similarity clusters 
# use standardized z scores
# high z (positive) = high-value similarity cluster
# low z (negative) = low-value similarity cluster
# note: does not capture dissimilarity clusters or spatial "outliers"
# note2: two types: G and G*; G excludes focal unit in calculating 
# neighborhood average; G* includes focal unit

localg <- localG(shp$Count_, listw=wq1b)
# 
# graph with ggplot
temp <- shp.sf
str(temp)
temp$localg_z <- as.vector(localg)
temp$localg_cluster <- cut(temp$localg_z,
                            breaks = c(-Inf, -2.58, -1.96, 1.96, 2.58, Inf),
                            labels = c("low-low (99%)", "low-low (95%)", "n.s.", 
                                       "high-high (95%)", "high-high (99%)"),
                            include.lowest = TRUE
)

# define the color palette
localg_colors <- c(
  "low-low (99%)" = "darkblue",
  "low-low (95%)" = "lightblue",
  "n.s."    = "white",
  "high-high (95%)"  = "red",
  "high-high (99%)"  = "darkred"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localg_cluster), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localg_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, base"
  #) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    #axis.text = element_blank(),
    legend.position = "right"
  )

png(file="./figures/localg_spdep_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

# could also just map z scores

g <- ggplot() +
  geom_sf(data=temp, aes(fill = localg_z), color = "black", size = 0.1) +
  scale_fill_viridis_c(#direction=-1,
    name = "z score"
  ) +
  #labs(
  #  title = "Local G, base"
  #) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    #axis.text = element_blank(),
    legend.position = "right"
  )

png(file="./figures/localg_z_spdep_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


# table
localg_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localg_z, localg_cluster) |>
  arrange(localg_z)
localg_tab[1:5,]
names(localg_tab) <- c("district", "z-score", "cluster")

print.xtable(xtable(localg_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localg_spdep_base_lowest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)


localg_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localg_z, localg_cluster) |>
  arrange(desc(localg_z)) # descending
localg_tab[1:5,]
names(localg_tab) <- c("district", "z-score", "cluster")

print.xtable(xtable(localg_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localg_spdep_base_highest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)

summary(localg_tab)
hist(localg_tab$localg_z)
boxplot(localg_tab$localg_z)

# spdep permutation approach
localg_perm <- localG_perm(shp$Count_, listw=wq1b, nsim=4999)
head(localg)
hist(localg) # same distribution as localG above
boxplot(localg) # same distribution as localG above
str(localg)
attributes(localg)
attributes(localg)$cluster
attributes(localg)$internals[,4]
# note: use localGS() for G*

temp$localg_z_perm <- as.vector(localg)
temp$localg_cluster_perm <- cut(temp$localg_z_perm,
                           breaks = c(-Inf, -2.58, -1.96, 1.96, 2.58, Inf),
                           labels = c("low-low (99%)", "low-low (95%)", "n.s.", 
                                      "high-high (95%)", "high-high (99%)"),
                           include.lowest = TRUE
)

# define the color palette
localg_colors <- c(
  "low-low (99%)" = "darkblue",
  "low-low (95%)" = "lightblue",
  "n.s."    = "white",
  "high-high (95%)"  = "red",
  "high-high (99%)"  = "darkred"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localg_cluster_perm), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localg_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    #axis.text = element_blank(),
    legend.position = "right"
  )

png(file="./figures/localg_spdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

# could also just map z scores

g <- ggplot() +
  geom_sf(data=temp, aes(fill = localg_z_perm), color = "black", size = 0.1) +
  scale_fill_viridis_c(#direction=-1,
    name = "z score"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    #axis.text = element_blank(),
    legend.position = "right"
  )

png(file="./figures/localg_z_spdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


# if want local G*
localgs <- spdep::localGS(shp$Count_, listw=wq1b)

#######################################################
# using rgeoda
#x <- mlf:: get_var(shp.sf$Count_)   # get squared variance
tempW <- rgeoda::queen_weights(shp.sf)   # note that rgeoda limits options for W
alpha=0.05
localg_2 <- rgeoda::local_g(tempW, 
                                data.frame(shp.sf$Count_), 
                                permutations = 4999)
localg_2


localgs_2 <- rgeoda::local_gstar(tempW, 
                            data.frame(shp.sf$Count_), 
                            permutations = 4999)


# graphing
clusters <- rgeoda::lisa_clusters(localg_2, cutoff = alpha)
labels <- rgeoda::lisa_labels(localg_2)
pvalue <- rgeoda::lisa_pvalues(localg_2)
colors <- rgeoda::lisa_colors(localg_2)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localg_2_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localg_2_clusters", labels = labels, 
                           #palette = pal,
                           palette = colors,
                           style = "cat",
                           lwd=0,
                           border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local G Cluster Map", legend.outside = TRUE)
# creates map with border and formatted title/labels/legend

# ggplot
table(clusters)
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localg_2_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue", "grey"), 
                    breaks = c("0", "1", "2", "4"), 
                    labels=c("n.s.", "high-high", "low-low", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local G, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localgmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#######################
# if want local G*
localgstar <- rgeoda::local_gstar(tempW, 
                                  data.frame(shp.sf$Count_), 
                                  permutations = 4999)



clusters <- rgeoda::lisa_clusters(localgstar, cutoff = alpha)
labels <- rgeoda::lisa_labels(localgstar)
pvalue <- rgeoda::lisa_pvalues(localgstar)
colors <- rgeoda::lisa_colors(localgstar)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localgstar_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localgstar_clusters", labels = labels, 
                           #palette = pal,
                           palette = colors,
                           style = "cat",
                           lwd=0,
                           border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local G* Cluster Map", legend.outside = TRUE)
# creates map with border and formatted title/labels/legend

# ggplot
table(clusters)
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localgstar_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue", "grey"), 
                    breaks = c("0", "1", "2", "4"), 
                    labels=c("n.s.", "high-high", "low-low", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local G*, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localgstarmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


###################################################
# using sfdep

#using sfdep
localg_3 <- sfdep::local_g(shp$Count_, nb=wq1b$neighbours, wt=wq1b$weights)
localg_3_perm <- sfdep::local_g_perm(shp$Count_, nb=wq1b$neighbours, wt=wq1b$weights,
                                     nsim=4999)

str(localg_3_perm) # data.frame, unlike spdep
head(localg_3_perm)
hist(localg_3_perm$gi) # 
summary(localg_3_perm$gi) # 
boxplot(localg_3_perm$p_folded_sim) # 
hist(localg_3_perm$p_folded_sim) # 

temp$localg3 <- localc_3_perm$gi
temp$localg3_psim_perm <- localg_3_perm$p_sim
temp$localg3_pfoldedsim_perm <- localg_3_perm$p_folded_sim
temp$localg3_cluster_perm <- localg_3_perm$cluster
str(temp$localg3_cluster_perm)
# Factor w/ 4 levels "High-High","Low-Low",..: 1 3 4 3 2 3 2 3 2 3 ...
head(temp$localg3_cluster_perm)
# [1] High-High      Other Positive Negative       Other Positive Low-Low        Other Positive
#Levels: High-High Low-Low Other Positive Negative
table(temp$localg3_cluster_perm)

temp$localg3_cluster_perm <- as.character(temp$localg3_cluster_perm)
temp$localg3_cluster_perm_sim <- as.character(temp$localg3_cluster_perm)
temp$localg3_cluster_perm_foldedsim <- as.character(temp$localg3_cluster_perm)

temp$localg3_cluster_perm[temp$localg3_p_perm>0.05] <- "n.s."
temp$localg3_cluster_perm_sim[temp$localg3_psim_perm>0.05] <- "n.s."
temp$localg3_cluster_perm_foldedsim[temp$localg3_pfoldedsim_perm>0.05] <- "n.s."

table(temp$localg3_cluster_perm)
table(temp$localg3_cluster_perm_sim)
table(temp$localg3_cluster_perm_foldedsim)


# define the color palette
localg_colors <- c(
  "High" = "red",
  "Low" = "blue",
  "n.s."    = "white"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localg3_cluster_perm_foldedsim), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localg_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() 

png(file="./figures/localg_sfdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

# don't map z-values because these are analytic based on normal distribution, 
# which does not work with small samples for local stats

# table
localg_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localg3_pfoldedsim_perm, localg3_cluster_perm_foldedsim) |>
  arrange(localg3_pfoldedsim_perm)
names(localg_tab) <- c("district", "p (folded sim)", "cluster")
localg_tab[1:5,]

print.xtable(xtable(localg_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localg_sfdep_perm_lowest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)


localg_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localg3_pfoldedsim_perm, localg3_cluster_perm_foldedsim) |>
  arrange(desc(localg3_pfoldedsim_perm)) # descending
names(localg_tab) <- c("district", "p (folded sim)", "cluster")
localg_tab[1:5,]

print.xtable(xtable(localg_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localg_sfdep_perm_highest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)

summary(localg_tab)
hist(localg_tab$localg_p_perm)
boxplot(localg_tab$localg_p_perm)


# if want local G* with sfdep:
localgs_3_perm <- sfdep::local_gstar_perm(shp$Count_, nb=wq1b$neighbours, wt=wq1b$weights,
                                          nsim=4999)

str(localgs_3_perm) # data.frame, unlike spdep
head(localgs_3_perm)
hist(localgs_3_perm$gi) # 
summary(localgs_3_perm$gi) # 
boxplot(localgs_3_perm$p_folded_sim) # 
hist(localgs_3_perm$p_folded_sim) # 

temp$localgs3 <- localc_3_perm$gi
temp$localgs3_psim_perm <- localgs_3_perm$p_sim
temp$localgs3_pfoldedsim_perm <- localgs_3_perm$p_folded_sim
temp$localgs3_cluster_perm <- localgs_3_perm$cluster
str(temp$localgs3_cluster_perm)
# Factor w/ 4 levels "High-High","Low-Low",..: 1 3 4 3 2 3 2 3 2 3 ...
head(temp$localgs3_cluster_perm)
# [1] High-High      Other Positive Negative       Other Positive Low-Low        Other Positive
#Levels: High-High Low-Low Other Positive Negative
table(temp$localgs3_cluster_perm)

temp$localgs3_cluster_perm <- as.character(temp$localgs3_cluster_perm)
temp$localgs3_cluster_perm_sim <- as.character(temp$localgs3_cluster_perm)
temp$localgs3_cluster_perm_foldedsim <- as.character(temp$localgs3_cluster_perm)

temp$localgs3_cluster_perm[temp$localgs3_p_perm>0.05] <- "n.s."
temp$localgs3_cluster_perm_sim[temp$localgs3_psim_perm>0.05] <- "n.s."
temp$localgs3_cluster_perm_foldedsim[temp$localgs3_pfoldedsim_perm>0.05] <- "n.s."

table(temp$localgs3_cluster_perm)
table(temp$localgs3_cluster_perm_sim)
table(temp$localgs3_cluster_perm_foldedsim)


# define the color palette
localgs_colors <- c(
  "High" = "red",
  "Low" = "blue",
  "n.s."    = "white"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localgs3_cluster_perm_foldedsim), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localgs_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() 

png(file="./figures/localgs_sfdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


##########################################################################
##########################################################################
# localC (Geary's C); 
# Reminder:
# can capture similarity and dissimilarity
# based on squared differences, so large values 
# (large, squared differences), capture dissimilarity
# mean=1 if spatially random; values < 1 are similar-values clusters
# small values = similarity clusters (low and high, 
#     based on matching observations on a moran scatter plot) 
# large values (>1) = dissimilarity clusters
# however, squared differences may also 
# match observations close to mean (around 1), so
# these are "other positive" associations

localc <- localC(shp$Count_, listw=wq1b)
localc
# only has value, the squared difference; no p-value or z-score
# this is not very informative

# spdep permutation approach generates z-scores and pseudo-p
localc_perm <- localC_perm(shp$Count_, listw=wq1b, nsim=4999)
head(localc_perm)
hist(localc_perm) # same distribution as localG above
boxplot(localc_perm) # same distribution as localG above
str(localc_perm)
attributes(localc_perm)
attributes(localc_perm)$cluster
attributes(localc_perm)$`pseudo-p`[,3] # z-scores

temp$localc_z_perm <- attributes(localc_perm)$`pseudo-p`[,3]
temp$localc_p_perm <- attributes(localc_perm)$`pseudo-p`[,4]
temp$localc_psim_perm <- attributes(localc_perm)$`pseudo-p`[,5]
temp$localc_pfoldedsim_perm <- attributes(localc_perm)$`pseudo-p`[,6]

temp$localc_cluster_perm <- attributes(localc_perm)$cluster
str(temp$localc_cluster_perm)
# Factor w/ 4 levels "High-High","Low-Low",..: 1 3 4 3 2 3 2 3 2 3 ...
head(temp$localc_cluster_perm)
# [1] High-High      Other Positive Negative       Other Positive Low-Low        Other Positive
#Levels: High-High Low-Low Other Positive Negative
table(temp$localc_cluster_perm)

temp$localc_cluster_perm <- as.character(temp$localc_cluster_perm)
temp$localc_cluster_perm_sim <- as.character(temp$localc_cluster_perm)
temp$localc_cluster_perm_foldedsim <- as.character(temp$localc_cluster_perm)

# assign n.s.
temp$localc_cluster_perm[temp$localc_p_perm>0.05] <- "n.s."
temp$localc_cluster_perm_sim[temp$localc_psim_perm>0.05] <- "n.s."
temp$localc_cluster_perm_foldedsim[temp$localc_pfoldedsim_perm>0.05] <- "n.s."


table(temp$localc_cluster_perm)
table(temp$localc_cluster_perm_sim)
table(temp$localc_cluster_perm_foldedsim)


# define the color palette
localc_colors <- c(
  "High-High" = "red",
  "Low-Low" = "blue",
  "n.s."    = "white",
  "Other Positive"  = "yellow",
  "Negative"  = "lightblue"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localc_cluster_perm_foldedsim), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localc_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    #axis.text = element_blank(),
    legend.position = "right"
  )

png(file="./figures/localc_spdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


# table
localc_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localc_pfoldedsim_perm, localc_cluster_perm_foldedsim) |>
  arrange(localc_pfoldedsim_perm)
localc_tab[1:5,]
names(localc_tab) <- c("district", "p (folded sim)", "cluster")

print.xtable(xtable(localc_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localc_spdep_perm_lowest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)


localc_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localc_pfoldedsim_perm, localc_cluster_perm_foldedsim) |>
  arrange(desc(localc_pfoldedsim_perm)) # descending
localc_tab[1:5,]
names(localc_tab) <- c("district", "p (folded sim)", "cluster")

print.xtable(xtable(localc_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localc_spdep_perm_highest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)

summary(localc_tab)
hist(localc_tab$localc_p_perm)
boxplot(localc_tab$localc_p_perm)


# using rgeoda
#x <- mlf:: get_var(shp.sf$Count_)   # get squared variance
tempW <- rgeoda::queen_weights(shp.sf)
alpha=0.05
localc_2 <- rgeoda::local_geary(tempW, 
                                data.frame(shp.sf$Count_), 
                                permutations = 4999)
localc_2

clusters <- rgeoda::lisa_clusters(localc_2, cutoff = alpha)
labels <- rgeoda::lisa_labels(localc_2)
pvalue <- rgeoda::lisa_pvalues(localc_2)
colors <- rgeoda::lisa_colors(localc_2)
lisa_patterns <- labels[clusters + 1]
table(clusters)
#pal <- match_palette(lisa_patterns, labels, colors)
labels <- labels[labels %in% lisa_patterns]
shp.sf["localc_2_clusters"] <- clusters
#plot
tm_shape(shp.sf) + tm_fill("localc_2_clusters", labels = labels, 
                          #palette = pal,
                          palette = colors,
                          style = "cat",
                          lwd=0,
                          border.alpha=0) +
  tm_borders(col=NA, lwd=1) +
  tm_layout("Local Geary C Cluster Map", legend.outside = TRUE)

# ggplot
g <- ggplot(data=shp.sf) +
  geom_sf(aes(fill=factor(localc_2_clusters))) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","yellow","lightblue", "grey"), 
                    breaks = c("0", "1", "2", "3", "4", "6"), 
                    labels=c("n.s.", "high-high", "low-low","other positive",
                             "negative", "isolated"), guide="legend") + 
  labs(x="", y="", title="Local Geary C, Solar Panels (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="black") +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/localcmap_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

###################################
# using sfdep

localc_3 <- sfdep::local_c(shp$Count_, nb=wq1b$neighbours, wt=wq1b$weights)
localc_3
# just like spdep's localC, this only has value, the squared difference; no p-value or z-score
# this is not very informative

# sfdep permutation approach generates z-scores and pseudo-p
localc_3_perm <- sfdep::local_c_perm(shp$Count_, nb=wq1b$neighbours, wt=wq1b$weights,
                                    nsim=4999)
str(localc_3_perm) # data.frame, unlike spdep's localC_perm
head(localc_3_perm)
hist(localc_3_perm$ci) # 
summary(localc_3_perm$ci) # 
hist(localc_3_perm$z_ci) # 
boxplot(localc_3_perm$p_folded_sim) # 
hist(localc_3_perm$p_folded_sim) # 

temp$localc3 <- localc_3_perm$ci
temp$localc3_z_perm <- localc_3_perm$z_ci
temp$localc3_p_perm <- localc_3_perm$p_ci
temp$localc3_psim_perm <- localc_3_perm$p_ci_sim
temp$localc3_pfoldedsim_perm <- localc_3_perm$p_folded_sim
temp$localc3_cluster_perm <- localc_3_perm$cluster
str(temp$localc3_cluster_perm)
# Factor w/ 4 levels "High-High","Low-Low",..: 1 3 4 3 2 3 2 3 2 3 ...
head(temp$localc3_cluster_perm)
# [1] High-High      Other Positive Negative       Other Positive Low-Low        Other Positive
#Levels: High-High Low-Low Other Positive Negative
table(temp$localc3_cluster_perm)

temp$localc3_cluster_perm <- as.character(temp$localc3_cluster_perm)
temp$localc3_cluster_perm_sim <- as.character(temp$localc3_cluster_perm)
temp$localc3_cluster_perm_foldedsim <- as.character(temp$localc3_cluster_perm)

temp$localc3_cluster_perm[temp$localc3_p_perm>0.05] <- "n.s."
temp$localc3_cluster_perm_sim[temp$localc3_psim_perm>0.05] <- "n.s."
temp$localc3_cluster_perm_foldedsim[temp$localc3_pfoldedsim_perm>0.05] <- "n.s."

table(temp$localc3_cluster_perm)
table(temp$localc3_cluster_perm_sim)
table(temp$localc3_cluster_perm_foldedsim)


# define the color palette
localc_colors <- c(
  "High-High" = "red",
  "Low-Low" = "blue",
  "n.s."    = "white",
  "Other Positive"  = "yellow",
  "Negative"  = "lightblue"
)

# map
g <- ggplot() +
  geom_sf(data=temp, aes(fill = localc3_cluster_perm_foldedsim), color = "black", size = 0.1) +
  scale_fill_manual(
    values = localc_colors,
    name = "cluster"
  ) +
  #labs(
  #  title = "Local G, permutation"
  #) +
  theme_minimal() 

png(file="./figures/localc_sfdep_perm_map_y_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

# don't map z-values because these are analytic based on normal distribution, 
# which does not work with small samples for local stats

# table
localc_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localc_pfoldedsim_perm, localc_cluster_perm_foldedsim) |>
  arrange(localc_pfoldedsim_perm)
localc_tab[1:5,]

print.xtable(xtable(localc_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localc_sfdep_perm_lowest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)


localc_tab <- st_drop_geometry(temp) |>
  select(DIST_2008, localc_pfoldedsim_perm, localc_cluster_perm_foldedsim) |>
  arrange(desc(localc_pfoldedsim_perm)) # descending
localc_tab[1:5,]

print.xtable(xtable(localg_tab[1:5,], digits=2), 
             type="latex",
             file="./tables/localc_sfdep_perm_highest5.tex", 
             floating=FALSE,
             include.rownames = FALSE
)

summary(localc_tab)
hist(localc_tab$localc_p_perm)
boxplot(localc_tab$localc_p_perm)


# overall, prefer Local Moran
# -- get both similarity and dissimilarity, like local C
# -- split "negative" clusters into low-high and high-low
# more intuitive range from negative values (dissimilar) to positive values (similar), which 
# resonates with common statistics like correlation coefficients or betas

####################################
# save working data

save.image("./data/working/working20230626_exploratory.RData")

#end
