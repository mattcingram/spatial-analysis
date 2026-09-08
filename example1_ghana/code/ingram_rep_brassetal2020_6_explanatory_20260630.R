################################################################################
#
# Ingram, Matt 
# Reproduction of Brass et al. (2020) in Political Geography
# created: 2023-04-23
# last updated: 2026-06-30
# steps here: explanatory analysis; 
#             spatial models (SLM, SEM, SLX, SAC, SDM, 
#               and GWR and MGWR)
#
################################################################################

##########################################################
# if returning to project, load last working data file:
load("./data/working/working20230626_explanatory.RData")

# if returning to data already processed by this script
load("./data/working/working20260624_models.RData")

###############################################################
# EXPLANATORY ANALYSIS
###############################################################

#######################################
# BASELINE OLS MODEL (not reported in article)

# reminder that reversed % turnout (p_turn) to get % nonvoters
shp$turninv<- (1-shp$p_turn)

# note: in original model in Table 2, % nonvoters is not in model 1, 
# does not appear separately in model 2, and is not in model 3
# however, it is in replication materials shared by authors
# we include it here

# base ols
ols <- lm(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv       
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data)
summary(ols)

# save basic ols to file
stargazer(ols, out = "./tables/olsresults.tex", type="latex", 
          no.space = TRUE,
          float = FALSE, # leave out \begin{table} so can customize within tex file
          dep.var.caption = "", # omits dep var caption
          covariate.labels = c("poverty", "inequality", "female ratio", 
                               "vote share", "volatility (vote share)",
                               "turnout",
                               "volatility (turnout)",
                               "ethnic frag.",
                               "Count3",
                               "road density",
                               "pop. density",
                               "count4",
                               "literacy",
                               "grid density",
                               "constant"),
          omit.stat = "f",
          add.lines = list(c("AIC", 
                             as.integer(AIC(ols))
                             ))
)



# heteroskedasticity
bptest(ols)

# graph test
plot(ols$fitted.values, ols$residuals)
# Lieberman-style graph of yhat vs y
plot(ols$fitted.values, ols$model$Count_)
abline(ols$fitted.values, ols$model$Count_)

# residuals by region (not district) --- see if any geographic pattern
temp <- data.frame(ols$residuals, shp.sf$REGION)
colnames(temp) <- c("residual", "region")
plot(factor(temp$region), temp$residual)

# anova
anova1 <- aov(residual ~ factor(region), data = temp)
summary(anova1)
lm1 <- lm(residual ~ factor(region), data=temp)
summary(lm1)


#######################################
# BASELINE OLS MODEL WITH CLUSTERED SEs (Model 1 in article)

#Model 1
# using miceadds::lm.cluster(..., cluster = "REGION")

ols1a <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data, cluster = "REGION")

# can also use estimatr::lm_robust() (with option ... clusters = REGION)

ols1b <- lm_robust(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share
                   + p_shvol + turninv
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp@data,   # need to specify data object with lm_robust
                   clusters = REGION,
                   se_type = "CR2"   # options are CR0, CR2 (default), or stata
                   )

summary(ols1a)   
summary(ols1b)


# dot plot of coefficients
dwplot(list(ols1a$lm_res, 
            ols1b), ci=.95)

#######################################
# BASELINE OLS WITH CLUSTERED SEs and INTERACTION (Model 2 in article)

#Model 2
# with miceadds::lm.cluster()
ols2a <- lm.cluster(Count_~ pov_p_2008 + gini_2008
                   + ferat_2008 + p_share:turninv
                   + p_share + turninv
                   + p_shvol 
                   + p_tuvol + p_ethfr
                   + Count_3 + Density_RD
                   + Pop_Densit + Count_4
                   + literacy + grid_perCa,
                   data=shp, 
                   cluster = "REGION"
           )

summary(ols2a)

# with estimatr::lm_robust()
ols2b <- lm_robust(Count_~ pov_p_2008 + gini_2008
                    + ferat_2008 + p_share:turninv
                    + p_share + turninv
                    + p_shvol 
                    + p_tuvol + p_ethfr
                    + Count_3 + Density_RD
                    + Pop_Densit + Count_4
                    + literacy + grid_perCa,
                    data=shp@data,    # need to specify data with lm_robust
                    clusters = REGION  # with default se_type = "CR2"
                )

summary(ols2b)

# both of these models generate exactly same results as in article



#######################################
# SPATIAL MODELING
#######################################

# 3 main approaches:
### (1) run diagnostics, then select model
### (2) run most complex model, then evaluate and select model
### (3) build model based on theory

# in practice: 
# if have good theory: let theory guide model specification, 
# check with diagnostics,
# and, in any case, run more than one model to 
# check stability/robustness of results;
# if don't have good theory or working in new area,
# could follow options 1 or 2 in more
# exploratory approach

# here, assume have well-developed theory, and focus on diagnostics (option 1 above)

###########################
# DIAGNOSTICS

# moran's I

temp <- shp.sf
temp$ols_residuals <- ols$residuals

mres <- moran.test(temp$ols_residuals, listw=wq1b)
mres
#Moran I test under randomisation
#
#data:  temp$ols_residuals  
#weights: wq1b    
#
#Moran I statistic standard deviate = 3, p-value = 0.001
#alternative hypothesis: greater
#sample estimates:
#  Moran I statistic       Expectation          Variance 
#0.12706          -0.00592           0.00193 

# map residuals

g <- ggplot(data=temp) +
  geom_sf(aes(fill=ols_residuals)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name="res") +
  #labs(x="", y="", title="Local Moran Clusters, Solar Panels (saddlepoint)") +
  #geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/map_ols-residuals_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

# abs(residuals)
g <- ggplot(data=temp) +
  geom_sf(aes(fill=abs(ols_residuals))) +
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_fill_viridis_c(name="abs(res)") +
  #labs(x="", y="", title="OLS residuals (absolute value)") +
  #geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/map_ols-residuals-abs_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()



##############################################################
# local moran of residuals - permutation

# permutation approach
lisa_perm <- localmoran_perm(temp$ols_residuals, listw=w, nsim=9999)
summary(lisa_perm)

# question: what is this doing?
# permutation = shuffling

# create LISA cluster identifiers
DV <- temp$ols_residuals
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
shp$lisa_s.res <- lisa_s[,4]
shp$lisa_s.p.res <- p
shp$lisa_s.cl.res <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.res <- lisa_s[,4]
shp.sf$lisa_s.p.res <- p
shp.sf$lisa_s.cl.res <- as.factor(quadrant)

# using temp
temp <- shp.sf
temp$lisa_s.res <- lisa_s[,4]
temp$lisa_s.p.res <- p
temp$lisa_s.cl.res <- as.factor(quadrant)


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
  labs(x="", y="", title="LISA clusters, residuals (permutation)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("residuals (centered on mean)") + ylab("spatial lag of residuals (centered on mean)")+
  theme_minimal()
g

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

png(file="./figures/moranplot_residuals_perm_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=temp) +
  geom_sf(aes(fill=lisa_s.cl.res)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, residuals (permutation)") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_residuals_perm_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


###############################################################
# local moran of residuals - saddlepoint

lisa_s <- as.data.frame(
  summary(localmoran.sad(lm(ols_residuals ~ 1, temp), 
                         nb=nb, style="C"))) # this works; 
# need summary for saddlepoint syntax

# create LISA cluster identifiers
DV <- temp$ols_residuals
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
shp$lisa_s.res <- lisa_s[,4]
shp$lisa_s.p.res <- p
shp$lisa_s.cl.res <- as.factor(quadrant)
#names(shp)

shp.sf$lisa_s.res <- lisa_s[,4]
shp.sf$lisa_s.p.res <- p
shp.sf$lisa_s.cl.res <- as.factor(quadrant)

# using temp
temp <- shp.sf
temp$lisa_s.res <- lisa_s[,4]
temp$lisa_s.p.res <- p
temp$lisa_s.cl.res <- as.factor(quadrant)


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
  labs(x="", y="", title="LISA clusters, residuals (saddlepoint)") +
  geom_smooth(method = "lm", se=F, colour="black", linewidth=.7) + 
  geom_vline(xintercept=df$meany,colour="black",linetype="longdash") + 
  geom_hline(yintercept=df$meanWy,colour="black",linetype="longdash")+ 
  xlab("residuals (centered on mean)") + ylab("spatial lag of residuals (centered on mean)")+
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

png(file="./figures/moranplot_residuals_sad_col_cen.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()

#########################
# LISA MAP
#########################

g <- ggplot(data=temp) +
  geom_sf(aes(fill=lisa_s.cl.res)) +
  scale_fill_manual(name="cluster", 
                    values=c("white", "red", "blue","lightblue","pink"), 
                    breaks = c("0", "1", "2", "3", "4"), 
                    labels=c("n.s.", "high-high", "low-low","low-high","hgh-low"), guide="legend") + 
  labs(x="", y="", title="Local Moran Clusters, residuals (saddlepoint)") +
  geom_sf(data=lakes.sf[4,], fill="grey20", alpha=0.5) +
  #coord_sf(crs=4269) +     # applies to all layers
  #xlim(-3.5, 1.5) +
  #ylim(4.5, 11.5) +
  theme_minimal()
g

png(file="./figures/lisamap_residuals_sad_color.png", height=6, width=6, units="in", res=300) # could also do pdf, jpeg, bmp, tiff
print(g)
dev.off()


#############################################################

# classic tests: Lagrange Multiplier test (LM test)
lmtests <- lm.RStests(ols, listw=wq1, 
                      #test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA")
                      test="all")
summary(lmtests)

temptab <- summary(lmtests)
str(temptab$results)
# convert to kable object to save as tex file

tex_table <- kbl(temptab$results, format = "latex", float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/LMtests.tex")

# output: note that does not quite match original appendix (table A2, p4)
# no difference if use ols1a$lm_res
#        statistic parameter  p.value   
#LMerr     5.3531         1 0.020686 * 
#RLMerr    1.4432         1 0.229622   
#LMlag     9.0335         1 0.002651 **
#RLMlag    5.1236         1 0.023602 * 
#SARMA    10.4767         2 0.005309 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# comments:
# LMerr and LMlag both sig, so move to robust tests;
# only LMlag sig; also, mixed SARMA test sig
# this matches discussion in appendix, pp 3-4
# thus: SLM is most appropriate, but could still model SEM or SAC/SARMA to check

#################################################
# RS tests
# Add Durbin tests (new to updated spdep package)

SDtests <- SD.RStests(ols, listw=wq1, test = "all",
           Durbin = TRUE)
summary(SDtests)
temptab <- summary(SDtests)
str(temptab$results)
# convert to kable object to save as tex file
tex_table <- kbl(temptab$results, format = "latex", float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/SDtests.tex")

####################################################
# Test for GNS (3-source model), what Anselin et al. (2026) call "STGE-Pre" strategy
# if RS tests are run, and null is not rejected regarding WX, i.e., not significant,
# then "proceed with the initial estimates" and proceed with model selection, leaving WX out
# however, ``if the null is rejected [i.e., WX is sig], re-estimate as SLX model and apply classic LM tests
# to this model (which is also estimated with OLS)

# note: have to specify and run SLX model (pulling from below)

# in Brass et al, reject null for WX, so STGE-Pre approach says we should apply LM tests to slx model
# classic tests: Lagrange Multiplier test (LM test)
lmtests_slx <- lm.RStests(slx, listw=wq1, 
                          #test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA")
                          test="all")
summary(lmtests_slx)

temptab <- summary(lmtests_slx)
str(temptab$results)
# convert to kable object to save as tex file

tex_table <- kbl(temptab$results, format = "latex", float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/LMtests_SLX.tex")



# SLM: Model 3 in article

slm <- lagsarlm(Count_~ pov_p_2008 + gini_2008      # formula
                           + ferat_2008 + p_share
                           + p_shvol + turninv
                           + p_tuvol + p_ethfr
                           + Count_3 + Density_RD
                           + Pop_Densit + Count_4
                           + literacy + grid_perCa,
                data=shp,      # data
                listw=wq1)   # W
summary(slm)


# SEM 
sem <- errorsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1)
summary(sem)

# SAC/SARAR
sac <- sacsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1 #,    # W for Wy process (SLM) 
                #listw2=wq1    # W for We process (SEM); could specify different one, otherwise same as listw
                )
summary(sac)
# note rho sig, lambda not sig

# SDM
sdm <- lagsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1,
                #type="emixed",
                Durbin = TRUE)
summary(sdm)

# new SDM.RStests from Koley 2024 (diss, ch2), and Koely and Bera (2024)

SDMtests <- SD.RStests(ols, listw=wq1, test = "SDM", Durbin=TRUE)
summary(SDMtests)
str(summary(SDMtests))

tex_table <- kbl(summary(SDMtests)$results, format = "latex", float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/SDMtests.tex")


# SDEM
sdem <- errorsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1,
                etype="mixed")
summary(sdem)

# SLX
slx <- lmSLX(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
             data=shp, 
             listw=wq1,
             Durbin = TRUE)
summary(slx)



# GNS

gns <- sacsarlm(Count_~ pov_p_2008 + gini_2008
                + ferat_2008 + p_share
                + p_shvol + turninv
                + p_tuvol + p_ethfr
                + Count_3 + Density_RD
                + Pop_Densit + Count_4
                + literacy + grid_perCa,
                data=shp, 
                listw=wq1,    
                type="sacmixed"
)
summary(gns)



# could also streamline 
model_formula <- formula(Count_~ pov_p_2008 + gini_2008 + ferat_2008 + p_share + p_shvol + turninv
                         + p_tuvol + p_ethfr
                         + Count_3 + Density_RD
                         + Pop_Densit + Count_4
                         + literacy + grid_perCa)

slm <- lagsarlm(model_formula, data=shp, listw=wq1)
summary(slm)


# aside from LM tests, could now do post-estimation comparison
moran.test(ols$residuals, listw=wr1)
moran.test(ols$residuals, listw=wr1b)

moran.test(ols$residuals, listw=wq1)
moran.test(ols$residuals, listw=wq1b)

moran.test(ols$residuals, listw=wk1)
moran.test(ols$residuals, listw=wk5)

moran.test(ols$residuals, listw=w_dist50k)

moran.test(ols$residuals, listw=w_idw50k)

moran.test(ols$residuals, listw=w_roadnet)
moran.test(ols$residuals, listw=w_roadnet2)


lm.morantest(ols, listw=wq1b, alternative="two.sided")
lm.morantest.exact(ols, listw=wq1b, alternative="two.sided")
lm.morantest.sad(ols, listw=wq1b, alternative="two.sided")
# same estimated I, though p varies slightly across 3 approaches 

# now check residuals of spatial models
# robust LM test in part of the output of spatial models
# can also check with MC simulation

slm$LMtest
# [,1]
# [1,] 0.498
moran.mc.slm <- moran.mc(slm$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
slm$LL
# or
logLik(slm) # same result

sem$LMtest
# NULL
moran.mc.sem <- moran.mc(sem$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
sem$LL
logLik(sem)

LL.sac <- sac$LL
moran.mc.sac <- moran.mc(sac$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")


# slx
LL.slx <- logLik(slx)
moran.mc.slx <- moran.mc(slx$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
moran.mc.slx

sdm$LMtest
# 0.157
moran.mc.sdm <- moran.mc(sdm$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
sdm$LL
# -762

sdem$LMtest
# NULL
moran.mc.sdem <- moran.mc(sdem$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
# statistic = -0.003, observed rank = 5519, p-value = 0.4
sdem$LL
# -763


moran.mc.gns <- moran.mc(gns$residuals, listw = wq1b, nsim = 9999, alternative = "two.sided")
gns$LL

#######################################
# common factor constraint (Burridge 1981)
# based on relationship between SEM and SDM (similar to relationship between SLM and SDEM); 
# see Anselin and Rey (2014), Cook et al (2020), etc.

spatialreg::LR.Sarlm(sdm, sem)
#Likelihood ratio for spatial linear models
#
#data:  
#  Likelihood ratio = 22, df = 14, p-value = 0.08
#sample estimates:
#  Log likelihood of sdm Log likelihood of sem 
#-762                  -773 

#same as:
lmtest::lrtest(sdm, sem)

# if cannot reject null (theta = -rho*beta), then SDM reduces to SEM; go with SEM
# if reject null, then go with SDM
# here, p = 0.08; technically not below 0.05, but very close; 
# enough to go with SDM rather than ignore feedback and spillovers



#######################################

nobs <- length(slm$y)

AIC(ols)  # base ols
#AIC(ols1a$lm_res) # ols with clustered SEs (model 1)
#AIC(ols2a$lm_res) # ols with clustered SEs and interaction (model 2)
k = ols$rank
AICc_ols <- AIC(ols) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(slm)
k <- slm$parameters #17
AICc_slm <- AIC(slm) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(sem)
k <- sem$parameters #17
AICc_sem <- AIC(sem) + (2 * k * (k + 1)) / (nobs - k - 1)
BIC(ols)

AIC(sac)
k <- sac$parameters #18
AICc_sac <- AIC(sac) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(sdm)
k <- sdm$parameters #31
AICc_sdm <- AIC(sdm) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(sdem)
k <- sdem$parameters #31
AICc_sdem <- AIC(sdem) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(slx)
k <- slx$rank #29
AICc_slx <- AIC(slx) + (2 * k * (k + 1)) / (nobs - k - 1)

AIC(gns)
k <- gns$parameters #29
AICc_gns <- AIC(gns) + (2 * k * (k + 1)) / (nobs - k - 1)


AICw = 

# save to file
modelsummary(models = list(slm, sem, sac, sdm, sdem, slx),
             fmt=2, # two decimal places to save space on this big table
             coef_map = c(
               "pov_p_2008"="poverty", 
               "gini_2008"= "inequality",
               "ferat_2008"="female ratio",
               "p_share" = "NDC vote share", 
               "p_shvol" = "vote volatility",
               "turninv" = "turnout",
               "p_tuvol" = "turnout volatility",
               "p_ethfr" =  "ethnic frag.",
               "Count_3" =  "WB projects",
               "Density_RD"  = "road density",
               "Pop_Densit" = "pop. density",
               "Count_4" = "health facilities",
               "literacy"  = "literacy",
               "grid_perCa" = "grid density",
               "rho" = "rho",
               "lambda" = "lambda",
               "lag.pov_p_2008" = "l.poverty",
               "lag.gini_2008"= "l.inequality",
               "lag.ferat_2008"="l.female ratio",
               "lag.p_share" = "l.NDC vote share", 
               "lag.p_shvol" = "l.vote volatility",
               "lag.turninv" = "l.turnout",
               "lag.p_tuvol" = "l.turnout volatility",
               "lag.p_ethfr" =  "l.ethnic frag.",
               "lag.Count_3" =  "l.WB projects",
               "lag.Density_RD"  = "l.road density",
               "lag.Pop_Densit" = "l.pop. density",
               "lag.Count_4" = "l.health facilities",
               "lag.literacy"  = "l.literacy",
               "lag.grid_perCa" = "l.grid density",
               "(Intercept)" = "Constant"),
             #gof_map = c("aic", "rmse"),
             gof_map = NA,
             stars = TRUE,
             output = "./tables/spatialmodels_ols-slx.tex"
             )

modelsummary(models = list("SLM"=slm, 
                           "SEM"=sem, 
                           "SAC"=sac, 
                           "SDM"=sdm, 
                           "SDEM"=sdem, 
                           "SLX"=slx, 
                           "GNS"=gns),
             fmt=2, # two decimal places to save space on this big table
             coef_map = c(
               "pov_p_2008"="poverty", 
               "gini_2008"= "inequality",
               "ferat_2008"="female ratio",
               "p_share" = "NDC vote share", 
               "p_shvol" = "vote volatility",
               "turninv" = "turnout",
               "p_tuvol" = "turnout volatility",
               "p_ethfr" =  "ethnic frag.",
               "Count_3" =  "WB projects",
               "Density_RD"  = "road density",
               "Pop_Densit" = "pop. density",
               "Count_4" = "health facilities",
               "literacy"  = "literacy",
               "grid_perCa" = "grid density",
               "rho" = "rho",
               "lambda" = "lambda",
               "lag.pov_p_2008" = "l.poverty",
               "lag.gini_2008"= "l.inequality",
               "lag.ferat_2008"="l.female ratio",
               "lag.p_share" = "l.NDC vote share", 
               "lag.p_shvol" = "l.vote volatility",
               "lag.turninv" = "l.turnout",
               "lag.p_tuvol" = "l.turnout volatility",
               "lag.p_ethfr" =  "l.ethnic frag.",
               "lag.Count_3" =  "l.WB projects",
               "lag.Density_RD"  = "l.road density",
               "lag.Pop_Densit" = "l.pop. density",
               "lag.Count_4" = "l.health facilities",
               "lag.literacy"  = "l.literacy",
               "lag.grid_perCa" = "l.grid density",
               "(Intercept)" = "Constant"),
             #gof_map = c("aic", "rmse"),
             gof_map = NA,
             stars = TRUE,
             output = "./tables/spatialmodels_ols-gns.tex"
)


tempdf <- data.frame(
  rbind(
    c("OLS", round(AIC(ols), 2), round(BIC(ols), 2), round(summary(ols)$sigma, 2), 
      round(logLik(ols)[1], 2), #"-", 
      round(lm.morantest(ols, listw=wq1b, alternative="two.sided")$estimate[[1]], 3),
      round(lm.morantest(ols, listw=wq1b, alternative="two.sided")$p.value, 3)),
    c("SLM", round(AIC(slm), 2), round(BIC(slm), 2), round(sqrt(slm$s2), 2),
      round(slm$LL[1], 2), #round(slm$LMtest[1], 2),
      round(moran.mc.slm$statistic[[1]], 3),
      round(moran.mc.slm$p.value, 3)),
    c("SEM", round(AIC(sem), 2), round(BIC(sem), 2), round(sqrt(sem$s2), 2),
      round(sem$LL[1], 2), #"-",
      round(moran.mc.sem$statistic[[1]], 3),
      round(moran.mc.sem$p.value, 3)),
    c("SAC", round(AIC(sac), 2), round(BIC(sac), 2), round(sqrt(sac$s2), 2),
      round(sac$LL[1], 2), #"-",
      round(moran.mc.sac$statistic[[1]], 3),
      round(moran.mc.sac$p.value, 3)),
    c("SDM", round(AIC(sdm), 2), round(BIC(sdm), 2), round(sqrt(sdm$s2), 2),
      round(sdm$LL[1], 2), #round(sdm$LMtest[1], 2),
      round(moran.mc.sdm$statistic[[1]], 3),
      round(moran.mc.sdm$p.value, 3)),
    c("SDEM", round(AIC(sdem), 2), round(BIC(sdem), 2), round(sqrt(sdem$s2), 2),
      round(sdem$LL[1], 2), #"-",
      round(moran.mc.sdem$statistic[[1]], 3),
      round(moran.mc.sdem$p.value, 3)),
    c("SLX", round(AIC(slx), 2), round(BIC(slx), 2), round(summary(slx)$sigma, 2),
      round(logLik(slx)[1], 2), #"-",
      round(moran.mc.slx$statistic[[1]], 3),
      round(moran.mc.slx$p.value, 3)),
    c("GNS", round(AIC(gns), 2), round(BIC(gns), 2), round(sqrt(gns$s2), 2),
      round(gns$LL[1], 2), #"-",
      round(moran.mc.gns$statistic[[1]], 3),
      round(moran.mc.gns$p.value, 3))
  )
)


colnames(tempdf) <-     c("model", "AIC", "BIC", "RMSE", "LL", #"LM", 
                          "Moran", "p")
# rearrange columns
tempdf <- tempdf %>% relocate(model, LL, AIC, BIC, RMSE, Moran, p)
for (a in 2:7){
  tempdf[,a] <- as.numeric(tempdf[, a])
}
str(tempdf)

# if want AIC weights
AICmin <- min(tempdf$AIC)
tempdf$AICdelta <- tempdf$AIC-AICmin
tempdf$AICw <- (exp(-.5*tempdf$AICdelta))/(sum(
  (exp(-.5*tempdf$AICdelta[1])) +
    (exp(-.5*tempdf$AICdelta[2]))+
    (exp(-.5*tempdf$AICdelta[3]))+
    (exp(-.5*tempdf$AICdelta[4]))+
    (exp(-.5*tempdf$AICdelta[5]))+
    (exp(-.5*tempdf$AICdelta[6]))+
    (exp(-.5*tempdf$AICdelta[7]))+
    (exp(-.5*tempdf$AICdelta[8]))
))



tex_table <- kbl(tempdf, format = "latex", 
                 digits = 3,
                 float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/models_gof.tex")


# overall, SLM has lowest AIC and AICc, so looks reasonable
# but, SDM and SDEM have lowest RMSE

#### SORT TABLE TO RANK MODELS BASED ON EACH CRITERION

temp1 <- tempdf %>%
  arrange(desc(LL)) %>%
  select(model)

temp2 <- tempdf %>%
  arrange(AIC) %>%
  select(model)

temp3 <- tempdf %>%
  arrange(BIC) %>%
  select(model)

temp4 <- tempdf %>%
  arrange(RMSE) %>%
  select(model)

temp5 <- tempdf %>%
  arrange(Moran) %>%
  select(model)

temp6 <- tempdf %>%
  arrange(desc(p)) %>%  # descending order -- most non-significant at top
  select(model)

tempdf2 <- cbind(c(1, 2, 3, 4, 5, 6, 7, 8), 
                 temp1, temp2, temp3, temp4, temp5, temp6)
colnames(tempdf2) <- c("rank", "LL", "AIC", "BIC", "RMSE", #"LM", 
                       "Moran", "p")
tempdf2

tex_table <- kbl(tempdf2, format = "latex", 
                 digits = 0,
                 float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/models_gof_ranked.tex")


# table with average rank of each model

OLSrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="OLS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$BIC=="OLS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="OLS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="OLS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$Moran=="OLS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="OLS"])
)
)

SLMrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SLM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$BIC=="SLM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SLM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="SLM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$Moran=="SLM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="SLM"])
)
)

SEMrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$AICc=="SEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="SEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="SEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="SEM"])
)
)

SACrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SAC"]),
                     as.numeric(row.names(tempdf2)[tempdf2$AICc=="SAC"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SAC"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="SAC"]),
                     as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="SAC"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="SAC"])
)
)

SDMrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SDM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$AICc=="SDM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SDM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="SDM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="SDM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="SDM"])
)
)

SDEMrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SDEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$AICc=="SDEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SDEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="SDEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="SDEM"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="SDEM"])
)
)


SLXrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="SLX"]),
                   as.numeric(row.names(tempdf2)[tempdf2$AICc=="SLX"]),
                   as.numeric(row.names(tempdf2)[tempdf2$RMSE=="SLX"]),
                   as.numeric(row.names(tempdf2)[tempdf2$LL=="SLX"]),
                   as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="SLX"]),
                   as.numeric(row.names(tempdf2)[tempdf2$p=="SLX"])
              )
)

GNSrank.mn <- mean(c(as.numeric(row.names(tempdf2)[tempdf2$AIC=="GNS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$AICc=="GNS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$RMSE=="GNS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$LL=="GNS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$`Moran (MC)`=="GNS"]),
                     as.numeric(row.names(tempdf2)[tempdf2$p=="GNS"])
)
)

ranks_df <- as.data.frame(cbind(tempdf$model, c(
  round(OLSrank.mn, 1), 
  round(SLMrank.mn, 1), 
  round(SEMrank.mn, 1), 
  round(SACrank.mn, 1),
  round(SDMrank.mn, 1), 
  round(SDEMrank.mn, 1), 
  round(SLXrank.mn, 1), 
  round(GNSrank.mn, 1))
)
)

colnames(ranks_df) <- c("model", "rank_mean")

ranks_df <- ranks_df %>%
  arrange(rank_mean) 

tex_table <- kbl(ranks_df, format = "latex", 
                 digits = 2,
                 float=FALSE, booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

save_kable(tex_table, file = "./tables/models_gof_rank_mean.tex")


###############################################
# INTERPRETATION

# for SLM, SAC, SDM, and SLX, need to estimate impacts because of spatial multiplier
# derived from Wy process
# B = ()
# for SEM, can interpret betas as OLS coefficients

# note: original article found turnout volatility, female ratio, and road density
# significant

impacts.slm<- impacts(slm, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.slm, zstats=TRUE)
# here, we find same general results (female ratio, turnout volatility, and road
# density are sig., )
# values are not the same, but relative magnitudes are similar

# save to file:


# graph impacts




summary(sem)

impacts.sac<- impacts(sac, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.sac, zstats=TRUE)

impacts.sdm<- impacts(sdm, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.sdm, zstats=TRUE)


# partitioned; preferred for partitioned results for graphing:
w <- as(as_dgRMatrix_listw(wq1), "CsparseMatrix")
trMC <- trW(w, type = "MC")

partition_sdm <- impacts(sdm, tr=trMC, R=1000, Q=5)
#summary(partition_sdm, zstats=T, short=T, Q=5, reportQ=T) 
part_sdm.sum <- summary(partition_sdm, zstats=T, short=T, Q=5, reportQ=T) 
# save to file:
capture.output(part_sdm.sum, file = "./tables/partitionQ5_sdm1.csv")
# process for latex

pzmat <- data.frame(part_sdm.sum$pzmat)
sdmimpsum <- as.data.frame(cbind(
  c("poverty", 
    "inequality",
    "female ratio",
    "NDC vote share", 
    "vote volatility",
    "turnout",
    "turnout volatility",
    "ethnic frag.",
    "WB projects",
    "road density",
    "pop. density",
    "health facilities",
    "literacy",
    "grid density"
  ),
  partition_sdm$res$direct,
  pzmat$Direct,
  partition_sdm$res$indirect,
  pzmat$Indirect,
  partition_sdm$res$total,
  pzmat$Total)
)

colnames(sdmimpsum) <- c("Variable", "Direct", "p (dir)", 
                          "Indirect", "p (ind)", 
                          "Total", "p (tot)")
str(sdmimpsum)
summary(sdmimpsum)
sdmimpsum[, c(2:7)] <- sapply(sdmimpsum[, c(2:7)], as.numeric)
summary(sdmimpsum)

# same as:
for (a in 2:7){
  sdmimpsum[,a] <- as.numeric(sdmimpsum[,a])
}

print(xtable(sdmimpsum),
      include.rownames=FALSE,
      type="latex", 
      #display = c("d","s","f","f","f","f","f","f"),
      digits=3, 
      floating=FALSE,
      align="lcccccc",
      file="./tables/table_sdm_impsum_index.tex")


# save quartile p-values to file

print(xtable(data.frame(part_sdm.sum$Qpzmats$Direct)),
      include.rownames=TRUE,
      type="latex",
      digits=3,
      floating=FALSE,
      align="lcccccccccccccc",
      file = "./tables/table_sdm_impdir_q1-q5.tex")

print(xtable(data.frame(part_sdm.sum$Qpzmats$Indirect)),
      include.rownames=TRUE,
      type="latex",
      digits=3,
      floating=FALSE,
      align="lcccccccccccccc",
      file = "./tables/table_sdm_impindir_q1-q5.tex")

print(xtable(data.frame(part_sdm.sum$Qpzmats$Total)),
      include.rownames=TRUE,
      type="latex",
      digits=3,
      floating=FALSE,
      align="lcccccccccccccc",
      file = "./tables/table_sdm_imptot_q1-q5.tex")

# graph:

# impact graphs

# SDM

# partitioned impacts object is just a list of numbers
# convert to df for ggplot
nvar=14 # num of covariates (excluding constant)
impacts.dir.df <- data.frame(matrix(part_sdm.sum$Qdirect_sum$statistics, 
                                    nrow=nvar*5)) # num of covariates * 5 (Q5)

varnames <- c("poverty", 
              "inequality",
              "female ratio",
              "NDC vote share", 
              "vote volatility",
              "turnout",
              "turnout volatility",
              "ethnic frag.",
              "WB projects",
              "road density",
              "pop. density",
              "health facilities",
              "literacy",
              "grid density")
list.dG <- list()
list.iG <- list()
list.tG <- list()

# loop through to generate all graphs
for (i in 1:length(varnames)){
  q1 = (i*5)-4
  q5 = i*5
  d <- impacts.dir.df[q1:q5,]
  d$W <- seq(1,5)
  # base graph
  g <- ggplot(d, aes(W, group=1))
  # ribbon with line
  g <- g +
    geom_hline(yintercept=0, linetype=2) +
    geom_ribbon(aes(ymin = X1-(1.96*X2), 
                    ymax = X1+(1.96*X2)), 
                fill = "grey60", alpha= .5) +
    geom_line(aes(y = X1)) +
    labs(title=paste(varnames[i]," - direct SDM", sep=""), y="impact") +
    theme(
      plot.title = element_text(size=14),
      axis.title = element_text(size=10),
      axis.text = element_text(size=10)
    ) +
    theme_minimal()
  list.dG[[i]] <- g
  
  png(filename = paste("./figures/impacts_sdm_dir_", varnames[i], "_index.png", sep=""), height=6, width=6, units="in", res=300)
  print(g)
  dev.off()
}

##############################################################
# INDIRECTS

# partitioned impacts object is just a list of numbers
# convert to df for ggplot
nvar = 14 # number of covariates (excluding constant)
impacts.indir.df <- data.frame(matrix(part_sdm.sum$Qindirect_sum$statistics, nrow=nvar*5))

# loop through to generate all graphs
for (i in 1:length(varnames)){
  q1 = (i*5)-4
  q5 = i*5
  d <- impacts.indir.df[q1:q5,]
  d$W <- seq(1,5)
  # base graph
  g <- ggplot(d, aes(W, group=1))
  # ribbon with line
  g <- g +
    geom_hline(yintercept=0, linetype=2) +
    geom_ribbon(aes(ymin = X1-(1.96*X2), 
                    ymax = X1+(1.96*X2)), 
                fill = "grey60", alpha= .5) +
    geom_line(aes(y = X1)) +
    labs(title=paste(varnames[i]," - indirect SDM", sep=""), y="impact") +
    theme(
      plot.title = element_text(size=14),
      axis.title = element_text(size=10),
      axis.text = element_text(size=10)
    ) +
    theme_minimal()
  list.dG[[i]] <- g
  
  png(filename = paste("./figures/impacts_sdm_indir_", varnames[i], "_index.png", sep=""), height=6, width=6, units="in", res=300)
  print(g)
  dev.off()
}



# SLX

impacts.slx<- impacts(slx, 
                      listw=wq1,
                      R=1000,zstats=TRUE
                      #,useHESS = T  # Hessian matrix not available
)
summary(impacts.slx, zstats=TRUE)

# SUMMING UP:
# slm consistent with findings in original article
# other models show stability of these core results, with some variation
# SAC: pop density also sig
# SDM and SLX" turnout volatility no longer sig, even at .10 level, 
# and literacy now significant at either .05 (SDM) or .10 (SLX) level

###############################################
# PREDICTIONS ON SDM MODEL
# after simulating change in X

# could change road density or literacy, among others
# road density has stat sig direct and indirect impacts
# literacy does, too

test_data <- shp.sf
summary(test_data$Density_RD)
# big unit with low road density, north of center/east
test_data$Density_RD[138]
# 0.0321
plot(shp.sf[c(138),1])

summary(test_data$literacy) # median = .738
test_data$Density_RD[c(149, 152)]  # two units in northeast that seem to have lowest lit
# 0.0474 0.0714
plot(shp.sf[c(149,152),1])

# literacy; increase to mean in unit 149

test_data$literacy[149] <- .738



#################################
# calculate impact of 1-unit shock in focal unit
# code based on multiple google searches, Gemini queries, and checks of code to make sure it ran correctly
# as of 2026-08-15, lots of mistakes and hallucinations from Gemini

# make listw a standard sparse matrix
W1 <- as(listw2mat(wq1), "CsparseMatrix")
n <- nrow(W1)
I_n <- Diagonal(n)

# identify  spatial parameters and variable/coef of interest 
rho <- sdm$rho
beta_x1  <- coef(sdm)["literacy"]
theta_x1 <- coef(sdm)["lag.literacy"]

# compute the full global multiplier matrix (I - rho * W)^-1
# This accounts for the infinite rounds of feedback throughout the network
inv_A <- solve(I_n - rho * W1)
S_x1  <- inv_A %*% (I_n * beta_x1 + W1 * theta_x1)

# choose focal unit
target_unit_index <- 149

shock_impacts <- data.frame(
  ID = 1:n,
  Total_Impact = as.numeric(S_x1[, target_unit_index])
)

# 4. Isolate pure spillover (indirect effect)
# Set the target unit's own direct effect to 0 so it doesn't skew the spillover scale
shock_impacts$Pure_Spillover <- shock_impacts$Total_Impact
shock_impacts$Pure_Spillover[target_unit_index] <- 0

# 5. Merge into your sf spatial object
temp$ID <- 1:nrow(temp)
shock_map_sf <- left_join(temp, shock_impacts, by = "ID")

# Create a logical flag to highlight the origin unit on the map
shock_map_sf$Is_Origin <- (shock_map_sf$ID == target_unit_index)

ggplot(data = shock_map_sf) +
  # Plot the spillover gradient
  geom_sf(aes(fill = Pure_Spillover), color = "white", size = 0.05) +
  
  # Highlight the origin unit with a thick red border
  geom_sf(data = filter(shock_map_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 1.2) +
  
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "spillover"
  ) +
  theme_void() 


################################################
# RE_DO, now over 5 orders of neighbors

W1 <- as(listw2mat(wq1), "CsparseMatrix")
n <- nrow(W1)
I_n <- Diagonal(n)

# identify  spatial parameters and variable/coef of interest 
rho <- sdm$rho
beta_x1  <- coef(sdm)["literacy"]
theta_x1 <- coef(sdm)["lag.literacy"]

# choose focal unit
target_unit_index <- 149

# Initialize list to collect the footprints of the shock
shock_orders <- list()
W_current <- I_n 

# set your custom change here (e.g., 1 unit increase)
# Use a negative number if you want to simulate a decrease!
shock_magnitude <- 1 # default; don't even need to specify it if just want 1-unit change  
#shock_magnitude <- 2  

# FIX ODD DIRECT VALUES AT ZERO-ORDER
for (k in 0:4) {
  
  if (k == 0) {
    S_k <- I_n * beta_x1
  } else if (k == 1) {
    W_current <- W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  } else {
    W_current <- W_current %*% W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  }
  
  # Extract the full impact column for our target origin unit
  total_impact_k <- as.numeric(S_k[, target_unit_index])
  
  # FIX 2: Create a proper N-length vector for the direct effect 
  # This ensures only the origin unit lights up, and all other units stay at 0
  direct_vector <- rep(0, n)
  direct_vector[target_unit_index] <- total_impact_k[target_unit_index]
  
  # Isolate the pure spillover component to all other units
  indirect_vector <- total_impact_k
  indirect_vector[target_unit_index] <- 0               
  
  # Combine components for mapping
  combined_impact <- direct_vector + indirect_vector
  
  # scale to policy relevant value
  # cale the raw impacts by your specific shock magnitude
  direct_scaled   <- direct_vector * shock_magnitude
  indirect_scaled <- indirect_vector * shock_magnitude
  combined_scaled <- direct_scaled + indirect_scaled
  
  
  # Normalize this specific facet's output to prevent higher-order decay flatlining
  # only doing this for 1-unit change in order to illustrate
  max_val <- max(abs(combined_impact), na.rm = TRUE)
  if(max_val > 0) {
    normalized_impact <- combined_impact / max_val
  } else {
    normalized_impact <- combined_impact
  }
  
  max_val2 <- max(abs(combined_scaled), na.rm = TRUE)
  if(max_val2 > 0) {
    normalized_scaled <- combined_scaled / max_val2
  } else {
    normalized_scaled <- combined_scaled
  }
  # Store clean vectors
  shock_orders[[paste0("O", k)]] <- data.frame(
    ID = 1:n,
    Order = paste("Order", k),
    Direct = direct_vector,
    Indirect = indirect_vector,
    Total = combined_impact,
    Total_norm = normalized_impact,
    Direct2 = direct_scaled,
    Indirect2 = indirect_scaled,
    Total2 = combined_scaled,
    Total2_norm = normalized_scaled
  )
}


# 3. Combine long dataframe and merge with spatial data
all_shocks_df <- bind_rows(shock_orders)

temp <- shp.sf
temp$ID <- 1:nrow(temp)
spatial_shocks_sf <- temp %>%
  left_join(all_shocks_df, by = "ID")

# add TOTAL EFFECTS; added above in loop, so commented out
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Pure_Direct + spatial_shocks_sf$Pure_Indirect
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Direct + spatial_shocks_sf$Indirect

# Flag the origin polygon for mapping
spatial_shocks_sf$Is_Origin <- (spatial_shocks_sf$ID == target_unit_index)

# map

library(ggh4x)

# directs
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Direct), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "direct"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_literacy_direct.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# indirects
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Indirect), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +

  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "indirect"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_literacy_indirect.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# total

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_literacy_total.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()


# total, normalized

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total_norm), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_literacy_total_norm.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()





##################
# now for road density in unit 138
# now over 5 orders of neighbors

W1 <- as(listw2mat(wq1), "CsparseMatrix")
n <- nrow(W1)
I_n <- Diagonal(n)

# identify  spatial parameters and variable/coef of interest 
rho <- sdm$rho
beta_x1  <- coef(sdm)["Density_RD"]
theta_x1 <- coef(sdm)["lag.Density_RD"]

# choose focal unit
target_unit_index <- 138

# Initialize list to collect the footprints of the shock
shock_orders <- list()
W_current <- I_n 

# NEW: Set your custom change here (e.g., 2 unit increase)
# Use a negative number if you want to simulate a decrease!
shock_magnitude <- 1 # default=1; don't even need to specify it if just want 1-unit change
#shock_magnitude <- 2  

for (k in 0:4) {
  
  if (k == 0) {
    S_k <- I_n * beta_x1
  } else if (k == 1) {
    W_current <- W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  } else {
    W_current <- W_current %*% W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  }
  
  # Extract the full impact column for our target origin unit
  total_impact_k <- as.numeric(S_k[, target_unit_index])
  
  # FIX 2: Create a proper N-length vector for the direct effect 
  # This ensures only the origin unit lights up, and all other units stay at 0
  direct_vector <- rep(0, n)
  direct_vector[target_unit_index] <- total_impact_k[target_unit_index]
  
  # Isolate the pure spillover component to all other units
  indirect_vector <- total_impact_k
  indirect_vector[target_unit_index] <- 0               
  
  # Combine components for mapping
  combined_impact <- direct_vector + indirect_vector
  
  # scale to policy relevant value
  # cale the raw impacts by your specific shock magnitude
  direct_scaled   <- direct_vector * shock_magnitude
  indirect_scaled <- indirect_vector * shock_magnitude
  combined_scaled <- direct_scaled + indirect_scaled
  
  
  # Normalize this specific facet's output to prevent higher-order decay flatlining
  # only doing this for 1-unit change in order to illustrate
  max_val <- max(abs(combined_impact), na.rm = TRUE)
  if(max_val > 0) {
    normalized_impact <- combined_impact / max_val
  } else {
    normalized_impact <- combined_impact
  }
  
  max_val2 <- max(abs(combined_scaled), na.rm = TRUE)
  if(max_val2 > 0) {
    normalized_scaled <- combined_scaled / max_val2
  } else {
    normalized_scaled <- combined_scaled
  }
  # Store clean vectors
  shock_orders[[paste0("O", k)]] <- data.frame(
    ID = 1:n,
    Order = paste("Order", k),
    Direct = direct_vector,
    Indirect = indirect_vector,
    Total = combined_impact,
    Total_norm = normalized_impact,
    Direct2 = direct_scaled,
    Indirect2 = indirect_scaled,
    Total2 = combined_scaled,
    Total2_norm = normalized_scaled
  )
}


# 3. Combine long dataframe and merge with spatial data
all_shocks_df <- bind_rows(shock_orders)

temp <- shp.sf
temp$ID <- 1:nrow(temp)
spatial_shocks_sf <- temp %>%
  left_join(all_shocks_df, by = "ID")

# add TOTAL EFFECTS; added above in loop, so commented out
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Pure_Direct + spatial_shocks_sf$Pure_Indirect
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Direct + spatial_shocks_sf$Indirect

# Flag the origin polygon for mapping
spatial_shocks_sf$Is_Origin <- (spatial_shocks_sf$ID == target_unit_index)

# map

library(ggh4x)

# directs
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Direct), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "direct"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_direct.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# indirects
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Indirect), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "indirect"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_indirect.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# total

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_total.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()


# total, normalized

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total_norm), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_total_norm.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()


#### now simulate a reduction in road density, which would be the more interesting part

#######################################
# now for road density in unit 138
# But now simulating a decrease in road density

W1 <- as(listw2mat(wq1), "CsparseMatrix")
n <- nrow(W1)
I_n <- Diagonal(n)

# identify  spatial parameters and variable/coef of interest 
rho <- sdm$rho
beta_x1  <- coef(sdm)["Density_RD"]
theta_x1 <- coef(sdm)["lag.Density_RD"]

# choose focal unit
target_unit_index <- 138

# Initialize list to collect the footprints of the shock
shock_orders <- list()
W_current <- I_n 

# NEW: Set your custom change here (e.g., 2 unit increase)
# Use a negative number if you want to simulate a decrease!
shock_magnitude <- -1 # default=1; don't even need to specify it if just want 1-unit change
                    # here -1 to capture decrease
#shock_magnitude <- 2  

for (k in 0:4) {
  
  if (k == 0) {
    S_k <- I_n * beta_x1
  } else if (k == 1) {
    W_current <- W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  } else {
    W_current <- W_current %*% W1
    S_k <- (rho^k * W_current * beta_x1) + (rho^(k-1) * W_current * theta_x1)
  }
  
  # Extract the full impact column for our target origin unit
  total_impact_k <- as.numeric(S_k[, target_unit_index])
  
  # FIX 2: Create a proper N-length vector for the direct effect 
  # This ensures only the origin unit lights up, and all other units stay at 0
  direct_vector <- rep(0, n)
  direct_vector[target_unit_index] <- total_impact_k[target_unit_index]
  
  # Isolate the pure spillover component to all other units
  indirect_vector <- total_impact_k
  indirect_vector[target_unit_index] <- 0               
  
  # Combine components for mapping
  combined_impact <- direct_vector + indirect_vector
  
  # scale to policy relevant value
  # cale the raw impacts by your specific shock magnitude
  direct_scaled   <- direct_vector * shock_magnitude
  indirect_scaled <- indirect_vector * shock_magnitude
  combined_scaled <- direct_scaled + indirect_scaled
  
  
  # Normalize this specific facet's output to prevent higher-order decay flatlining
  # only doing this for 1-unit change in order to illustrate
  max_val <- max(abs(combined_impact), na.rm = TRUE)
  if(max_val > 0) {
    normalized_impact <- combined_impact / max_val
  } else {
    normalized_impact <- combined_impact
  }
  
  max_val2 <- max(abs(combined_scaled), na.rm = TRUE)
  if(max_val2 > 0) {
    normalized_scaled <- combined_scaled / max_val2
  } else {
    normalized_scaled <- combined_scaled
  }
  # Store clean vectors
  shock_orders[[paste0("O", k)]] <- data.frame(
    ID = 1:n,
    Order = paste("Order", k),
    Direct = direct_vector,
    Indirect = indirect_vector,
    Total = combined_impact,
    Total_norm = normalized_impact,
    Direct2 = direct_scaled,
    Indirect2 = indirect_scaled,
    Total2 = combined_scaled,
    Total2_norm = normalized_scaled
  )
}


# 3. Combine long dataframe and merge with spatial data
all_shocks_df <- bind_rows(shock_orders)

temp <- shp.sf
temp$ID <- 1:nrow(temp)
spatial_shocks_sf <- temp %>%
  left_join(all_shocks_df, by = "ID")

# add TOTAL EFFECTS; added above in loop, so commented out
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Pure_Direct + spatial_shocks_sf$Pure_Indirect
#spatial_shocks_sf$impacts_total <- spatial_shocks_sf$Direct + spatial_shocks_sf$Indirect

# Flag the origin polygon for mapping
spatial_shocks_sf$Is_Origin <- (spatial_shocks_sf$ID == target_unit_index)

# map

library(ggh4x)

# directs
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Direct2), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "direct"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_direct2_decrease.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# indirects
g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Indirect2), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "indirect"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_indirect2_decrease.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()

# total

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total2), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_total2_decrease.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()


# total, normalized

g <- ggplot(data = spatial_shocks_sf) +
  # Plot the localized spillover values
  geom_sf(aes(fill = Total2_norm), color = "grey70", size = 0.02) +
  
  # Explicitly outline the origin unit in black on every facet
  geom_sf(data = filter(spatial_shocks_sf, Is_Origin == TRUE), 
          fill = NA, color = "black", size = 0.8) +
  
  
  facet_wrap(~Order, ncol = 5) +
  scale_fill_gradient2(
    low = "blue",       # Blue for negative spillover
    mid = "white",       # Pale yellow for zero neutral zones
    high = "red",      # Red for positive spillover
    midpoint = 0,
    name = "total"
  ) +
  theme_void() +
  theme(
    # strip.text = element_text(face = "bold", size = 11),
    #plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"#,
    #legend.key.width = unit(0.8, "cm")
  )
g

png(file="./figures/map_sim_road-density_total_norm2_decrease.png", height=3, width=6, units="in", res=300)
print(g)
dev.off()





###############################################
# GWR (Geographically Weighted Regression)
###############################################

# GWR of model 3 above

# create distance matrix
tic("dm")
dMat <- gw.dist(dp.locat=coordinates(shp))
toc()

model <- formula(Count_~ pov_p_2008 + gini_2008
                 + ferat_2008 + p_share
                 + p_shvol + turninv
                 + p_tuvol + p_ethfr
                 + Count_3 + Density_RD
                 + Pop_Densit + Count_4
                 + literacy + grid_perCa)

# find optimal bandwidth
bw.bsq <- bw.gwr(model,
                 data=shp, 
                 approach="CV",
                 kernel="bisquare",
                 adaptive=TRUE,
                 dMat=dMat)

# check for heterogeneity
# randomization test to identify which covariates have sig non-stationarity
tic("mc")
mc1 <- montecarlo.gwr(model, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.bsq,
                      kernel="bisquare",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
# 36 sec

mc1
# shows female ratio and grid_percap are nonstationary/heterogeneous

# check with different bandwidth
bw.g <- bw.gwr(model,
               data=shp, 
               approach="CV",
               kernel="gaussian",
               adaptive=TRUE,
               dMat=dMat)

tic("mc")
mc2 <- montecarlo.gwr(model, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.g,
                      kernel="gaussian",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
# also about 36 sec

mc2
# again shows fem ratio and grid_perca significant

# note that original authors focused on road density and turnout volatility
# because these were the significant predictors in SLM (article, p9; appendix, p11)

# save mc objects to file; use xtable instead of stargazer to preserve digits 

print.xtable(xtable(mc1, digits=3), file="./tables/mc_bw1.tex", type="latex", 
          floating=FALSE)
print.xtable(xtable(mc2, digits=3), file="./tables/mc_bw2.tex", type="latex", 
             floating=FALSE)

###############################################
# bootstrap tests for models with raw vars

dMat_clean = dMat
tic("boot")
boot1 <- gwr.bootstrap(model,
                         data=shp, 
                         R = 19,
                         #k.nearneigh = 20,
                         approach="AIC",
                         #bw = bw.bsqsc,
                         kernel="bisquare",
                         adaptive=TRUE, 
                         dMat = dMat_clean,
                         longlat=TRUE
)
toc()
# kept getting same error:
# Error in model.type(obj) : Unsupported regression type.
# got this despite numerous troubleshooting attempts including clean, numeric data; 
# spatial points; no NAs; own dMat; 

############################################################
# own residual bootstrap method following Chen et al 2020,
# plus online searches
# build on basic ols and gwr1 models

# 
set.seed(1234)

temp <- shp

y_var  <- "Count_"
x_vars <- c("pov_p_2008", "gini_2008", "ferat_2008", "p_share", "p_shvol",
            "turninv", "p_tuvol", "p_ethfr", "Count_3", "Density_RD",
            "Pop_Densit", "Count_4", "literacy", "grid_perCa") 
all_coefs <- c("Intercept", x_vars) # Track the intercept too

formula_null <- as.formula(paste(y_var, "~", paste(x_vars, collapse = "+")))


# define function to extra covariance
# This helper calculates the variance of every local coefficient across space
get_gwr_coef_variance <- function(formula, data, dMat, bw, kernel) {
  # Select bandwidth dynamically
  #bw <- bw.bsq
  # Fit GWR
  gwr_model <- gwr.basic(formula, 
                         data=data, 
                         bw=bw,
                         kernel=kernel,
                         adaptive=TRUE, 
                         dMat=dMat
  )
  
  # Extract localized beta coefficients matrix
  # Rows = geographic locations, Columns = covariates/intercept
  betas <- data.frame(gwr_model$SDF[, 1:15]) 
  colnames(betas) <- all_coefs
  
  # Calculate variance across space for each covariate
  coef_variances <- apply(betas, 2, var)
  return(coef_variances)
}

# 3. CALCULATE OBSERVED COEFFICIENT VARIANCE
# This establishes the true localized variability of your real coefficients
observed_variances <- get_gwr_coef_variance(formula=formula_null, data=temp, dMat=dMat,
                                            bw=bw.bsq,
                                            kernel="bisquare")

# get yhats and resids from global ols 

fitted_vals <- fitted(ols)
resids      <- residuals(ols)

# BOOTSTRAP LOOP FOR INDIVIDUAL COVARIATES
B <- 999 # Number of replications
# Matrix to store the variance of coefficients for each bootstrap run
boot_variance_matrix <- matrix(NA, nrow = B, ncol = length(all_coefs))
colnames(boot_variance_matrix) <- all_coefs

cat("Running bootstrap. This might take a moment...\n")

for (i in 1:B) {
  # Wild Bootstrap (robust to heteroskedasticity)
  rademacher  <- sample(c(-1, 1), length(resids), replace = TRUE)
  boot_resids <- sample(resids, replace=TRUE) * rademacher
  
  # Reconstruct y-variable under global assumptions (no spatial variation)
  temp@data$boot_y <- fitted_vals + boot_resids
  boot_formula <- as.formula(paste("boot_y ~", paste(x_vars, collapse = "+")))
  
  # Capture variances from the simulated data
  boot_variance_matrix[i, ] <- get_gwr_coef_variance(boot_formula, temp, dMat,
                                                     bw=bw.bsq, kernel="bisquare")
}

# COMPUTE INDIVIDUAL P-VALUES
p_values <- numeric(length(all_coefs))
names(p_values) <- all_coefs

for (coef in all_coefs) {
  # P-value = fraction of bootstrap runs where random variance exceeded real variance
  p_values[coef] <- sum(boot_variance_matrix[, coef] >= observed_variances[coef]) / B
}

#PRINT FINAL ASSESSMENT TABLE
cat("\n--- Spatial Non-Stationarity Test Results ---\n")
results_df <- data.frame(
  Observed_Variance = observed_variances,
  P_Value = p_values,
  Decision = ifelse(p_values < 0.05, "Spatially Varying (Reject H0)", "Spatially Constant (Fail to Reject)")
)
print(results_df)

temptab <- data.frame(p = p_values)

print.xtable(xtable(temptab, digits=3), file="./tables/bootstrap_bsq.tex", type="latex", 
             floating=FALSE)



#### RE-DO with Gaussian kernel, just as did two version with monte carlo test

observed_variances <- get_gwr_coef_variance(formula=formula_null, data=temp, dMat=dMat,
                                            bw=bw.g,
                                            kernel="gaussian")

# get yhats and resids from global ols 

fitted_vals <- fitted(ols)
resids      <- residuals(ols)

# BOOTSTRAP LOOP FOR INDIVIDUAL COVARIATES
B <- 999 # Number of replications
# Matrix to store the variance of coefficients for each bootstrap run
boot_variance_matrix <- matrix(NA, nrow = B, ncol = length(all_coefs))
colnames(boot_variance_matrix) <- all_coefs

cat("Running bootstrap. This might take a moment...\n")

for (i in 1:B) {
  # Wild Bootstrap (robust to heteroskedasticity)
  rademacher  <- sample(c(-1, 1), length(resids), replace = TRUE)
  boot_resids <- sample(resids, replace=TRUE) * rademacher
  
  # Reconstruct y-variable under global assumptions (no spatial variation)
  temp@data$boot_y <- fitted_vals + boot_resids
  boot_formula <- as.formula(paste("boot_y ~", paste(x_vars, collapse = "+")))
  
  # Capture variances from the simulated data
  boot_variance_matrix[i, ] <- get_gwr_coef_variance(boot_formula, temp, dMat, 
                                                     bw=bw.g, kernel="gaussian")
}

# COMPUTE INDIVIDUAL P-VALUES
p_values <- numeric(length(all_coefs))
names(p_values) <- all_coefs

for (coef in all_coefs) {
  # P-value = fraction of bootstrap runs where random variance exceeded real variance
  p_values[coef] <- sum(boot_variance_matrix[, coef] >= observed_variances[coef]) / B
}


temptab <- data.frame(p = p_values)

print.xtable(xtable(temptab, digits=3), file="./tables/bootstrap_g.tex", type="latex", 
             floating=FALSE)



###############################################

# local collinearity diagnostics

gwr1.diagnostics <- gwr.collin.diagno(model, data = shp, 
                                    bw = bw.bsq,
                                    kernel="bisquare",
                                    adaptive=TRUE,
                                    dMat=dMat)
gwr1.diagnostics$corr.mat
summary(gwr1.diagnostics$VIF)
gwr1.diagnostics$local_CN # these are very high, above 200

gwr2.diagnostics <- gwr.collin.diagno(model, data = shp, 
                                      bw = bw.g,
                                      kernel="gaussian",
                                      adaptive=TRUE,
                                      dMat=dMat)
gwr2.diagnostics$local_CN # these are also very high, above 200

###############################################
# run gwr models with two different bandwidths
tic("gwr1")
gwr1 <- gwr.basic(model, 
                  data=shp, 
                  bw=bw.bsq,     
                  kernel="bisquare",
                  adaptive=TRUE, 
                  dMat=dMat
)
toc()
# less than 1 sec

write.csv(gwr1$SDF, "./tables/gwr1.csv") 


# check with different bandwidth
bw.g <- bw.gwr(model,
               data=shp, 
               approach="CV",
               kernel="gaussian",
               adaptive=TRUE,
               dMat=dMat)


tic("gwr2")
gwr2 <- gwr.basic(model, 
                  data=shp, 
                  bw=bw.g,     
                  kernel="gaussian",
                  adaptive=TRUE, 
                  dMat=dMat
)
toc()
# less than 1 sec

write.csv(gwr2$SDF, "./tables/gwr2.csv") 

# GWR results
# can view as table
gwr1
gwr2

# can also compare fit to other spatial models
gwr1$GW.diagnostic$gwR2.adj
gwr2$GW.diagnostic$gwR2.adj

gwr1$GW.diagnostic$AIC
gwr2$GW.diagnostic$AIC



# better to graph/visualize results
# see below

###############################################
# Generate local estimates at 95% confidence
###############################################

# note: authors only focus on road density and turnout volatility
# we do all to illustrate how maps align with ols model and diagnostics

#############################################

# local estimates from gwr1
gwr1$SDF$pov95 <- gwr1$SDF$pov_p_2008
gwr1$SDF$pov95[abs(gwr1$SDF$pov_p_2008_TV)<=1.96] <- 0

gwr1$SDF$gini95 <- gwr1$SDF$gini_2008
gwr1$SDF$gini95[abs(gwr1$SDF$gini_2008_TV)<=1.96] <- 0

gwr1$SDF$ferat95 <- gwr1$SDF$ferat_2008
gwr1$SDF$ferat95[abs(gwr1$SDF$ferat_2008_TV)<=1.96] <- 0

gwr1$SDF$p_share95 <- gwr1$SDF$p_share
gwr1$SDF$p_share95[abs(gwr1$SDF$p_share_TV)<=1.96] <- 0

gwr1$SDF$p_shvol95 <- gwr1$SDF$p_shvol
gwr1$SDF$p_shvol95[abs(gwr1$SDF$p_shvol_TV)<=1.96] <- 0

gwr1$SDF$turn95 <- gwr1$SDF$turninv
gwr1$SDF$turn95[abs(gwr1$SDF$turninv_TV)<=1.96] <- 0

gwr1$SDF$p_tuvol95 <- gwr1$SDF$p_tuvol
gwr1$SDF$p_tuvol95[abs(gwr1$SDF$p_tuvol_TV)<=1.96] <- 0

gwr1$SDF$ethfr95 <- gwr1$SDF$p_ethfr
gwr1$SDF$ethfr95[abs(gwr1$SDF$p_ethfr_TV)<=1.96] <- 0

gwr1$SDF$count3_95 <- gwr1$SDF$Count_3
gwr1$SDF$count3_95[abs(gwr1$SDF$Count_3_TV)<=1.96] <- 0

gwr1$SDF$Density_RD95 <- gwr1$SDF$Density_RD
gwr1$SDF$Density_RD95[abs(gwr1$SDF$Density_RD_TV)<=1.96] <- 0

gwr1$SDF$popdens95 <- gwr1$SDF$Pop_Densit
gwr1$SDF$popdens95[abs(gwr1$SDF$Pop_Densit_TV)<=1.96] <- 0

gwr1$SDF$count4_95 <- gwr1$SDF$Count_4
gwr1$SDF$count4_95[abs(gwr1$SDF$Count_4_TV)<=1.96] <- 0

gwr1$SDF$lit95 <- gwr1$SDF$literacy
gwr1$SDF$lit95[abs(gwr1$SDF$literacy_TV)<=1.96] <- 0

gwr1$SDF$grid_perCa95 <- gwr1$SDF$grid_perCa
gwr1$SDF$grid_perCa95[abs(gwr1$SDF$grid_perCa_TV)<=1.96] <- 0

# local estimates from gwr2
gwr2$SDF$p_share95 <- gwr2$SDF$p_share
gwr2$SDF$p_share95[abs(gwr2$SDF$p_share_TV)<=1.96] <- 0

gwr2$SDF$p_shvol95 <- gwr2$SDF$p_shvol
gwr2$SDF$p_shvol95[abs(gwr2$SDF$p_shvol_TV)<=1.96] <- 0

gwr2$SDF$p_tuvol95 <- gwr2$SDF$p_tuvol
gwr2$SDF$p_tuvol95[abs(gwr2$SDF$p_tuvol_TV)<=1.96] <- 0

gwr2$SDF$ferat95 <- gwr2$SDF$ferat_2008
gwr2$SDF$ferat95[abs(gwr2$SDF$ferat_2008_TV)<=1.96] <- 0

gwr2$SDF$Density_RD95 <- gwr2$SDF$Density_RD
gwr2$SDF$Density_RD95[abs(gwr2$SDF$Density_RD_TV)<=1.96] <- 0

gwr2$SDF$grid_perCa95 <- gwr2$SDF$grid_perCa
gwr2$SDF$grid_perCa95[abs(gwr2$SDF$grid_perCa_TV)<=1.96] <- 0

#####################################
# add local estimates to shapefile
#####################################

# add estimates from new models

# gwr1
shp$pov.gwr1 <- gwr1$SDF$pov95
shp$gini.gwr1 <- gwr1$SDF$gini95
shp$ferat.gwr1 <- gwr1$SDF$ferat95
shp$p_share.gwr1 <- gwr1$SDF$p_share95
shp$p_shvol.gwr1 <- gwr1$SDF$p_shvol95
shp$turn.gwr1 <- gwr1$SDF$turn95
shp$p_tuvol.gwr1 <- gwr1$SDF$p_tuvol95
shp$ethfr.gwr1 <- gwr1$SDF$ethfr95
shp$count3.gwr1 <- gwr1$SDF$count3_95
shp$road.gwr1 <- gwr1$SDF$Density_RD95
shp$popdens.gwr1 <- gwr1$SDF$popdens95
shp$count4.gwr1 <- gwr1$SDF$count4_95
shp$lit.gwr1 <- gwr1$SDF$lit95
shp$gpc.gwr1 <- gwr1$SDF$grid_perCa95

# gwr2
shp$p_share.gwr2 <- gwr2$SDF$p_share95
shp$p_shvol.gwr2 <- gwr2$SDF$p_shvol95
shp$p_tuvol.gwr2 <- gwr2$SDF$p_tuvol95
shp$ferat.gwr2 <- gwr2$SDF$ferat95
shp$road.gwr2 <- gwr2$SDF$Density_RD95
shp$gpc.gwr2 <- gwr2$SDF$grid_perCa95

# check collinearity of gwr coefficients
col1 <- gwr.collin.diagno(model, data=shp, adaptive = TRUE, 
                          kernel="bisquare", bw=bw.bsq, dMat=dMat)
summary(col1$VIF)
#highest is only around 6 (Count_4, health facilities)


#########################################################
# save data
#save.image("./data/working/working20230626_models.RData")
#########################################################


########################################################
#
# MAPS OF GWR COEFFICIENTS
#
# authors focused on tunout volatility and road density because those were sig in SLM
# here, we report all covariates related to core hypotheses (vote share, vote share volatility,
# turnout volatility, elec. grid per capita), plus fem ratio and road density because they 
# were either significant in SLM or montecarlo test showed non-stationarity
#
########################################################

# convert to sf object for graphing
shp.sf <- st_as_sf(shp)

# GWR1:

# poverty

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=pov.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Poverty, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_poverty.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# inequality

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gini.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Inequality, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_gini.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# female ratio

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Female Ratio, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_femratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# vote share

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_share.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote share, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_voteshare.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# vote share volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout 

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=turn.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_turnout.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# ethfr

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ethfr.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Ethnic fragmentation, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_ethfr.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# Count 3 (WB projects)

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=count3.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="WB projects, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_count3wb.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# pop density

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=popdens.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Population density, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_popdensity.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# road density

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=road.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# count 4 (health facilities)

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=count4.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Health facilities, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g

png(file="./figures/gwr1_count4healthfac.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# literacy

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=lit.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Literacy, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_lit.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.gwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 1, bisquare)") +
  theme_minimal()
g


png(file="./figures/gwr1_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

#######################
# GWR2
#######################

# vote share volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Vote Share volatility, local \u03B2 (GWR 2, gaussian)") +
  theme_minimal()
g

png(file="./figures/gwr2_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Turnout volatility, local \u03B2 (GWR 2, gaussian)") +
  theme_minimal()
g

png(file="./figures/gwr2_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# female ratio

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Female Ratio, local \u03B2 (GWR 2, gaussian)") +
  theme_minimal()
g

png(file="./figures/gwr2_femratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# road density

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=road.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Road density, local \u03B2 (GWR 2, gaussian)") +
  theme_minimal()
g


png(file="./figures/gwr2_road_density.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.gwr2)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title="Grid density pc, local \u03B2 (GWR 2, gaussian)") +
  theme_minimal()
g


png(file="./figures/gwr2_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

##############################################
# MGWR of gwr1


# LOAD PACKAGES for MGWR
library(pacman)
p_load(GWmodel, tidyverse, devtools, tmap, sf, ggplot2, gridExtra, car, 
       plyr, dplyr, spdep, broom)


# 2021 source updated multiscale function that generates SEs and T-values
#source_url("https://github.com/lexcomber/inia_materials/blob/master/gwr.multiscale_T.r?raw=TRUE") 


########################################################
# STARTED HERE JUN 2026
########################################################

model <- formula(Count_~ pov_p_2008 + gini_2008
                 + ferat_2008 + p_share
                 + p_shvol + turninv
                 + p_tuvol + p_ethfr
                 + Count_3 + Density_RD
                 + Pop_Densit + Count_4
                 + literacy + grid_perCa)

# MGWR steps

########################
# MGWR of GWR1 (bisquare kernel)
# STEP 1: generate initial global bandwidth (here, gaussian)
tic("mgwr_step1")
bw.bsq <- bw.gwr(model, data=shp, 
                 approach="AIC", 
                 kernel="bisquare", adaptive=TRUE, dMat=dMat)
toc()
# less than 1 sec on 20260618
bw.bsq
# [1] 120

# STEP 2: plug bw from STEP 1 into MGWR model to generate multiple bandwidths
tic("mgwr_step2")
mgwr_bw.bsq <- gwr.multiscale(model, data=shp,
                              criterion="dCVR", 
                              adaptive=TRUE, max.iterations = 30,
                              threshold=.01, # following recommended "strategy 3" in Lu et al 2017 (994)
                              kernel="bisquare", 
                              bws0 = c(rep(bw.bsq, length(ols$coefficients))),  # 1 for intercept and 1 for each predictor
                              #p.vals=c(1,2,3),
                              #dMats=list(dMat, dMat), # there should be as many dMats here as coefficients
                              verbose=TRUE,
                              hatmatrix = TRUE # if want hatmatrix for whole model
                              #parallel.method = "omp"
                              )
toc()
# mgwr_step2: 
# 26 secs on 20260618

## extract the bandwidths (see below)
bw.mgwr_bw.bsq  <- round(mgwr_bw.bsq[[2]]$bws,1) # the estimated bandwidths


# STEP 3: plug bandwidths from step 2 into new MGWR (PSDM GWR and MGWR are basically same thing;
# different names for same thing)
#PSDM GWR with adaptive

tic("mgwr_step3")
mgwr_bw.bsq_res <- gwr.multiscale(model, data=shp,
                                 criterion="dCVR", 
                                 adaptive=TRUE, 
                                 max.iterations = 10,
                                 threshold = 0.01, # consider lowering to 0.001
                                 kernel="bisquare", 
                                 bws0=c(bw.mgwr_bw.bsq), bw.seled=rep(T, length(ols$coefficients)),
                                 #p.vals=c(1,2,3),
                                 #dMats=list(dMat, dMat, dMat, dMat, dMat, dMat, dMat, dMat, 
                                 #           dMat, dMat, dMat, dMat, dMat, dMat, dMat, dMat),
                                 verbose = TRUE, # so you can see progress; set FALSE otherwise
                                 hatmatrix = TRUE#,  # added hatmatrix TRUE to see if std errs appears
                                 #parallel.method="omp"
                                 #parallel.arg = 6
                                 #nlower=30
)
toc()
# mgwr_step3: 
# 7 secs on 20260618 with threshold = 0.01

mgwr_bw.bsq_res

# bandwidths
round(mgwr_bw.bsq_res[[2]]$bws,1)

####################################
# save working data

#save.image("./data/working/working20260618_models.RData")

####################################
# MGWR maps

mgwr1 <- mgwr_bw.bsq_res 

mgwr1$SDF$p_share95 <- mgwr1$SDF$p_share
mgwr1$SDF$p_share95[abs(mgwr1$SDF$p_share_TV)<=1.96] <- 0

mgwr1$SDF$p_shvol95 <- mgwr1$SDF$p_shvol
mgwr1$SDF$p_shvol95[abs(mgwr1$SDF$p_shvol_TV)<=1.96] <- 0

mgwr1$SDF$p_tuvol95 <- mgwr1$SDF$p_tuvol
mgwr1$SDF$p_tuvol95[abs(mgwr1$SDF$p_tuvol_TV)<=1.96] <- 0

mgwr1$SDF$ferat95 <- mgwr1$SDF$ferat_2008
mgwr1$SDF$ferat95[abs(mgwr1$SDF$ferat_2008_TV)<=1.96] <- 0

mgwr1$SDF$Density_RD95 <- mgwr1$SDF$Density_RD
mgwr1$SDF$Density_RD95[abs(mgwr1$SDF$Density_RD_TV)<=1.96] <- 0

mgwr1$SDF$literacy95 <- mgwr1$SDF$literacy
mgwr1$SDF$literacy95[abs(mgwr1$SDF$literacy_TV)<=1.96] <- 0

mgwr1$SDF$grid_perCa95 <- mgwr1$SDF$grid_perCa
mgwr1$SDF$grid_perCa95[abs(mgwr1$SDF$grid_perCa_TV)<=1.96] <- 0

# add mgwr1 values to shp
shp$p_share.mgwr1 <- mgwr1$SDF$p_share95
shp$p_shvol.mgwr1 <- mgwr1$SDF$p_shvol95
shp$p_tuvol.mgwr1 <- mgwr1$SDF$p_tuvol95
shp$ferat.mgwr1 <- mgwr1$SDF$ferat95
shp$road.mgwr1 <- mgwr1$SDF$Density_RD95
shp$lit.mgwr1 <- mgwr1$SDF$literacy95
shp$gpc.mgwr1 <- mgwr1$SDF$grid_perCa95

# convert to sf object for graphing
shp.sf <- st_as_sf(shp)

# check order of predictors for proper bandwidths
mgwr1$GW.arguments$formula
mgwr1$GW.arguments$bws

# female ratio (ferat_2008)

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.mgwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("female ratio, local \u03B2 (MGWR), bw = ", 
                                mgwr1$GW.arguments$bws[4], " (", 
                                round((mgwr1$GW.arguments$bws[4]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1_female_ratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# voteshare volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.mgwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1$GW.arguments$bws[6], " (", 
                                round((mgwr1$GW.arguments$bws[6]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.mgwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1$GW.arguments$bws[8], " (", 
                                round((mgwr1$GW.arguments$bws[8]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.mgwr1)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1$GW.arguments$bws[15], " (", 
                                round((mgwr1$GW.arguments$bws[15]/170)*100,2), "%)")) +
  theme_minimal()
g


png(file="./figures/mgwr1_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

#####################################

# REPEAT WITH SCALED VARIABLES TO CHECK STABILITY
# Interpretability of results are easier with raw variables, but check robustness 

model_scale <- formula(Count_~ scale(pov_p_2008) + 
                         scale(gini_2008) +
                         scale(ferat_2008) + 
                         scale(p_share) +
                         scale(p_shvol) + 
                         scale(turninv) + 
                         scale(p_tuvol) + 
                         scale(p_ethfr) +
                         scale(Count_3) + 
                         scale(Density_RD) + 
                         scale(Pop_Densit) + 
                         scale(Count_4) + 
                         scale(literacy) + 
                         scale(grid_perCa)
)

olssc <- lm(model_scale, data=shp.sf)

# save scaled ols to file
stargazer(olssc, out = "./tables/olsresults_scaled.tex", type="latex", 
          no.space = TRUE,
          float = FALSE, # leave out \begin{table} so can customize within tex file
          dep.var.caption = "", # omits dep var caption
          covariate.labels = c("poverty", "inequality", "female ratio", 
                             "vote share", "volatility (vote share)",
                             "turnout",
                             "volatility (turnout)",
                             "ethnic frag.",
                             "Count3",
                             "road density",
                             "pop. density",
                             "count4",
                             "literacy",
                             "grid density",
                             "constant"),
          omit.stat = "f",
          add.lines = c("AIC", AIC(olssc))
)

# save basic and scaled ols to file together

stargazer(ols, olssc, out = "./tables/olsresults_basic-and-scaled.tex", type="latex", 
          no.space = TRUE,
          float = FALSE, # leave out \begin{table} so can customize within tex file
          model.names = FALSE,
          model.numbers = FALSE,
          column.labels = c("OLS (raw Xs)", "OLS (scaled Xs)"),
          dep.var.caption = "", # omits dep var caption
          covariate.labels = c("poverty", "inequality", "female ratio", 
                             "vote share", "volatility (vote share)",
                             "turnout",
                             "volatility (turnout)",
                             "ethnic frag.",
                             "Count3",
                             "road density",
                             "pop. density",
                             "count4",
                             "literacy",
                             "grid density"),
          omit.stat = "f",
          add.lines = c("AIC", AIC(ols), AIC(olssc))
)

# use modelsummary
p_load(modelsummary)
models <- list("OLS" = ols, "OLS (scaled Xs)" = olssc)
modelsummary(models,
             output = "./tables/olsresults_basic-and-scaled.tex",
             stars = TRUE,
             coef_map = c(
              "pov_p_2008"="poverty", 
              "gini_2008"= "inequality",
              "ferat_2008"="female ratio",
              "p_share" = "NDC vote share", 
              "p_shvol" = "vote volatility",
              "turninv" = "turnout",
              "p_tuvol" = "turnout volatility",
              "p_ethfr" =  "ethnic frag.",
              "Count_3" =  "WB projects",
              "Density_RD"  = "road density",
              "Pop_Densit" = "pop. density",
              "Count_4" = "health facilities",
              "literacy"  = "literacy",
              "grid_perCa" = "grid density",
              "scale(pov_p_2008)"="poverty", 
              "scale(gini_2008)"= "inequality",
              "scale(ferat_2008)"="female ratio",
              "scale(p_share)" = "NDC vote share", 
              "scale(p_shvol)" = "vote volatility",
              "scale(turninv)" = "turnout",
              "scale(p_tuvol)" = "turnout volatility",
              "scale(p_ethfr)" =  "ethnic frag.",
              "scale(Count_3)" =  "WB projects",
              "scale(Density_RD)"  = "road density",
              "scale(Pop_Densit)" = "pop. density",
              "scale(Count_4)" = "health facilities",
              "scale(literacy)"  = "literacy",
              "scale(grid_perCa)" = "grid density",
              "(Intercept)" = "Constant"),
             gof_map = c("nobs", "r.squared", "rmse", "aic") # keep only these GOF
             )
# check montecarlo

# check for heterogeneity
# randomization test to identify which covariates have sig non-stationarity

tic("b.bsqsc")
bw.bsqsc <- bw.gwr(model_scale, data=shp, 
                 approach="AIC", 
                 kernel="bisquare", adaptive=TRUE, dMat=dMat)
toc()
# less than 1 sec on 20260622
bw.bsqsc
# 120

tic("mc")
mc1sc <- montecarlo.gwr(model_scale, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.bsq,
                      kernel="bisquare",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
# 36 sec

mc1sc
# shows gini, p_share, turninv, p_ethr, count_3, and gpc are all stable/stationary
# everything else is heterogeneous/uneven


# check with different bandwidth
bw.gsc <- bw.gwr(model_scale,
               data=shp, 
               approach="AIC",
               kernel="gaussian",
               adaptive=TRUE,
               dMat=dMat)
bw.gsc
# still 19

tic("mc")
mc2sc <- montecarlo.gwr(model_scale, 
                      data=shp, 
                      nsims = 999,
                      bw = bw.gsc,
                      kernel="gaussian",
                      adaptive=TRUE, 
                      dMat = dMat)
toc()
#  40 sec

mc2sc
# again shows fem ratio and grid_perca significant

# note that original authors focused on road density and turnout volatility
# because these were the significant predictors in SLM (article, p9; appendix, p11)

# save mc objects to file; use xtable instead of stargazer to preserve digits 

print.xtable(xtable(mc1sc, digits=3), file="./tables/mc_bw1bsqsc.tex", type="latex", 
             floating=FALSE)
print.xtable(xtable(mc2sc, digits=3), file="./tables/mc_bw2gsc.tex", type="latex", 
             floating=FALSE)

####################################################
# bootstrap tests for stationarity


shp@data$pov_p_2008_std <- as.vector(scale(shp@data$pov_p_2008)) # as.vector() strips attr from scaled values 
# so that it is just a clean numeric vector
shp@data$gini_2008_std <- as.vector(scale(shp@data$gini_2008))
shp@data$ferat_2008_std <- as.vector(scale(shp@data$ferat_2008))
shp@data$p_share_std <- as.vector(scale(shp@data$p_share))
shp@data$p_shvol_std <- as.vector(scale(shp@data$p_shvol))
shp@data$turninv_std <- as.vector(scale(shp@data$turninv))
shp@data$p_tuvol_std <- as.vector(scale(shp@data$p_tuvol))
shp@data$p_ethfr_std <- as.vector(scale(shp@data$p_ethfr))
shp@data$Count_3_std <- as.vector(scale(shp@data$Count_3))
shp@data$Density_RD_std <- as.vector(scale(shp@data$Density_RD))
shp@data$Pop_Densit_std <- as.vector(scale(shp@data$Pop_Densit))
shp@data$Count_4_std <- as.vector(scale(shp@data$Count_4))
shp@data$literacy_std <- as.vector(scale(shp@data$literacy))
shp@data$grid_perCa_std <- as.vector(scale(shp@data$grid_perCa))

model_scale2 <- formula(Count_ ~ pov_p_2008_std + 
                          gini_2008_std + ferat_2008_std + 
                          p_share_std + p_shvol_std + 
                          turninv_std + p_tuvol_std + 
                          p_ethfr_std + Count_3_std + 
                          Density_RD_std + Pop_Densit_std + 
                          Count_4_std + literacy_std + 
                          grid_perCa_std)

# clean numeric matrix
dftemp <- shp@data
dftemp_clean_numeric <- dftemp[, c(
  "Count_" , "pov_p_2008_std" , 
    "gini_2008_std" , "ferat_2008_std" , 
    "p_share_std" , "p_shvol_std" , 
    "turninv_std" , "p_tuvol_std" , 
    "p_ethfr_std" , "Count_3_std" , 
    "Density_RD_std" , "Pop_Densit_std" , 
    "Count_4_std" , "literacy_std" , 
    "grid_perCa_std"
)]

str(dftemp_clean_numeric)
summary(dftemp_clean_numeric)
# no NAs
table(is.na(dftemp_clean_numeric))
# all FALSE

# 2. Extract coordinates and force the names to be EXACTLY "X" and "Y"
coords_matrix <- coordinates(shp)
colnames(coords_matrix) <- c("X", "Y")
df_points <- SpatialPointsDataFrame(coords_matrix, dftemp_clean_numeric) 

tic("boot")
boot1sc <- gwr.bootstrap(model_scale2,
                        data=df_points, 
                        R = 49,
                        approach="AIC",
                        #bw = bw.bsqsc,
                        kernel="bisquare",
                        adaptive=TRUE, 
                        #dMat = dMat
                        longlat=TRUE
                        )
toc()
# 36 sec

boot1sc
# shows gini, p_share, turninv, p_ethr, count_3, and gpc are all stable/stationary
# everything else is heterogeneous/uneven


# check with different bandwidth
bw.gsc <- bw.gwr(model_scale,
                 data=shp, 
                 approach="CV",
                 kernel="gaussian",
                 adaptive=TRUE,
                 dMat=dMat)
bw.gsc
# still 19

tic("boot")
boot2sc <- gwr.bootstrap(model_scale2, 
                        data=shp, 
                        R = 99,
                        kernel="gaussian",
                        adaptive=TRUE, 
                        dMat = dMat)
toc()
#  40 sec

mc2sc
# again shows fem ratio and grid_perca significant

# note that original authors focused on road density and turnout volatility
# because these were the significant predictors in SLM (article, p9; appendix, p11)

# save mc objects to file; use xtable instead of stargazer to preserve digits 

print.xtable(xtable(mc1sc, digits=3), file="./tables/mc_bw1bsqsc.tex", type="latex", 
             floating=FALSE)
print.xtable(xtable(mc2sc, digits=3), file="./tables/mc_bw2gsc.tex", type="latex", 
             floating=FALSE)

####################################################
# local collinearity diagnostics

gwr1sc.diagnostics <- gwr.collin.diagno(model_scale, data = shp, 
                                      bw = bw.bsqsc,
                                      kernel="bisquare",
                                      adaptive=TRUE,
                                      dMat=dMat)
#gwr1sc.diagnostics$corr.mat
#summary(gwr1.diagnostics$VIF)
gwr1sc.diagnostics$local_CN 
summary(gwr1sc.diagnostics$local_CN)
hist(gwr1sc.diagnostics$local_CN)

# these are much better, most in single digits; median around 8; max around 13

gwr2sc.diagnostics <- gwr.collin.diagno(model_scale, data = shp, 
                                      bw = bw.gsc,
                                      kernel="gaussian",
                                      adaptive=TRUE,
                                      dMat=dMat)
gwr2sc.diagnostics$local_CN # these are also very high, above 200
summary(gwr2sc.diagnostics$local_CN)
# again, much better than raw vars; median around 11; max around 25

############################################

# MGWR STEPS

tic("mgwr__scaled_step1")
bw.bsqsc <- bw.gwr(model_scale, data=shp, 
                 approach="AIC", 
                 kernel="bisquare", adaptive=TRUE, dMat=dMat)
toc()
# less than 1 sec on 20260622
bw.bsqsc
# still 120

# STEP 2: plug bw from STEP 1 into MGWR model to generate multiple bandwidths
tic("mgwr_step2")
mgwr_bw.bsqsc <- gwr.multiscale(model_scale, data=shp,
                              criterion="dCVR", 
                              adaptive=TRUE, max.iterations = 30,
                              threshold=.01, # following recommended "strategy 3" in Lu et al 2017 (994)
                              kernel="bisquare", 
                              bws0 = c(rep(bw.bsqsc, length(ols$coefficients))),  # 1 for intercept and 1 for each predictor
                              #p.vals=c(1,2,3),
                              #dMats=list(dMat, dMat), # there should be as many dMats here as coefficients
                              verbose=TRUE,
                              hatmatrix = TRUE,
                              #parallel.method = "omp"
)
toc()
# mgwr_step2: 
# 26 secs on 20260622

## extract the bandwidths (see below)
bw.mgwr_bw.bsqsc  <- round(mgwr_bw.bsqsc[[2]]$bws,1) # the estimated bandwidths
# exact same bws as with unscaled vars

# STEP 3: plug bandwidths from step 2 into new MGWR (PSDM GWR and MGWR are basically same thing;
# different names for same thing)
#PSDM GWR with adaptive

tic("mgwr_scale_step3")
mgwr_bw.bsq_ressc <- gwr.multiscale(model_scale, data=shp,
                                  criterion="dCVR", 
                                  adaptive=TRUE, 
                                  max.iterations = 10,
                                  threshold = 0.01, # consider lowering to 0.001
                                  kernel="gaussian", 
                                  bws0=c(bw.mgwr_bw.bsqsc), bw.seled=rep(T, length(ols$coefficients)),
                                  #p.vals=c(1,2,3),
                                  #dMats=list(dMat, dMat, dMat, dMat, dMat, dMat, dMat, dMat, 
                                  #           dMat, dMat, dMat, dMat, dMat, dMat, dMat, dMat),
                                  verbose = TRUE, # so you can see progress; set FALSE otherwise
                                  hatmatrix = TRUE,  # added hatmatrix TRUE to see if std errs appears
                                  #parallel.method="omp"
                                  #parallel.arg = 6
)
toc()
# mgwr_step3: 
# 6 secs on 20260618 with threshold = 0.01

mgwr_bw.bsq_ressc

####################################
# MGWR maps

mgwr1sc <- mgwr_bw.bsq_ressc 

mgwr1sc$SDF$p_share95 <- mgwr1sc$SDF$`scale(p_share)`
mgwr1sc$SDF$p_share95[abs(mgwr1sc$SDF$`scale(p_share)_TV`)<=1.96] <- 0

mgwr1sc$SDF$p_shvol95 <- mgwr1sc$SDF$`scale(p_shvol)`
mgwr1sc$SDF$p_shvol95[abs(mgwr1sc$SDF$`scale(p_shvol)_TV`)<=1.96] <- 0

mgwr1sc$SDF$p_tuvol95 <- mgwr1sc$SDF$`scale(p_tuvol)`
mgwr1sc$SDF$p_tuvol95[abs(mgwr1sc$SDF$`scale(p_tuvol)_TV`)<=1.96] <- 0

mgwr1sc$SDF$ferat95 <- mgwr1sc$SDF$`scale(ferat_2008)`
mgwr1sc$SDF$ferat95[abs(mgwr1sc$SDF$`scale(ferat_2008)_TV`)<=1.96] <- 0

mgwr1sc$SDF$Density_RD95 <- mgwr1sc$SDF$`scale(Density_RD)`
mgwr1sc$SDF$Density_RD95[abs(mgwr1sc$SDF$`scale(Density_RD)_TV`)<=1.96] <- 0

mgwr1sc$SDF$literacy95 <- mgwr1sc$SDF$`scale(literacy)`
mgwr1sc$SDF$literacy95[abs(mgwr1sc$SDF$`scale(literacy)_TV`)<=1.96] <- 0

mgwr1sc$SDF$grid_perCa95 <- mgwr1sc$SDF$`scale(grid_perCa)`
mgwr1sc$SDF$grid_perCa95[abs(mgwr1sc$SDF$`scale(grid_perCa)_TV`)<=1.96] <- 0

# add mgwr1sc values to shp
shp$p_share.mgwr1sc <- mgwr1sc$SDF$p_share95
shp$p_shvol.mgwr1sc <- mgwr1sc$SDF$p_shvol95
shp$p_tuvol.mgwr1sc <- mgwr1sc$SDF$p_tuvol95
shp$ferat.mgwr1sc <- mgwr1sc$SDF$ferat95
shp$road.mgwr1sc <- mgwr1sc$SDF$Density_RD95
shp$lit.mgwr1sc <- mgwr1sc$SDF$literacy95
shp$gpc.mgwr1sc <- mgwr1sc$SDF$grid_perCa95

# convert to sf object for graphing
shp.sf <- st_as_sf(shp)

# check order of predictors for proper bandwidths
mgwr1sc$GW.arguments$formula
mgwr1sc$GW.arguments$bws

# female ratio (ferat_2008)

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=ferat.mgwr1sc)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("female ratio, local \u03B2 (MGWR), bw = ", 
                                mgwr1sc$GW.arguments$bws[4], " (", 
                                round((mgwr1sc$GW.arguments$bws[4]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1sc_female_ratio.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# voteshare volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_shvol.mgwr1sc)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1sc$GW.arguments$bws[6], " (", 
                                round((mgwr1sc$GW.arguments$bws[6]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1sc_voteshare_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# turnout volatility

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=p_tuvol.mgwr1sc)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1sc$GW.arguments$bws[8], " (", 
                                round((mgwr1sc$GW.arguments$bws[8]/170)*100,2), "%)")) +
  theme_minimal()
g

png(file="./figures/mgwr1sc_turnout_volatility.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# literacy

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=lit.mgwr1sc)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("literacy, local \u03B2 (MGWR), bw = ", 
                                mgwr1sc$GW.arguments$bws[14], " (", 
                                round((mgwr1sc$GW.arguments$bws[14]/170)*100,2), "%)")) +
  theme_minimal()
g


png(file="./figures/mgwr1sc_lit.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


# grid density per capita

g <- ggplot(data=shp.sf) + 
  geom_sf(aes(fill=gpc.mgwr1sc)) + 
  scale_fill_gradient2(name="\u03B2", 
                       #values=rescale(c(0, .05, .2)), 
                       low="blue",
                       mid="white",
                       high="red",
                       midpoint=0,
                       guide="colourbar") + 
  labs(x="", y="", title=paste0("p_shvol, local \u03B2 (MGWR), bw = ", 
                                mgwr1sc$GW.arguments$bws[15], " (", 
                                round((mgwr1sc$GW.arguments$bws[15]/170)*100,2), "%)")) +
  theme_minimal()
g


png(file="./figures/mgwr1sc_gpc.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()


##########################################################
# SPATIAL FILTERING (to explore Griffith's 2008 critiques of GWR)
# eigenvectory spatial filtering
library(spatialreg)

# FIRST, use default for SpatialFiltering()
# note: default assumes spatial error dependence (see docs)
model_esf <- spatialreg::SpatialFiltering(
  formula = model_formula, 
  data = shp.sf, 
  nb = nb.q1, 
  style = "C",
  alpha = 0.05 # threshold for when to stop adding vectors; if clustering remains in residuals later, lower this
)

# inspect the table with extracted eigenvectors
model_esf
# output shows 5 eigenvectors, 1-5, with step 0 meaning no eigenvector added
# Step shows the order in which the eigenvector was added
# SelEvec identies which eigenvector was selected (3, 17, 6, 2, 18)
# MinMi ir Moran's I of residuals after adding the eigenvector
# ZMimMi is its z-value
# Pr is the corresponding p-value
#
#Step SelEvec  Eval  MinMi ZMinMi   Pr(ZI)    R2 gamma
#0    0       0 0.000 0.1271   3.66 0.000250 0.267   0.0
#1    1       3 0.882 0.1044   3.32 0.000907 0.288  52.0
#2    2      17 0.569 0.0827   2.91 0.003615 0.320 -63.5
#3    3       6 0.775 0.0603   2.54 0.011181 0.341 -51.9
#4    4       2 0.905 0.0372   2.18 0.029619 0.359 -47.1
#5    5      18 0.553 0.0171   1.79 0.072793 0.383 -55.3
#
# not here that after adding 5th eigenvector, p-value of Moran of residuals drops to 0.07, meaning any residual 
# autocorrelation is no longer statistically significant, at least at the 0.05 level

# clean table with this info
stargazer(model_esf$selection, type="latex", out="./tables/model_esf.tex",
          rownames = FALSE, float = FALSE)

# note that full set of values is in object: model_esf$dataset

## check against the "lag form"; note that Bivand says 
#"The lag form adds the covariates in assessment of which eigenvectors to choose, but does not use them in constructing the eigenvectors."
# from: https://cran.r-project.org/web/packages/spatialreg/vignettes/SpatialFiltering.html
# note at bottom says this vignette was part of earlier edition of R book:
# "This vignette formed pp. 302–305 of the first edition of 
# Bivand, R. S., Pebesma, E. and Gómez-Rubio V. (2008) Applied Spatial Data Analysis with R, 
# Springer-Verlag, New York. It was retired from the second edition (2013) 
# to accommodate material on other topics, and is made available in this form with the understanding of the publishers."

# eigenvector spatial filtering in lag form
model_esf_lag <- spatialreg::SpatialFiltering(
  formula = formula(Count_ ~ 1), # if lagformula used, formula should include only outcome and intercept
  lagformula = formula(~ pov_p_2008 + gini_2008 + ferat_2008 + p_share + p_shvol + 
    turninv + p_tuvol + p_ethfr + Count_3 + Density_RD + Pop_Densit + 
    Count_4 + literacy + grid_perCa), 
  data = shp.sf, 
  nb = nb.q1, 
  style = "C",
  alpha = 0.05 # threshold for when to stop adding vectors; if clustering remains in residuals later, lower this
)
model_esf_lag

# get the matrix of selected eigenvectors
egvectors <- fitted(model_esf)
egvectors_lag <- fitted(model_esf_lag)

# convert eigenvectors to df
egvectors_df <- as.data.frame(egvectors)
egvectors_lag_df <- as.data.frame(egvectors_lag)
# adjust names of vectors for lag model
names(egvectors_lag_df) <- paste(names(egvectors_lag_df), "_lag", sep="")

# Bind the eigenvectors into your main dataset
temp <- cbind(shp.sf, egvectors_df, egvectors_lag_df)


# can map these; these are called Moran Eigenvalue Maps

library(patchwork) # For placing maps side-by-side

# Map the first chosen eigenvector (e.g., "vec1")
map_vec3 <- ggplot(data = temp) +
  geom_sf(aes(fill = vec3), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 3") +
  theme_void()

# Map the second chosen eigenvector
map_vec17 <- ggplot(data = temp) +
  geom_sf(aes(fill = vec17), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 17") +
  theme_void()

# Map the third chosen eigenvector
map_vec6 <- ggplot(data = temp) +
  geom_sf(aes(fill = vec6), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 6") +
  theme_void()

# Map the fourth chosen eigenvector
map_vec2 <- ggplot(data = temp) +
  geom_sf(aes(fill = vec2), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 2") +
  theme_void()

# Map the fifth chosen eigenvector
map_vec18 <- ggplot(data = temp) +
  geom_sf(aes(fill = vec18), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 18") +
  theme_void()

# lag model: just 2 vectors selected: vec3_lag and vec23_lag

map_vec3_lag <- ggplot(data = temp) +
  geom_sf(aes(fill = vec3_lag), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 3") +
  theme_void()

# Map the second chosen eigenvector
map_vec23_lag <- ggplot(data = temp) +
  geom_sf(aes(fill = vec23_lag), color = "grey80", size = 0.1) +
  #scale_fill_viridis_c(option = "plasma", name = "Vector 3 Value") +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  #labs(title = "Vector 17") +
  theme_void()


# Display side-by-side
map_vec3 + map_vec2 + map_res

# or all 5 vectors

#g <- (map_vec3 + map_vec17 + map_vec6) / (map_vec2 + map_vec18 + map_res)
#g <- (map_vec3 + map_vec17) /  (map_vec6 + map_vec2) / (map_vec18 + map_res)
g <- wrap_plots(
  map_vec3, map_vec17,
  map_vec6, map_vec2,
  map_vec18,
  design = "
    AB
    CD
    E#
  "
)

png(file="./figures/spatialfiltermaps.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# lag model

g <- map_vec3_lag + map_vec23_lag 

png(file="./figures/spatialfiltermaps_lag.png", height=6, width=6, units="in", res=300)
print(g)
dev.off()

# re-fit the model with the chosen eigenvectors (adding them as additional parameters)

# Construct a formula that appends all selected eigenvectors
# (e.g., chosen_eigenvectors are named vec1, vec2, etc.)
final_formula <- as.formula(
  paste("Count_ ~ pov_p_2008 + gini_2008 + ferat_2008 + p_share + p_shvol + 
    turninv + p_tuvol + p_ethfr + Count_3 + Density_RD + Pop_Densit + 
    Count_4 + literacy + grid_perCa", 
        paste(colnames(egvectors), collapse = " + "), sep =" + ")
)

final_formula_lag <- as.formula(
  paste("Count_ ~ pov_p_2008 + gini_2008 + ferat_2008 + p_share + p_shvol + 
    turninv + p_tuvol + p_ethfr + Count_3 + Density_RD + Pop_Densit + 
    Count_4 + literacy + grid_perCa", 
        paste(colnames(egvectors_lag_df), collapse = " + "), sep =" + ")
)

# Run the final ESF-purged OLS regression
final_model <- lm(final_formula, data = temp)
final_model_lag <- lm(final_formula_lag, data = temp)
summary(final_model)
summary(final_model_lag)

modelsummary(models = list("error (default)" = final_model, "lag" = final_model_lag),
             estimate = "{estimate} ({std.error})", # replaces beta on first line with beta and SE
             statistic = NULL,  # removes default std. error from second line
             fmt=2, # two decimal places to save space on this big table
             coef_map = c(
               "pov_p_2008"="poverty", 
               "gini_2008"= "inequality",
               "ferat_2008"="female ratio",
               "p_share" = "NDC vote share", 
               "p_shvol" = "vote volatility",
               "turninv" = "turnout",
               "p_tuvol" = "turnout volatility",
               "p_ethfr" =  "ethnic frag.",
               "Count_3" =  "WB projects",
               "Density_RD"  = "road density",
               "Pop_Densit" = "pop. density",
               "Count_4" = "health facilities",
               "literacy"  = "literacy",
               "grid_perCa" = "grid density",
               "vec3" = "vector 3",
               "vec17" = "vector 17",
               "vec6" = "vector 6",
               "vec2" = "vector 2",
               "vec18" = "vector 18",
               "vec3_lag" = "vector 3 (lag model)",
               "vec23_lag" = "vector 23 (lag model)",
               "(Intercept)" = "Constant"),
             #gof_map = c("aic", "rmse"),
             gof_map = NA,
             stars = TRUE,
             output = "./tables/spatialfiltermodel.tex"
)

# to map data, also extract residuals
residuals_final <- residuals(final_model)
residuals_final_lag <- residuals(final_model_lag)

# add new columns to sf object 
temp <- shp.sf
temp <- cbind(temp, egvectors_df, egvectors_lag_df, residuals_final, residuals_final_lag)

# or just:
temp <- cbind(temp, residuals_final)


library(patchwork) # For placing maps side-by-side


# Map the final residuals to check for random distribution
map_res <- ggplot(data = temp) +
  geom_sf(aes(fill = residuals_final), color = "grey80", size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "res") +
  #labs(title = "Final Residuals") +
  theme_void()

# lag model
map_res_lag <- ggplot(data = temp) +
  geom_sf(aes(fill = residuals_final_lag), color = "grey80", size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "res_lag") +
  #labs(title = "Final Residuals") +
  theme_void()

# Display side-by-side
map_vec3 + map_res

# Display side-by-side
map_vec3 + map_vec2 + map_res

# or all 5 vectors

#g <- (map_vec3 + map_vec17 + map_vec6) / (map_vec2 + map_vec18 + map_res)
#g <- (map_vec3 + map_vec17) /  (map_vec6 + map_vec2) / (map_vec18 + map_res)
g <- (map_vec3 + map_vec17) /  (map_vec6 + map_vec2) / (map_vec18 + plot_spacer())

png(file="./figures/spatialfilter_res-final.png", height=6, width=6, units="in", res=300)
print(map_res)
dev.off()

png(file="./figures/spatialfilter_res-final_lag.png", height=6, width=6, units="in", res=300)
print(map_res_lag)
dev.off()


###################################

####################################
# save working data

save.image("./data/working/working20260630_models.RData")

#end
