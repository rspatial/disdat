## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE, eval = FALSE, tidy.opts=list(width.cutoff=80), fig.align="center")

## ----eval=TRUE----------------------------------------------------------------
# install.packages('disdat')
# devtools::install_github('rspatial/disdat')
library(disdat)


## ----eval=TRUE, paged.print=TRUE----------------------------------------------
awt <- disBg("AWT")
head(awt)

## -----------------------------------------------------------------------------
#  library(GGally)
#  
#  ggcorr(awt[, 7:ncol(awt)],
#         method = c("pairwise", "spearman"),
#         label = TRUE,
#         label_size = 3,
#         label_color = "white",
#         digits = 2) +
#    theme(legend.justification = c(1, 0),
#          legend.position = c(0.5, 0.7),
#          legend.direction = "horizontal") +
#    guides(fill = guide_colorbar(barwidth = 9,
#                                 barheight = 1,
#                                 title.position = "top",
#                                 title.hjust = 0.5,
#                                 title = "Spearman correlation"))

## ---- eval=TRUE, out.width = 600, echo=F, fig.align="center"------------------
knitr::include_graphics("../man/figures/corr_plot.png")

## -----------------------------------------------------------------------------
#  library(ggplot2)
#  

## ----eval=TRUE----------------------------------------------------------------
# create density plot for species presence-only vs background data
# first prepare the species records in the right format for the tidyverse package:
po <- disPo("AWT") # presence-only data
bg <- disBg("AWT") # background data
spdata <- rbind(po, bg)
spdata$occ <- as.factor(spdata$occ)
levels(spdata$occ) <- c("Landscape", "Species")
levels(spdata$occ)


## -----------------------------------------------------------------------------
#  # now plot the data - specify the variable by its column name, as seen below in "aes(x = bc01,.....)"
#  ggplot(data = spdata, aes(x = bc01, fill = occ)) +
#    geom_density(alpha = 0.4) +
#    xlab("Annual mean temperature") +
#    scale_fill_brewer(palette = "Dark2") +
#    guides(fill = guide_legend(title = "")) +
#    theme_bw()
#  

## ---- eval=TRUE, out.width = 600, echo=F, fig.align="center"------------------
knitr::include_graphics("../man/figures/density_plot.png")

## -----------------------------------------------------------------------------
#  library(sf)
#  library(tidyverse)
#  
#  # presence-only data
#  awt_sf <- st_as_sf(disPo("AWT"), coords = c("x", "y"), crs = 28355)
#  
#  # a function to calculate the min distance (excluding self-distance)
#  mindist <- function(x) {
#    mindis <- vector(mode = "numeric", length = nrow(x))
#    for(i in 1:nrow(x)){
#      mindis[i] <- min(x[i, -i])
#    }
#    return(mindis)
#  }
#  
#  awt_min_dist <- awt_sf %>%
#    group_by(spid) %>%
#    nest() %>%
#    mutate(distM = map(data, ~st_distance(x = .)),
#           minDist = map(distM, ~mindist(x = .)),
#           meanDist = map_dbl(minDist, ~mean(x = .)))
#  

## ---- eval=TRUE, out.width = 400, echo=FALSE----------------------------------
knitr::include_graphics("../man/figures/boxplot.jpeg")

## -----------------------------------------------------------------------------
#  library(disdat)
#  library(sf)
#  library(tmap)
#  library(grid)
#  
#  # loading the data
#  r <- disBorder("NSW") # a polygon file showing border of the region
#  podata <- disPo("NSW") # presence-only data
#  padata <- disPa("NSW", "db") # presence-absence of group db for species 'nsw14'
#  
#  # select the species to plot
#  species <- "nsw14"
#  # convert the data.frame to sf objects for plotting
#  po <- st_as_sf(podata[podata$spid == species, ], coords = c("x", "y"), crs = 4326) # subset the species
#  pa <- st_as_sf(padata, coords = c("x", "y"), crs = 4326)
#  
#  # create map showing training data
#  train_sample <- tm_shape(r) + # create tmap object
#    tm_polygons() + # add the border
#    tm_shape(po) + # create tmap object for the points to overlay
#    tm_dots(size = 0.2, col = "blue", alpha = 0.6, legend.show = FALSE) + # add the points
#    tm_compass(type = "8star", position = c("left", "top")) + # add north arrow
#    tm_layout(main.title = "Training data", main.title.position = "center") + # manage the layout
#    tm_grid(lwd = 0.2, labels.inside.frame = FALSE, alpha = 0.4, projection = 4326, # add the grid
#            labels.format = list(big.mark = ",", fun = function(x){paste0(x, "º")})) # add degree symbol
#  
#  # create map showing testing data
#  pa[,species] <- as.factor(padata[,species])
#  test_sample <- tm_shape(r) +
#    tm_polygons() +
#    tm_shape(pa) +
#    tm_dots(species, size = 0.2, palette = c("red", "blue"), title = "Occurrence", alpha = 0.6) +
#    tm_layout(main.title = "Testing data", main.title.position = "center") +
#    tm_grid(lwd = 0.2, labels.inside.frame = FALSE, alpha = 0.4, projection = 4326,
#            labels.format = list(big.mark = ",", fun = function(x){paste0(x, "º")}))
#  
#  # create a layout for putting the maps side-by-side
#  grid.newpage()
#  pushViewport(viewport(layout=grid.layout(1,2)))
#  print(train_sample, vp=viewport(layout.pos.col = 1))
#  print(test_sample, vp=viewport(layout.pos.col = 2))
#  

## ----eval=TRUE, echo=FALSE----------------------------------------------------
knitr::include_graphics("../man/figures/tmap.png")

## ----eval=TRUE, fig.height=5, fig.width=6, message=FALSE, warning=FALSE-------
library(mapview)
library(sf)

# get presence-absence data
padata <- disPa(region = "NSW", group = "db") 
# use disCRS for each region for its crs code (EPSG)
disCRS("NSW", format = "EPSG")
# convert the data.frame to sf objects for plotting
pa <- st_as_sf(padata, coords = c("x", "y"), crs = 4326)
# select a species to plot
species <- "nsw14"

# plot presence-absence data
# you can add: map.types = "Esri.WorldImagery"
mapview(pa, zcol = species)


