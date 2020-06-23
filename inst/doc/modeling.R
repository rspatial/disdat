## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE, tidy.opts=list(width.cutoff=80))

## ---- out.width = 600, echo=F, fig.align="center"-----------------------------
knitr::include_graphics("../man/figures/mapbook_eg.png")

## ----eval=FALSE---------------------------------------------------------------
#  # loading the library
#  library(disdat)
#  library(forcats) # for handling factor variables
#  library(randomForest)

## ----eval=FALSE---------------------------------------------------------------
#  # set output directory for prediction csv files:
#  outdir <- "~/Desktop/modeling_nceas"
#  # provide names for regions to be modeled - here we model all 6:
#  regions <- c("AWT", "CAN", "NSW", "NZ", "SA", "SWI")
#  # specify names of all categorical variables across all regions:
#  categoricalvars <- c("ontveg", "vegsys", "toxicats", "age", "calc")
#  
#  # check the model's output directory
#  if(file.exists(outdir)){
#    print("The directory already exists!")
#  } else{
#    dir.create(file.path(outdir))
#    print("The directory is created.")
#  }

## ----eval=FALSE---------------------------------------------------------------
#  n <- 0
#  for(r in regions){
#    # reading presence-only and background species data for this region, one file per region:
#    presences <- disPo(r)
#    background <- disBg(r)
#  
#    # extract names for all species
#    species <- unique(presences$spid)
#  
#    # now for each species, prepare data for modeling and evaluation
#    # with randomForests, model, predict to the evaluation file with
#    # environmental data.
#    for(s in species){
#      # subset presence records of species for this species
#      sp_presence <- presences[presences$spid == s, ]
#      # add background data
#      pr_bg <- rbind(sp_presence, background)
#  
#      # find the evaluation file – for some regions this means identifying the taxonomic group
#      if (r %in% c("AWT", "NSW")) {
#        grp <- sp_presence[, "group"][1]
#        evaluation <- disEnv(r, grp)
#      } else {
#        evaluation <- disEnv(r)
#      }
#  
#      # convert categorical vars to factor in both training and testing data. We use the package forcats to ensure that the levels of the factor in the evaluation data match those in the training data, regardless of whether all levels are present in the evaluation data.
#      for(i in 1:ncol(pr_bg)){
#        if(colnames(pr_bg)[i] %in% categoricalvars){
#          fac_col <- colnames(pr_bg)[i]
#          pr_bg[ ,fac_col] <- as.factor(pr_bg[ ,fac_col])
#          evaluation[ ,fac_col] <- as.factor(evaluation[ ,fac_col])
#          evaluation[ ,fac_col] <- forcats::fct_expand(evaluation[,fac_col], levels(pr_bg[,fac_col]))
#          evaluation[ ,fac_col] <- forcats::fct_relevel(evaluation[,fac_col], levels(pr_bg[,fac_col]))
#        }
#      }
#  
#      # extract the relevant columns for modelling
#      model_data <- pr_bg[, c("occ", disPredictors(r))]
#      # convert the response to factor for RF model to return probabilities
#      model_data$occ <- as.factor(model_data$occ)
#  
#      # start modeling! We use the "try" notation so if a species fails to fit, the loop will continue.
#      n <- n + 1
#      if(inherits(try(
#        mod <- randomForest(occ ~ ., data = model_data, ntree = 500)
#      ), "try-error")){print(paste("Error for species", s, "from", r))}
#  
#      # make and save the prediction
#      out_file <- evaluation[,1:3]
#      out_file$spid <- sp_presence$spid[1]
#      out_file$group <- sp_presence$group[1]
#      out_file$prediction <- predict(mod, evaluation, type = "prob")[,"1"] # prediction for presences
#      write.csv(out_file, paste(outdir, "/", r, "_", s, "_rf", ".csv", sep=""), row.names = FALSE)
#      print(n)
#    }
#  }
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#  library(ROCR)
#  
#  # specify the output folder that contains species predictions, and in which to save the models results
#  modelsdir <- "~/Desktop/modeling_nceas"
#  
#  # get the names of all the prediction files
#  sp_preds <- list.files(modelsdir, pattern = ".csv$", full.names = FALSE)
#  
#  # a data.frame to save the evaluation result
#  rf_eval <- data.frame(region = rep(NA, length(sp_preds)),
#                        species = NA,
#                        auc = NA)
#  
#  # now a loop to evaluate each species, one at a time:
#  n <- 0
#  for(i in sp_preds){
#    pred <- read.csv(file.path(modelsdir, i), stringsAsFactors = FALSE)
#    # get information on the species (s) and region (r) for each file:
#    r <- unlist(strsplit(i, "_"))[1]
#    s <- unlist(strsplit(i, "_"))[2]
#  
#    # now evaluate predictions, finding the right column for this species
#    # find the evaluation file – for some regions this means identifying the taxonomic group
#    if (r %in% c("AWT", "NSW")) {
#      grp <- pred[pred$spid == s, "group"][1]
#      evals <- disPa(r, grp)
#    } else {
#      evals <- disPa(r)
#    }
#  
#    n <- n + 1
#    perf <- prediction(pred$prediction, evals[, s]) %>%
#      performance("auc")
#    rf_eval$region[n] <- r
#    rf_eval$species[n] <- s
#    rf_eval$auc[n] <- as.numeric(perf@y.values)
#    print(s)
#  }
#  
#  mean(rf_eval$auc)
#  hist(rf_eval$auc)
#  
#  # auc mean for region
#  rf_eval %>%
#    group_by(region) %>%
#    summarise(mean(auc))
#  

## ----warning=F, message=F-----------------------------------------------------
library(raster)

## ----eval=F-------------------------------------------------------------------
#  po <- disPo("NSW")
#  nsw.raster <- raster("env_grids/nsw/mi.tif")

## ----message=F, eval=F, collapse=T--------------------------------------------
#  table(po$group)
#  target.groups <- list("bat"= "ba", "bird"= c("db", "nb"), "plant"= c("ot", "ou", "rt", "ru"), "reptile"= "sr")

## ----warning=F, message=F, eval=F---------------------------------------------
#  tgb.data <- list()
#  
#  for(i in 1:4){
#    getgroup <- match(po$group, target.groups[[i]])
#    dat <- po[!is.na(getgroup),]
#    samplecellID <- cellFromXY(nsw.raster,dat[,c("x","y")])
#    dup <- duplicated(samplecellID)
#    tgb.data[[i]] <- dat[!dup,]
#  }
#  
#  names(tgb.data) <- names(target.groups)

