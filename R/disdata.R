# Author: Robert J. Hijmans and Roozbeh Valavi
# contact: valavi.r@gmail.com
# Date : December 2019
# Version 0.1
# Licence GPL v3

.checkRegion <- function(region)  {
	regions <- c("AWT", "CAN", "NSW", "NZ", "SA", "SWI")
	region <- toupper(region[1])
	if (! region %in% regions) {
		stop("unknown region: ", region, ". Should be one of: ", paste(regions, collapse = ", "))
	}
	region
}

.pkgPath <- function(subdir="") {
	system.file(file.path("extdata", subdir), package="disdat")
}


.reshape_pa <- function(x) {
	r <- stats::reshape(x, varying=colnames(x)[-c(1:3)], times=colnames(x)[-c(1:3)], v.names="pa", timevar="spid", direction="long")
	r$id = NULL
	rownames(r) <- NULL
	r
}	

disData <- function(region) {

	region <- .checkRegion(region)
	path <- .pkgPath()
	f <- list.files(pattern=region, path, full.names=TRUE)
	nms <- gsub("\\.rds", "", basename(f))
	nms <- gsub(region, "", nms)
	nms <- gsub("test_", "", nms)
	nms <- gsub("train_", "", nms)
		
	if (length(f) == 4) {
		x <- lapply(f, readRDS)
		names(x) <- nms
		i <- which(nms == "pa")
		x[[i]] <- .reshape_pa(x[[i]]) 
	} else {
		fe <- grep("_env", f, value=TRUE)
		env <- lapply(fe, readRDS)
		env <- do.call(rbind, env)
		
		fa <- grep("_pa", f, value=TRUE)
		pa <- lapply(fa, readRDS)
		pa <- lapply(pa, .reshape_pa)
		pa <- do.call(rbind, pa)

		bg <- readRDS(grep("train_bg", f, value=TRUE))
		po <- readRDS(grep("train_po", f, value=TRUE))
		x <- list(env=env, pa=pa, bg=bg, po=po) 			
	}
}


getDisData <- function(region, dataset, type, group = NULL) {

	region <- .checkRegion(region)
	dataset <- tolower(dataset[1])
	type <- tolower(type[1])
	if(dataset == "train") {
		group <- NULL
	}
	.checkRegion(region)
	path <- .pkgPath()

	datasets <- c("train", "test")
	if (! dataset %in% datasets ) {
		stop("unknown dataset: ", dataset, ". Shoud be one of: ", paste(datasets, collapse = ", "))
	}
	if(dataset == "train" && ! type %in% c("po", "bg")){
		stop("unknown type for train dataset", ". Shoud be one of: ", paste(c("po", "bg"), collapse = ", "))
	}
	if(dataset == "test" && ! type %in% c("env", "pa")){
		stop("unknown type for test dataset", ". Shoud be one of: ", paste(c("pa", "env"), collapse = ", "))
	}
	if(region %in% c("AWT", "NSW") && dataset == "test" && is.null(group)){
		stop("You should specify a group for test dataset in AWT and NSW regions")
	}
	
	if(!is.null(group)){
		group <- tolower(group[1])
		groupAWT <- c("bird", "plant")
		if(region == "AWT" && ! group %in% groupAWT) {
			stop("unknown group for AWT region: ", group, ". Shoud be one of: ", paste(groupAWT, collapse = " "))
		}
		groupNSW <- c("ba", "db", "nb", "ot", "ou", "rt", "ru", "sr")
		if(region == "NSW" && ! group %in% groupNSW){
			stop("unknown group for NSW region: ", group, ". Shoud be one of: ", paste(groupNSW, collapse = " "))
		}
	}
	
	if(is.null(group)){
		x <- file.path(path, paste0(region, dataset, "_", type, ".rds"))
	} else{
		x <- file.path(path, paste0("/", region, dataset, "_", type, "_", group, ".rds"))
	}
	
	readRDS(x)
}



disPo <- function(region) { getDisData(region, "train", "po") }
disBg <- function(region) { getDisData(region, "train", "bg") }
disPa <- function(region, group) { getDisData(region, "test", "pa", group) }
disEnv <- function(region, group) { getDisData(region, "test", "env", group) }

