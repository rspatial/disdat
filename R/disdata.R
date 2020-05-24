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
	x
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


disBorder <- function(region, pkg="sf"){

	region <- .checkRegion(region)
	r <- tolower(region)
	path <- .pkgPath("borders")
	
	d <- file.path(path, paste0(r, ".gpkg"))
	if (pkg == "sf") {
		bor <- sf::st_read(d, quiet = TRUE)
	} else if (pkg == "sp") {
		bor <- rgdal::readOGR(d, verbose=FALSE)
	} else {
		bor <- terra::vect(d, quiet = TRUE)		
	}
	return(bor)
}


disPredictors <- function(region) {
	region <- .checkRegion(region)
	f <- file.path(.pkgPath(), paste0(region, "train_po.rds"))
	colnames(readRDS(f))[-c(1:6)]

	#if (region=="AWT") {
	#	return( c("bc01", "bc04", "bc05", "bc06", "bc12", "bc15", "bc17", "bc20", "bc31", "bc33", "slope", "topo", "tri") )
	#} else if (region=="CAN") {
	#	return( c("alt", "asp2", "ontprec", "ontprec4", "ontprecsd", "ontslp", "onttemp", "onttempsd", "onttmin4", "ontveg", "watdist") )
	#} else if (region=="NSW") {
	#	return( c("disturb", "mi", "rainann", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys", "cti", "raindq", "tempmin" ))
	#} else if (region=="NZ") { 
	#	return( c("age", "deficit", "dem", "hillshade", "mas", "mat", "r2pet", "rain", "slope", "sseas", "toxicats", "tseas", "vpd")	)
	#} else if (region=="SA") {
	#	return( c("sabio1", "sabio2", "sabio4", "sabio5", "sabio6", "sabio7", "sabio8", "sabio12", "sabio15", "sabio17", "sabio18") )
	#} else if (region=="SWI") {
	#	return( c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "tavecc", "topo") )
	#}
}


disCRS <- function(region) {
	region <- .checkRegion(region)
	if (region=="AWT") {
		"+proj=utm +zone=55 +south +ellps=GRS80"
	} else {
		"+proj=longlat +datum=WGS84"
	}
}

