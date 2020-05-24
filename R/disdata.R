# Author: Robert J. Hijmans and Roozbeh Valavi
# contact: valavi.r@gmail.com
# Date : December 2019
# Version 0.1
# Licence GPL v3

disData <- function(...){
  args <- unique(toupper(unlist(list(...))))
  if(length(args) == 0){ 
    args = 'ALL' 
  }
  if(args[1] == 'ALL'){
    args <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
  }
  path <- system.file("extdata", package="disdat")
  for(i in 1:length(args)){
    r <- toupper(args[[i]])
    if(! r %in% c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')){
      warning('unknown region: ', r)
    } else{
      dd <- list.files(path)[grep(r, list.files(path))]
      if(length(dd) > 4) message("Notice the test dataset for ", r, " region has more than one group")
      for(j in 1:length(dd)){
        d <- paste0(path, '/', dd[j])
        res <- readRDS(d)
        objname <- substr(dd[j], 1, nchar(dd[j])-4)
        assign(objname, res, envir = .GlobalEnv)
      }
    }
  }
}


getDisData <- function(region, dataset, type, group = NULL) {
  region <- toupper(region[1])
  dataset <- tolower(dataset[1])
  type <- tolower(type[1])
  if(dataset == 'train'){group <- NULL}
  regions <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
  if (! region %in% regions) {
    stop('unknown region: ', region, '. Should be one of: ', paste(regions, collapse = " "))
  }
  datasets <- c('train', 'test')
  if (! dataset %in% datasets ) {
    stop('unknown dataset: ', dataset, '. Shoud be one of: ', paste(datasets, collapse = " "))
  }
  if(dataset == 'train' && ! type %in% c('po', 'bg')){
    stop('unknown type for train dataset', '. Shoud be one of: ', paste(c('po', 'bg'), collapse = " "))
  }
  if(dataset == 'test' && ! type %in% c('env', 'pa')){
    stop('unknown type for test dataset', '. Shoud be one of: ', paste(c('pa', 'env'), collapse = " "))
  }
  if(region %in% c('AWT', 'NSW') && dataset == 'test' && is.null(group)){
    stop('You should specify a group for test dataset in AWT and NSW regions')
  }
  if(!is.null(group)){
    group <- tolower(group[1])
    groupAWT <- c('bird', 'plant')
    if(region == 'AWT' && ! group %in% groupAWT){
      stop('unknown group for AWT region: ', group, '. Shoud be one of: ', paste(groupAWT, collapse = " "))
    }
    groupNSW <- c('ba', 'db', 'nb', 'ot', 'ou', 'rt', 'ru', 'sr')
    if(region == 'NSW' && ! group %in% groupNSW){
      stop('unknown group for NSW region: ', group, '. Shoud be one of: ', paste(groupNSW, collapse = " "))
    }
  }
  path <- system.file("extdata", package="disdat")
  if(is.null(group)){
    x <- paste0(path, '/', region, dataset, '_', type, '.rds')
  } else{
    x <- paste0(path, '/', region, dataset, '_', type, '_', group, '.rds')
  }
  #thisenvir = new.env()
  #d <- get(load(x, thisenvir), thisenvir)
  d <- readRDS(x)
  return(d)
}


getBorder <- function(region){
  r <- tolower(region)
  path <- system.file("extdata/borders", package="disdat")
  if(! r %in% c('awt', 'can', 'nsw', 'nz', 'sa', 'swi')){
    warning('unknown region: ', r)
  } else{
    dd <- list.files(path)[grep(r, list.files(path))]
    d <- paste0(path, '/', dd)
    bor <- sf::st_read(d, quiet = TRUE)
    return(bor)
  }
}


predictors <- function(region) {
	regions = c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
	if (! region %in% regions) {
		stop('unknown region: ', region, '. Should be one of: ', regions)
	} else if (region=='AWT') {
		return( c('bc01', 'bc04', 'bc05', 'bc06', 'bc12', 'bc15', 'bc17', 'bc20', 'bc31', 'bc33', 'slope', 'topo', 'tri') )
	} else if (region=='CAN') {
		return( c('alt', 'asp2', 'ontprec', 'ontprec4', 'ontprecsd', 'ontslp', 'onttemp', 'onttempsd', 'onttmin4', 'ontveg', 'watdist') )
	} else if (region=='NSW') {
		return( c("disturb", "mi", "rainann", "rugged", "soildepth", "soilfert", "solrad", "tempann", "topo", "vegsys", "cti", "raindq", "tempmin" ))
	} else if (region=='NZ') { 
		return( c('age', 'deficit', 'dem', 'hillshade', 'mas', 'mat', 'r2pet', 'rain', 'slope', 'sseas', 'toxicats', 'tseas', 'vpd')	)
	} else if (region=='SA') {
		return( c('sabio1', 'sabio2', 'sabio4', 'sabio5', 'sabio6', 'sabio7', 'sabio8', 'sabio12', 'sabio15', 'sabio17', 'sabio18') )
	} else if (region=='SWI') {
		return( c("bcc", "calc", "ccc", "ddeg", "nutri", "pday", "precyy", "sfroyy", "slope", "sradyy", "swb", "tavecc", "topo") )
	}
}

