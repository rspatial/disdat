# Author: Robert J. Hijmans
# contact: r.hijmans@gmail.com
# Date : March 2010
# Version 0.1
# Licence GPL v3

disData <- function(...) {
	args <- unique(toupper(unlist(list(...))))
	if (length(args) == 0) { 
		args = 'ALL' 
	}
	if (args[1] == 'ALL') {
		args <- c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
	}
	path = system.file("data", package="disdat")
	for (i in 1:length(args)) {
		r = toupper(args[[i]])
		if (! r %in% c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')) {
			warning('unknown region:', r)
		} else {
			dd <- c('train', 'background', 'test')
			for (j in 1:3) {
				d <- paste0(path, '/', r, dd[i], '.rds')
				res <- readRDS(d[j])
				objname <- paste0(r, dd[i])
				assign(objname, res, envir = .GlobalEnv)
			}
		}
	}
}


getDisData <- function(region, type) {
	region = toupper(region[1])
	type = tolower(type[1])
	regions = c('AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI')
	if (! region %in% regions) {
		stop('unknown region: ', region, '. Should be one of: ', regions)
	}
	types = c('train', 'test', 'background')
	if (! type %in% types ) {
		stop('unknown data type: ', type, '. Shoud be one of: ', types)
	}
	path = system.file("data", package="disdat")
	x = paste(path, '/', region, type, '.rds', sep='')
	#thisenvir = new.env()
	#d <- get(load(x, thisenvir), thisenvir)
	d <- readRDS(x)
	return(d)
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

