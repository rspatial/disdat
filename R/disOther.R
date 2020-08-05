
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
}


disCRS <- function(region, format="proj4") {
	region <- .checkRegion(region)
	if (region=="AWT") {
		ifelse(format == "proj4",
		"+proj=utm +zone=55 +south +ellps=GRS80",
		"EPSG:28355")
	} else if (region=="CAN") {
		ifelse(format == "proj4",
		"+proj=longlat +ellps=clrk66",
		"EPSG:4008")
	} else if (region=="NZ") {
		ifelse (format=="proj4",
		"+proj=nzmg +lat_0=-41 +lon_0=173 +x_0=2510000 +y_0=6023150 +ellps=intl +towgs84=59.47,-5.04,187.44,0.47,-0.1,1.024,-4.5993 +units=m",
		"EPSG:27200")
	} else if (region=="SWI") {
	  ifelse (format=="proj4",
	          "+proj=tmerc +lat_0=46.95228333333333 +lon_0=7.439583333333333 +k=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs",
	          "EPSG:21781")
	} else {
		ifelse (format=="proj4",
		"+proj=longlat +datum=WGS84",
		"EPSG:4326")
	}
}

