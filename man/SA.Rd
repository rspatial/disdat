\name{SA}

\alias{SA}

\docType{data}

\title{South American plant species distribution data}

\description{
Species occurrence data for 30 vascular plant species (all from the Bignoniaceae family) from Continental Brazil, Ecuador, Colombia, Bolivia, and Peru, South America (SA), and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (po and bg) and test (pa, env) data:

\code{po} includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group (plant), and site values for 11 environmental variables (below). 

\code{bg} has 10000 sites selected at random across the study region. It is structured identically to \code{po}, with "0" for occurrence (not implying absence, but denoting background in a way suited to most modelling methods) and "NA" for group. 

\code{env} includes site names, coordinates, and site values for 11 environmental variables (below), at 152 sites. This file is suited to making predictions. 

\code{pa} includes site names, coordinates, and presence-absence records, one column per species. The sites are identical to the sites in \code{env}. This file is suited to evaluating the predictions made to \code{env}.

Raster (gridded) data for all environmental variables are available - see the reference below for details. 

The coordinate reference system of the x and y coordinates is longitude, latitude, with the WGS84 datum (EPSG:4326).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data. 


\strong{Environmental variables (extracted from WorldClim):}
\tabular{llll}{  
\strong{Code} \tab \strong{Description} \tab \strong{Units} \tab \strong{Type} \cr

sabio1  \tab  Annual mean temperature    \tab  degrees C * 10  \tab  Continuous  \cr
sabio2  \tab  Mean Diurnal Range (Mean of monthly (max temp - min temp))  \tab  degrees C * 10  \tab  Continuous  \cr
sabio4  \tab  Temperature Seasonality (standard deviation *100)  \tab  dimensionless  \tab  Continuous  \cr
sabio5  \tab  Max Temperature of Warmest Month  \tab  degrees C * 10  \tab  Continuous  \cr
sabio6  \tab  Min Temperature of Coldest Month  \tab  degrees C * 10  \tab  Continuous  \cr
sabio7  \tab  Temperature Annual Range \tab  degrees C * 10  \tab  Continuous  \cr
sabio8  \tab  Mean Temperature of Wettest Quarter   \tab  mm  \tab  Continuous  \cr
sabio12  \tab  Annual Precipitation  \tab  mm  \tab  Continuous  \cr
sabio15  \tab  Precipitation Seasonality (Coefficient of Variation)  \tab  mm  \tab  Continuous  \cr
sabio17  \tab  Precipitation of Driest Quarter  \tab  mm  \tab  Continuous  \cr
sabio18  \tab  Precipitation of Warmest Quarter  \tab  mm  \tab  Continuous  \cr
}
}


\source{
Environmental data prepared by Bette Loiselle, Lucia Lohmann and Catherine Graham. Species supplied by Bette Loiselle and Lucia Lohmann. PO data from the Missouri Botanical Gardens database and Lucia Lohmann; PA data collected by Al Gentry.

See the reference below for further details on source, accuracy, cleaning, and particular characteristics of these datasets.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E., In press. Presence-only and presence-absence data for comparing species distribution modeling methods. Biodiversity Informatics.
}


\examples{
x <- disData('SA')
sapply(x, head)
}

\keyword{datasets}

