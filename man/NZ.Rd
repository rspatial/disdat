\name{NZ}

\alias{NZ}

\docType{data}

\title{New Zealand species distribution data}

\description{
Species occurrence data for 52 vascular plant species - mostly trees and shrubs from indigenous forests -  in New Zealand (NZ), and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (po and bg) and test (pa, env) data:

\code{po} (training data) includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group (plant), and site values for 13 environmental variables (below).

\code{bg} (training data) has 10000 sites selected at random across the study region. It is structured identically to \code{po}, with "0" for occurrence (not implying absence, but denoting a background record in a way suited to most modelling methods) and \code{NA} for group.

\code{env} (testing data) includes group, site names, coordinates, and site values for 13 environmental variables (below), at 19120 sites. These data are suited to making predictions.

\code{pa} (testing data) includes group, site names, coordinates, and presence-absence records, one column per species. The sites are identical to the sites in \code{env}. This file is suited to evaluating the predictions made to \code{env}.

Raster (gridded) data for all environmental variables are available - see the reference below for details.

The coordinate reference system of the x and y coordinates is New Zealand Map Grid (NZMG), Datum: NZGD49 (New Zealand Geodetic Datum 1949), Ellipsoid: International 1924 (EPSG:27200).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data.

\strong{Environmental variables:}
\tabular{llll}{
\strong{Code} \tab \strong{Description} \tab \strong{Units} \tab \strong{Type} \cr

age  \tab  3 classes (0 to 2): <2000, 2000-postglacial (app. 30,000), and pre-glacial  \tab  number (category)  \tab  Categorical   \cr
deficit  \tab  Mean October vapor pressure deficit at 0900 hours  \tab  kPa  \tab  Continuous   \cr
dem  \tab  Elevation  \tab  meters  \tab  Continuous   \cr
hillshade  \tab  Hill shading (as surrogate for slope and aspect)  \tab  index of brightness  \tab  Continuous   \cr
mas  \tab  Mean annual solar radiation  \tab  Mj/m2/day  \tab  Continuous   \cr
mat  \tab  Mean annual temperature  \tab  degrees C * 10  \tab  Continuous   \cr
r2pet  \tab  Average monthly ratio of rainfall and potential evapotranspiration (ratio)  \tab  none  \tab  Continuous   \cr
rain  \tab  annual precipitation  \tab  mm  \tab  Continuous   \cr
slope  \tab  Slope  \tab  degrees    \tab  Continuous   \cr
sseas \tab Solar radiation seasonality \tab dimensionless \tab Continuous \cr
toxicats  \tab  Toxic Cations in classes: 0=low, 1=intermediate, 2=high   \tab  number (category) \tab  Categorical  \cr
tseas  \tab  Temperature seasonality  \tab  degrees C  \tab  Continuous  \cr
vpd  \tab  Mean October vapor pressure deficit at 9 AM  \tab  kPa  \tab  Continuous  \cr
}

}

\source{
Environmental predictors provided by Jake Overton. Species data supplied by Jake Overton and Susan Wiser, from Allan Herbarium and National Vegetation Survey databank.

See the reference below for further details on source, accuracy, cleaning, and particular characteristics of these datasets.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ferrier, S., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E., In press. Presence-only and presence-absence data for comparing species distribution modeling methods. Biodiversity Informatics 15:69-80.
}

\examples{
nz_po <- disPo("NZ")
nz_bg <- disBg("NZ")

nz_pa <- disPa("NZ")
nz_env <- disEnv("NZ")

x <- disData("NZ")
sapply(x, head)

disCRS("NZ")
}

\keyword{datasets}

