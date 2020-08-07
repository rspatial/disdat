\name{CAN}

\alias{CAN}

\docType{data}

\title{Canadian bird species distribution data}

\description{
Species occurrence data for 20 bird species from Ontario, a province in Canada (CAN), and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (po and bg) and test (pa, env) data:

\code{po} (training data) includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group (bird), and site values for 11 environmental variables (below).

\code{bg} (training data) has 10000 sites selected at random across the study region. It is structured identically to CANtrain_po, with "0" for occurrence (not implying absence, but denoting background in a way suited to most modelling methods) and "NA" for group.

\code{env} (testing data) includes group, site names, coordinates, and site values for 11 environmental variables (below), at 14571 sites. This file is suited to making predictions.

\code{pa} (testing data) includes group, site names, coordinates, and presence-absence records, one column per species. The sites are identical to the sites in \code{env}. This file is suited to evaluating the predictions made to \code{env}.

Raster (gridded) data for all environmental variables are available - see the reference below for details.

The reference system of the x and y coordinates is unprojected with Clarke 1866 ellipsoid . Latitude and longitude are in geographical coordinates using unknown datum based upon the Clarke 1866 ellipsoid (EPSG:4008).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data.

\strong{Environmental variables:}
\tabular{llll}{
\strong{Code} \tab \strong{Description} \tab \strong{Units} \tab \strong{Type} \cr
alt  \tab  Digital elevation   \tab  m  \tab  Continuous \cr
asp2  \tab  Aspect  \tab  ranges from -1 to 1 (sin transformation)  \tab  Continuous \cr
ontprec  \tab  Annual Precipitation  \tab  mm  \tab  Continuous \cr
ontprec4  \tab  April precipitation  \tab  mm  \tab  Continuous \cr
ontprecsd  \tab  Precipitation Seasonality  \tab  dimensionless  \tab  Continuous \cr
ontslp  \tab  Slope   \tab  degrees  \tab  Continuous \cr
onttemp  \tab  Annual mean temperature   \tab  degrees C * 10  \tab  Continuous \cr
onttempsd  \tab  Temperature standard deviation   \tab  dimensionless  \tab  Continuous \cr
onttmin4  \tab  April minimum temperature  \tab  degrees C * 10 \tab  Continuous \cr

ontveg  \tab  Vegetation, from Ontario Land Cover Database (OLC) vegetation map, derived from a mosaic of Landsat images.   \tab  5 classes: 1 = open forest & related; 2 = closed forest; 3 = open water, 4 = agriculture, 5 = human settlement  \tab  Categorical \cr
watdist  \tab  Distance from Hudson Bay  \tab  m  \tab  Continuous \cr
}
}


\source{
Environmental predictors prepared by Falk Huettmann, Jane Elith and Catherine Graham. Species data: PO from the Ontario Nest Records database, Royal Ontario Museum (ROM) and supplied by M. Peck to Falk Huettmann;  PA from Breeding Bird Atlas for Ontario, provided by M. Cadman to Falk Huettmann.

See the reference below for further details on source, accuracy, cleaning, and particular characteristics of these datasets.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ferrier, S., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E., In press. Presence-only and presence-absence data for comparing species distribution modeling methods. Biodiversity Informatics 15:69-80.
}


\examples{
can_po <- disPo("CAN")
can_bg <- disBg("CAN")

can_pa <- disPa("CAN")
can_env <- disEnv("CAN")


# Or all in one list
x <- disData("CAN")
sapply(x, head)

disCRS("CAN")

}

\keyword{datasets}

