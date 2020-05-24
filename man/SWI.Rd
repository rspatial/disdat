\name{SWI}

\alias{SWI}

\docType{data}

\title{Swiss species distribution data}

\description{
Species occurrence data for 30 tree species in Switzerland (SWI, a country in Europe) and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (SWItrain_po and SWItrain_bg) and test (SWItest_pa, SWItest_env) data:

SWItrain_po includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group (tree), and site values for 13 environmental variables (below). Records for any one species can be extracted by selecting the relevant rows.

SWItrain_bg has 10000 sites selected at random across the study region. It is structured identically to SWItrain_po, with "0" for occurrence (not implying absence, but denoting background in a way suited to most modelling methods) and "NA" for group. 

SWItest_env includes site names, coordinates, and site values for 13 environmental variables (below), at 10103 sites. This file is suited to making predictions. 

SWItest_pa includes site names, coordinates, and presence-absence records, one column per species. The sites are identical, and in the same order, as SWItest_env. This file is suited to evaluating the predictions made to SWItest_env.

Raster (gridded) data for all environmental variables are available - see the reference below for details. 

The reference system of the x and y coordinates is Transverse, spheroid Bessel  (EPSG:21781) (note all SWI data has a constant shift applied).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data. 

\strong{Environmental variables:}
\tabular{llll}{  
\strong{Code} \tab \strong{Description} \tab \strong{Units} \tab \strong{Type} \cr
 
 bcc  \tab  Broadleaved continuous cover  (based on Landsat images)  \tab  percentage  \tab  Continuous \cr
calc  \tab  Bedrock is strictly calcareous  \tab 1 (yes) or 0 (no)  \tab  Categorical  \cr
ccc  \tab  Coniferous continuous cover  (based on Landsat images) \tab  percentage   \tab  Continuous  \cr 

ddeg  \tab  Growing degree-days above a threshold of 0 degrees C  \tab  degrees C * days  \tab  Continuous   \cr

nutri  \tab  Soil nutrients index between 0-45  \tab  D mval/cm2  \tab  Continuous   \cr

pdsum  \tab  Number of days with rainfall higher than 1 mm   \tab  ndays  \tab  Continuous   \cr

precyy  \tab  Average yearly precipitation sum   \tab  mm  \tab  Continuous   \cr

sfro  \tab  Summer Frost Frequency \tab  days  \tab  Continuous   \cr

slope \tab  Slope  \tab  degrees x 10  \tab  Continuous   \cr

sradyy  \tab  Potential yearly global radiation (daily average)   \tab  (kJ/m2)/day  \tab  Continuous   \cr

swb  \tab  Site water balance  \tab  mm  \tab  Continuous   \cr

tavecc  \tab  Average temperature of the coldest month   \tab  degrees C  \tab  Continuous   \cr

topo  \tab  Topographic position  \tab  dimensionless   \tab  Continuous   \cr
}
}


\source{
Environmental predictors supplied by Niklaus E. Zimmermann. Species data supplied by Niklaus E. Zimmermann, Thomas Wohlgemuth and Meinrad Abegg.

See the reference below for further details on source, accuracy, cleaning, and particular characteristics of these datasets.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E., In press. Presence-only and presence-absence data for comparing species distribution modeling methods. Biodiversity Informatics.
}


\examples{
library(disdat)
x <- disData("SWI")
sapply(x, head)
}


\keyword{datasets}

