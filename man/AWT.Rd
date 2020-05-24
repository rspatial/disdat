\name{AWT}

\alias{AWT}
%\alias{AWTtrain}
%\alias{AWTbackground}
%\alias{AWTtest}


\docType{data}

\title{Australian Wet Tropics species distribution data}

\description{
Species occurrence data for 40 species (20 vascular plants, 20 birds) in the Australian Wet Tropics (AWT) and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (AWTtrain_po and AWTtrain_bg) and test (AWTtest_pa, AWTtest_env) data:

AWTtrain_po includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group (plant or bird), and site values for 13 environmental variables (below). Records for any one species can be extracted by selecting the relevant rows.

AWTtrain_bg has 10000 sites selected at random across the study region. It is structured identically to AWTtrain_po, with "0" for occurrence (not implying absence, but denoting background in a way suited to most modelling methods) and "NA" for group. 

AWTtest_env includes site names, coordinates, and site values for 13 environmental variables (below). Since these survey data come from different surveys for plants (102 sites) and birds (340 sites), there are two separate files that can be called from the \code{disData} and \code{getDisData} functions.  These files are suited to making predictions. 

AWTtest_pa includes site names, coordinates, and presence-absence records, one column per species. The sites are identical, and in the same order, as AWTtest_env, and again there are separate files for each group. These files are suited to evaluating the predictions made to AWTtest_env.  

Raster (gridded) data for all environmental variables are available - see the reference below for details. 

The reference system of the x and y coordinates is UTM, zone 55, spheroid GRS 1980, datum GDA94 (EPSG:28355).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data. 

\strong{Environmental variables:}
\tabular{llll}{  
\strong{Code}  \tab \strong{Description}  \tab \strong{Units} \tab \strong{Type}  \cr
bc01  \tab Annual mean temperature    \tab degrees C \tab Continuous  \cr
bc04  \tab Temperature seasonality    \tab dimensionless \tab Continuous  \cr
bc05  \tab Max. temperature of warmest period     \tab degrees C  \tab Continuous  \cr
bc06  \tab Min. temperature of coldest period     \tab  degrees C  \tab Continuous  \cr
bc12  \tab Annual precipitation       \tab mm     \tab  Continuous  \cr
bc15  \tab Precipitation seasonality  \tab dimensionless \tab Continuous  \cr
bc17  \tab Precipitation of driest quarter \tab mm \tab Continuous  \cr
bc20  \tab Annual mean radiation      \tab MJ/m2/day \tab Continuous  \cr
bc31  \tab Moisture index seasonality \tab dimensionless \tab Continuous \cr 
bc33  \tab Mean moisture index of lowest quarter (MI) \tab dimensionless \tab Continuous \cr
slope \tab Slope                      \tab percent \tab Continuous \cr
topo  \tab Topographic position       \tab 0 is a gully and 100 a ridge, 50 mid-slope  \tab Continuous \cr
tri   \tab Terrain ruggedness index	  \tab Sum of variation in a 1 km moving window \tab Continuous \cr
}


}

\usage{
disData('AWT')
}

\author{
Environmental predictors prepared by Karen Richardson, Caroline Bruce and Catherine Graham. Species data supplied by Andrew Ford, Stephen Williams and Karen Richardson.
}

\source{
See reference below for further details on source, accuracy, cleaning, and particular characteristics of these datasets.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E. (in prep) Presence-only and presence-absence data for comparing species distribution modeling methods. Scientific Data.
}

\keyword{datasets}

