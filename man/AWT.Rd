\name{AWT}

\alias{AWTtrain}
\alias{AWTbackground}
\alias{AWTtest}


\docType{data}

\title{Australian Wet Tropics species distribution data}

\description{
Species occurrence data for 40 species (20 plants, 20 birds) in the Australian Wet Tropics (AWT) and associated environmental data. There are three data sets with training (AWTtrain), background (AWTbackground), and test (AWTtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations of the same species, independent from the training data.


The reference system of the x and y coordinates is UTM, zone 55, spheroid grs1980, datum GDA94

Environmental variables:
\tabular{llll}{  
Code  \tab Description                \tab Units \tab Type  \cr
bc01  \tab Annual mean temperature    \tab degrees C \tab Continous  \cr
bc04  \tab Temperature seasonality    \tab dimensionless \tab Continous  \cr
bc05  \tab Max. temperature of warmest period     \tab degrees C  \tab Continous  \cr
bc06  \tab Min. temperature of coldest period     \tab  degrees C  \tab Continous  \cr
bc12  \tab Annual precipitation       \tab mm     \tab  Continous  \cr
bc15  \tab Precipitation seasonality  \tab dimensionless \tab Continous  \cr
bc17  \tab Precipitation of driest quarter \tab mm \tab Continous  \cr
bc20  \tab Annual mean radiation      \tab MJ/m2/day \tab Continous  \cr
bc31  \tab Moisture index seasonality \tab dimensionless \tab Continous \cr 
bc33  \tab Mean moisture index of lowest quarter (MI) \tab dimensionless \tab Continous \cr
slope \tab Slope                      \tab percent \tab Continous \cr
topo  \tab Topographic position       \tab 0 is a gully and 100 a ridge, 50 mid-slope  \tab Continous \cr
tri   \tab Terrain ruggedness index	  \tab Sum of variation in a 1 km moving window \tab Continous \cr
}


}

\usage{
data(AWTtrain)
data(AWTbackground)
data(AWTtest)
}

\author{Andrew Ford (CSIRO), Steven Williams, Karen Richardson (UQ Brisbane)}

\source{
Plant species records from Andrew Ford and Karen Richardson. Training data is from herbariums. These data have been cleaned and corrected by Andrew Ford for modeling by the Rainforest CRC, Townsville.  Most records are thought to be accurate to within 3 km, and were collected within the last 40 years. Presence-absence from 102 of Andrew Ford's survey sites. These have very accurate coordinate data and were collected in the last 20 years. Vertebrate data was collected by Steve Williams. Training data is from incidental surveys using GPS. Presence-absence data from field surveys. 
}

\keyword{datasets}

