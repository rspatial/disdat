\name{NZ}

\alias{NZtrain}
\alias{NZbackground}
\alias{NZtest}

\docType{data}

\title{New Zealand species distribution data}

\description{
Species occurence data for 52 plant species in New Zealand (NZ), mostly fro trees and shrubs from indigenous forests and shrub, and associated environmental data. There are three data sets with training (NZtrain), background (NZbackground), and test (NZtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the NZme species, independent from the training data.


\tabular{llll}{  
Code \tab Description \tab Units \tab Type\cr
age  \tab  3 classes <2000, 2000-postglacial (app. 30,000), and pre-glacial  \tab  none  \tab  Categorical   \cr
defecit  \tab  annual rainfall deficit in relation to potenital evapotranspiration  \tab  kPa  \tab  Continuous   \cr
dem  \tab  elevation  \tab  meters  \tab  Continuous   \cr
hillshade  \tab  Hill shading (as surrogate for slope and aspect)  \tab  index of brightness  \tab  Continuous   \cr
mas  \tab  Mean annual solar radiation  \tab  Mj/m2/day  \tab  Continuous   \cr
mat  \tab  Mean annual temperature  \tab  degrees C   \tab  Continuous   \cr
r2pet  \tab  Average monthly ratio of rainfall and potential evapotranspiration (ratio)  \tab  none  \tab  Continuous   \cr
rain  \tab  annual precipitation  \tab  mm  \tab  Continuous   \cr
slope  \tab  slope  \tab  degrees    \tab  Continuous   \cr
toxicats  \tab  Toxic Cations  \tab  Toxic Cations; 0=low, 1=intermediate, 2=high (e.g. serpentine soils) \tab  Categorical  \cr
tseas  \tab  Temperature seasonality  \tab  deg C  \tab  Continuous  \cr
vpd  \tab  Mean October vapor pressure deficit at 9 AM  \tab  kPa  \tab  Continuous  \cr
}

x y data are in "New Zealand map grid" projection. Units are meters.

}

\usage{
data(NZtrain)
data(NZbackground)
data(NZtest)
}

\author{Jake McC. Overton}

\source{
train data from the Allan Herbarium, operated by Landcare Research, NZ \url{http://www.landcareresearch.co.nz/research/biodiversity/plantsprog/herbarium.asp?herbarium} 
}

\keyword{datasets}

