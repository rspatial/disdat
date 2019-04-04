\name{CAN}

\alias{CANtrain}
\alias{CANbackground}
\alias{CANtest}

\docType{data}

\title{South American plant species distribution data}

\description{
Species occurence data for 20 bird species from Ontario, a province in Canada (CAN) and associated environmental data. There are three data sets with training (CANtrain), background (CANbackground), and test (CANtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the CANme species, independent from the training data.



Environmental variables:
\tabular{llll}{  
Code \tab Description \tab Units \tab Type\cr
Onttemp  \tab  Annual mean temperature   \tab  degrees C  \tab  Continuous \cr
Onttemp4  \tab  April temperature  \tab  degrees C  \tab  Continuous \cr
Ontveg  \tab  Vegetation   \tab  none  \tab  Categorical \cr
Ontprec  \tab  Annual Precipitation  \tab  mm  \tab  Continuous \cr
Ontprecsd  \tab  Precipitation Seasonality (Coefficient of Variation)  \tab  mm  \tab  Continuous \cr
Ontprec4  \tab  Precipitation of Driest Quarter  \tab  mm  \tab  Continuous \cr
Onttemsd  \tab  Temperature standard deviation   \tab  degrees C  \tab  Continuous \cr
alt  \tab  Digital elevation   \tab  m  \tab  Continuous \cr
asp2  \tab  Aspect  \tab  ranges from 0 to 1  \tab  Continuous \cr
ontslp  \tab  Slope   \tab  degrees  \tab  Continuous \cr
watdist  \tab  Distance from Hudson Bay  \tab  m  \tab  Continuous \cr
}
}

\usage{
data(CANtrain)
data(CANbackground)
data(CANtest)
}

\author{M. Peck, M. Cadman, Falk Huettmann}

\source{
Training data are 'Ontario Nest Records' from the Royal Ontario Museum (ROM) obtain from M. Peck for NCEAS project. Temporal Span 1870-2002 (most from 1960-2001). Methods of Georeferencing: Coordinates derived from map by ROM; some locations ground-truthed with GPS. Taxonomic Authorities: individual nest recorders.

Test data: the Breeding Bird Atlas (BBA) Data for Ontario, provided by M. Cadman.

}

\keyword{datasets}

