\name{SA}

\alias{SAtrain}
\alias{SAbackground}
\alias{SAtest}

\docType{data}

\title{South American plant species distribution data}

\description{
Species occurence data for 30 plant species (all from the Bignoniaceae family) from South America (SA) and associated environmental data. There are three data sets with training (SAtrain), background (SAbackground), and test (SAtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on the same species, independent from the training data.



Environmental variables were extracted from WorldClim:
\tabular{llll}{  
Code \tab Description \tab Units \tab Type\cr
sabio1  \tab  Annual mean temperature    \tab  degrees C  \tab  Continuous  \cr
sabio2  \tab  Mean Diurnal Range (Mean of monthly (max temp - min temp))  \tab  degrees C  \tab  Continuous  \cr
sabio4  \tab  Temperature Seasonality (standard deviation *100)  \tab  degrees C  \tab  Continuous  \cr
sabio5  \tab  Max Temperature of Warmest Month  \tab  degrees C  \tab  Continuous  \cr
sabio6  \tab  Min Temperature of Coldest Month  \tab  degrees C  \tab  Continuous  \cr
sabio7  \tab  Temperature Annual Range \tab  degrees C  \tab  Continuous  \cr
sabio8  \tab  Mean Temperature of Wettest Quarter   \tab  mm  \tab  Continuous  \cr
sabio12  \tab  Annual Precipitation  \tab  mm  \tab  Continuous  \cr
sabio15  \tab  Precipitation Seasonality (Coefficient of Variation)  \tab  mm  \tab  Continuous  \cr
sabio17  \tab  Precipitation of Driest Quarter  \tab  mm  \tab  Continuous  \cr
sabio18  \tab  Precipitation of Warmest Quarter  \tab  mm  \tab  Continuous  \cr
}

Latitude and latitude are in geographical coordinates using the WGS84 datum.

}

\usage{
data(SAtrain)
data(SAbackground)
data(SAtest)
}

\author{Bette Loiselle and Lucia Lohman}

\source{
Test data comes from the Missouri Botanical Garden database management system TROPICOS (http://www.mobot.org) and from Lucia Lohmann. Species localities were georeferenced from locality description. Coordiantes are have up to 4 decimal places, but that does not reflect that degree of accuracy.  Test data is from plot data collected by Al Gentry over a period of 22 years (1971-1993)
}

\keyword{datasets}

