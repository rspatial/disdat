\name{SWI}

\alias{SWItrain}
\alias{SWIbackground}
\alias{SWItest}

\docType{data}

\title{Switzerland species distribution data}

\description{
Species occurence data for 30 tree species in Switzerland (SWI, a country in Europe) and associated environmental data. There are three data sets with training (SWItrain), background (SWIbackground), and test (SWItest). The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations of the same species, independent from the training data.


Environmental variables:
\tabular{llll}{  
ddeg  \tab  Growing degree-days above a threshold of 0 degrees C  \tab  degrees C * days  \tab  Continuous   \cr
tavecc  \tab  Average temperature of the Coldest Month   \tab  degrees C  \tab  Continuous   \cr
sfro  \tab  Summer Frost Frequency \tab  days  \tab  Continuous   \cr
precyy  \tab  Average Yearly Precipitation Sum   \tab  mm  \tab  Continuous   \cr
pdsum  \tab  Number of days with rainfall higher than 1 mm   \tab  ndays  \tab  Continuous   \cr
swb  \tab  Site Water Balance  \tab  mm  \tab  Continuous   \cr
sradyy  \tab  Potential Yearly Global Radiation (daily average)   \tab  (kJ/m2)/day  \tab  Continuous   \cr
slope \tab  Slope  \tab  degrees x 10  \tab  Continuous   \cr
topo  \tab  Topographic Position  \tab  range   \tab  Continuous   \cr
nutri  \tab  Soil Nutrients index between 0-45  \tab  D mval/cm2  \tab  Continuous   \cr
calc  \tab  Bedrock strictly calcareous vs. other type   \tab  p/a  \tab  Categorical  \cr
bcc  \tab  Broadleaved Continuous Cover  (based on Landsat images)  \tab  percentage  \tab  Continuous \cr
ccc  \tab  Coniferous Continuous Cover  (based on Landsat images) \tab  percentage   \tab  Continuous  \cr 
}

}

\usage{
data(SWItrain)
data(SWIbackground)
data(SWItest)
}

\author{Antoine Guisan and Nicklaus Zimmerman}

\source{
Training data set is a forest inventory data set. A regular point lattice across Switzerland located on a 1km national grid. The lattice points that do contain forest according to the national definition within the Swiss forest inventory are sampled in the field. The data originate from the 1985 national inventory, actually collected from 1982-1986.

Test data set is from forest plots. A forest vegetation data base containing irregularly and nonsystematically sampled forest vegetation relevees throughout Switzerland. Dates: records start in 1913. The majority of the plots was collected after 1920, and about 3/4th of the data were sampled after 1940. The data base ends in 1998, but most records are from 1994 and earlier. The individual authors had their own local sampling design or used preferential sampling techniques. From the original data base, selected roughly 14,100 relevees that included minimal data standards such as coordinates. 
}

\keyword{datasets}

