\name{NSW}

\alias{NSWtrain}
\alias{NSWbackground}
\alias{NSWtest}


\docType{data}

\title{New South Wales species distribution data}

\description{
Species occurence data for 54 species from 8 biological groups in New South Wales (NSW, a state in Australia) and associated environmental data. There are three data sets with training (NSWtrain), background (NSWbackground), and test (NSWtest) data. The training data principally consist of environmental variables at locations where the species where observed (presence only), and at random locations for background data. The test data are presence/absence observations on hte same species, independent from the training data.


spid is a four letter code. The first two letters indicate group: ba = bats (7 species); db = diurnal birds (8 species); nb = nocturnal birds (2 species); ot = open-forest trees (8 species); ou = open-forest understorey plants (8 species); rt = rainforest trees (7 species); ru = rainforest understorey plants (6 species); sr = small reptiles (8 species) \cr


Environmental variables:
\tabular{llll}{  
Code      \tab Description                          \tab Units     \tab Type \cr
rainann   \tab mean annual rainfall                 \tab mm        \tab Continous \cr
raindq    \tab mean rainfall of the driest quarter  \tab mm        \tab Continous  \cr
tempann   \tab annual mean temperature              \tab deg C     \tab Continous   \cr
tempmin   \tab minimum temperature of the coldest month \tab deg C \tab Continous \cr
solrad    \tab annual mean solar radiation (terrain adjusted)      \tab MJm-2day-1 \tab Continous \cr
mi        \tab moisture index. Index of site wetness derived from a water balance algorithm \tab Between 0 (dry) and 1 (wet) \tab Continous \cr
soilfert  \tab soil fertility class, derived from soil maps and modeling of geochemical data \tab  1 (low) to 5 (high) \tab Continous \cr
soildepth \tab mean soil depth predicted from a model relating sampled soil depths to climate, geology and topography \tab m \tab Continous \cr
rugged    \tab ruggedness. Coefficient of variation of grid cells within 1km of cell of interest \tab percent \tab Continous \cr
topo      \tab topographic position. Mean difference in elevation between grid cell of interest and all cells within 1km radius (-ve values are gullies, +ve are ridges) \tab m \tab Continous\cr
cti      \tab "compound topographic index" a quantification of the position of a site in the local landscape. It is often referred to as the steady state wetness index and it is defined as: CTI = ln ( As / tanB ) where 'As' is the specific catchment area expressed as m2 per unit width orthogonal to the flow direction and 'B' is the slope angle \tab \tab Continous\cr
disturb  \tab disturbance (clearing, logging etc) index. \tab 1 = light, 2 = moderate, 3 = heavy \tab Continous\cr
vegsys   \tab broad vegetation type \tab 1 = Rainforest, 2 = Moist open forest, 3 = Dry open forest, 4 = Woodland, 5 = Coastal sclerophyll complex, 6 = Plateau sclerophyll complex, 7 = Disturbed remnant, 8 = Exotic (pine) plantation, 9 = Cleared \tab Categorical \cr
}
}

\usage{
data(NSWtrain)
data(NSWbackground)
data(NSWtest)
}

\author{Simon Ferrier}

\source{
Data was compiled and provided by Simon Ferrier and colleagues and distributed with his permission. 

Training data: Fauna data came from the Atlas of NSW Wildlife (a database of incidental sighting records), and the flora data consisted of specimen records from both the University of New England Herbarium and the Sydney Herbarium (Royal Botanic Gardens). All available records used. No information on collection dates or accuracy. Reduced to one record per species per 100m grid cell. These data were used by Ferrier, S. and G. Watson,  1997. An evaluation of the effectiveness of environmental surrogates and modeling techniques in predicting the distribution of biological diversity. Environment Australia, Canberra (\url{http://www.ea.gov.au/biodiversity/publications/technical/surrogates/}). That report provides further detail on the origin and nature of the data.

Testing data is from designed surveys. These are described in Ferrier and Watson (1996; see above), and Pearce J., Ferrier S. & Scotts D. (2001) An evaluation of the predictive performance of distributional models for flora and fauna in north-east New South Wales., J. Environ. Manage., 62, 171-184.

}

\keyword{datasets}

