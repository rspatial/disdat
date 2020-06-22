\name{NSW}

\alias{NSW}


\docType{data}

\title{New South Wales species distribution data}

\description{
Species occurrence data for 54 species from 8 biological groups in New South Wales (NSW, a state in Australia) and associated environmental data. Full details of the dataset are provided in the reference below. There are four data sets with training (po and bg) and test (pa, env) data:

\code{po} (training data) includes site names, species names, coordinates, occurrence ("1" for all, since all are presence records), group [ba = bats (7 species); db = diurnal birds (8 species); nb = nocturnal birds (2 species); ot = open-forest trees (8 species); ou = open-forest understorey plants (8 species); rt = rainforest trees (7 species); ru = rainforest understorey plants (6 species); sr = small reptiles (8 species)], and site values for 13 environmental variables (below).

\code{bg} (training data) has 10000 sites selected at random across the study region. It is structured identically to \code{po}, with "0" for occurrence (not implying absence, but denoting a background record in a way suited to most modelling methods) and \code{NA} for group.

\code{env} (testing data) includes group, site names, coordinates, and site values for 13 environmental variables (below). These are for sites from different surveys for each biological group (from 570 to 2075 sites per group), and can be returned as separate datasets by \code{disEnv}, or in one long format dataset by \code{disData}. This set of files is suited to making predictions.

\code{pa} (testing data) includes group, site names, coordinates, and presence-absence records, one column per species (in the wide format returned by \code{disPa}). They can also be returned in long format using \code{disData}. The sites are identical to the sites in \code{env}. These data are suited to evaluating the predictions made with \code{env}.

Raster (gridded) data for all environmental variables are available - see the reference below for details.

The reference system of the x and y coordinates is unprojected. Latitude and longitude are in geographical coordinates using the WGS84 datum (EPSG:4326).

The vignette provided with this package provides an example of how to fit and evaluate a model with these data.

\strong{Environmental variables:}
\tabular{llll}{
\strong{Code} \tab \strong{Description} \tab \strong{Units} \tab \strong{Type} \cr

cti      \tab "compound topographic index" - a quantification of the position of a site in the local landscape. It is often referred to as the steady state wetness index and it is defined as: CTI = ln ( As / tanB ) where 'As' is the specific catchment area expressed as m2 per unit width orthogonal to the flow direction and 'B' is the slope angle \tab \tab Continuous\cr

disturb  \tab disturbance (clearing, logging etc) index. \tab 1 = light, 2 = moderate, 3 = heavy \tab Continuous\cr

mi        \tab moisture index. Index of site wetness derived from a water balance algorithm using rainfall, evaporation, radiation and soil depth as inputs \tab Between 0 (dry) and 100 (wet) \tab Continuous \cr

rainann \tab mean annual rainfall  \tab mm \tab Continuous \cr

raindq    \tab mean rainfall of the driest quarter  \tab mm        \tab Continuous  \cr

rugged    \tab ruggedness. Coefficient of variation of grid cells within 1km of cell of interest \tab percent \tab Continuous \cr

soildepth \tab mean soil depth predicted from a model relating sampled soil depths to climate, geology and topography \tab m * 1000 \tab Continuous \cr

soilfert  \tab soil fertility ordinal class, derived from soil maps and modeling of geochemical data \tab  1 (low) to 5 (high) \tab Continuous \cr

solrad    \tab annual mean solar radiation (terrain adjusted)      \tab MJm^-2day^-1 * 10 \tab Continuous \cr

tempann   \tab annual mean temperature              \tab degrees C * 10     \tab Continuous   \cr

tempmin   \tab minimum temperature of the coldest month \tab degrees C * 10 \tab Continuous \cr

topo      \tab topographic position. Mean difference in elevation between grid cell of interest and all cells within 1km radius (-ve values are gullies, +ve are ridges) \tab m \tab Continuous\cr

vegsys   \tab broad vegetation type \tab 1 = Rainforest, 2 = Moist open forest, 3 = Dry open forest, 4 = Woodland, 5 = Coastal sclerophyll complex, 6 = Plateau sclerophyll complex, 7 = Disturbed remnant, 8 = Exotic (pine) plantation, 9 = Cleared \tab Categorical \cr
}
}


\source{
All data were compiled and provided by Simon Ferrier and colleagues.
}

\references{
Elith, J., Graham, C.H., Valavi, R., Abegg, M., Bruce, C., Ford, A., Guisan, A., Hijmans, R.J., Huettmann, F., Lohmann, L.G., Loiselle, B.A., Moritz, C., Overton, J.McC., Peterson, A.T., Phillips, S., Richardson, K., Williams, S., Wiser, S.K., Wohlgemuth, T. & Zimmermann, N.E., In press. Presence-only and presence-absence data for comparing species distribution modeling methods. Biodiversity Informatics.
}


\examples{
nsw_po <- disPo("NSW")
nsw_bg <- disBg("NSW")

nsw_pa_bat <- disPa("NSW", "ba")
nsw_env_bat <- disEnv("NSW", "ba")
nsw_pa_reptile <- disPa("NSW", "sr")
nsw_env_reptile <- disEnv("NSW", "sr")


# Or all in one list
nsw <- disData("NSW")
sapply(nsw, head)

disCRS("NSW")
}

\keyword{datasets}

