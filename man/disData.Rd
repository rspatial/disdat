\name{disData}

\alias{disData}
\alias{disPo}
\alias{disBg}
\alias{disPa}
\alias{disEnv}

\alias{disBorder}

\title{
Get disdat datasets
}

\description{
\code{disPo} returns the presence-only (po) data for a region

\code{disBg} returns the background (bg) data for a region

\code{disPa} returns the presence-absence (pa) data for a region and group

\code{disEnv} returns the environmental (env) data for sites matching those in the pa data, for a region and group

\code{disData} returns a list with all data for a region.

\code{disBorder} returns a polygon for one of the regions.
}


\usage{
disData(region)

disPo(region)

disBg(region)

disPa(region, group)

disEnv(region, group)

disBorder(region, pkg="sf")
}

\arguments{
  \item{region}{character. One of "AWT", "CAN", "NSW", "NZ", "SA", "SWI" }

  \item{group}{character. If \code{region} is \code{"NSW"}, one of "ba", "db", "nb", "ot", "ou", "rt", "ru", "sr". \code{region} is \code{"AWT"} "bird", "plant". The other regions each have only one group, so group should not be specified}

  \item{pkg}{character. Either "sf", "sp", or "terra" to get polygons as defined by that package}
}


\details{
\code{disData} returns a list with \code{env}, \code{pa}, \code{bg} and \code{po} data in that order. For regions with more than one group, the testing data (env and pa) will come from different surveys, and the model testing should be targeted to the relevant group. The first column of the \code{env} and \code{pa} data.frames is "group", which can be used to extract the correct data.
}


\value{
data.frame (disPo, disBg, disPa and disEnv) or list with four data.frames (disData)
}


\examples{
awt_po <- disPo("AWT")

awt_bg <- disBg("AWT")

awt_pa_plants <- disPa("AWT", "plant")

awt_env_plants <- disEnv("AWT", "plant")


x <- disData("NSW")

names(x)

sapply(x, head)


z <- disBorder("NSW")

plot(z)

}

\keyword{data}

