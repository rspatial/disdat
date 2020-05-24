\name{disData}

\alias{getDisData}
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
\code{disPo} returns the presence-only (po) data for a region region

\code{disBg} returns the background (bg) data for a region region

\code{disPa} returns the presence-absence (pa) data for a region region and group

\code{disEnv} returns the environmental (env) data for a region region and group

\code{disData} returns a list will all data for a region.

\code{getDisData} returns a single dataset for a region region, type and group.

\code{disBorder} returns a polygon for one of the regions.
}


\usage{
disData(region)

disPo(region)

disBg(region)

disPa(region, group)

disEnv(region, group)

getDisData(region, dataset, type, group)

disBorder(region, pkg="sf")
}
 
\arguments{
  \item{region}{character. One of "AWT", "CAN", "NSW", "NZ", "SA", "SWI" }
 
  \item{dataset}{character. One of "train", or "test" }

  \item{type}{character. One of "po" or "bg" (if \code{dataset} is \code{"train"}); "pa" or "env" (if \code{dataset} is \code{"test"})}
  
  \item{group}{character. If \code{region} is \code{"NSW"}, one of "ba", "db", "nb", "ot", "ou", "rt", "ru", "sr". \code{region} is \code{"AWT"} "bird", "plant". The other regions have no groups.}
 
  \item{pkg}{character. Eihter "sf", "sp", or "terra" to get polygons as defined by that package}
}


\value{
data.frame (getDisData) or list with four data.frames (disData)
}


\examples{
awt_po <- disPo("AWT")

awt_bg <- disBg("AWT")

awt_pa_plants <- disPa("AWT", "plants")

awt_env_plants <- disEnv("AWT", "plants")


x <- disData("NSW")

names(x)

sapply(x, head)

}

\keyword{data}

