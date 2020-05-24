\name{disData}

\alias{getDisData}
\alias{disData}
\alias{disBorder}

\title{
Get disdat datasets
}

\description{
Simple helper functions. \code{getDisData} returns a single dataset (region / dataset / type / group).

\code{disBorder} returns a polygon for one of the regions.
}

\usage{
disData(region)
getDisData(region, dataset, type, group)
disBorder(region, pkg="sf")
}
 
\arguments{
  \item{region}{Character. One of "AWT", "CAN", "NSW", "NZ", "SA", "SWI" }
  \item{dataset}{Character. One of "train", or "test" }

  \item{type}{Character. One of "po" or "bg" (for train dataset); "pa" or "env" (for test dataset)}
  
  \item{group}{Character. One of "ba", "db", "nb", "ot", "ou", "rt", "ru", "sr" for test dataset of NSW region or "bird", "plant" for test dataset of AWT region. The other regions have no groups.}
 
  \item{pkg}{Character. Eihter "sf", "sp", or "terra" to get polygons as defined by that package}
}


\value{
data.frame (getDisData) or list with four data.frames (disData)
}


\examples{
d <- getDisData("NSW", "test", "pa", "sr")
head(d)

b <- disBorder("NSW")

x <- disData("NSW")
names(x)
sapply(x, head)
}

\keyword{data}

