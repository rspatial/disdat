\name{disCRS}

\alias{disCRS}
 
\title{
Coordinate reference system
}

\description{
Get the coordinate reference system for the data of a region.  
}

\usage{
disCRS(region, format="proj4")
}
 
\arguments{
  \item{region}{character. One of "AWT", "CAN", "NSW", "NZ", "SA", "SWI" }

  \item{format}{character. Either "proj4" or "EPSG"}
}


\value{
character vector
}

\examples{
disCRS("AWT")
disCRS("NSW")
}

\keyword{data}
\keyword{spatial}

