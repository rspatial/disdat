\name{disMapBook}
\alias{disMapBook}

\title{
Generating maps of disdat species
}

\description{
A helper function for automatically generating maps for the species data in PDF format.
}

\usage{
disMapBook(region, output_pdf, verbose = TRUE)
}


\arguments{
  \item{region}{
 A character vector. The name of the region(s) to generate plots.
}
  \item{output_pdf}{
 Output pdf file to be saved.
}
  \item{verbose}{
 Logical. control amount of screen reporting.
}

}


\seealso{
\code{\link{disPo}}, \code{\link{disPa}} and \code{\link{disBorder}}
}


\examples{\donttest{
disMapBook(c("AWT", "NSW"), "~/Desktop/sp_mapbook.pdf")
}
}

\keyword{Map, spatial}

