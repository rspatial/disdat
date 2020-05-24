\name{spMapBook}
\alias{spMapBook}

\title{
Generating maps of disdat species 
}

\description{
A helper function for automatically generating maps for the species data in PDF format.
}

\usage{
spMapBook(region, output_pdf, map_theme = "classic", verbose = TRUE)
}


\arguments{
  \item{region}{
 A character vector. The name of the region(s) to generate plots.
}
  \item{output_pdf}{
 Output pdf file to be saved.
}
  \item{map_theme}{
 Character. A \code{tmap} package theme for the maps. The default value is "classic". See also the other themes in \code{\link[tmap]{tmap_style}}.
}
  \item{verbose}{
 Logical. control amount of screen reporting.
}

}

\author{
Roozbeh Valavi
}


\seealso{
\code{\link{disData}} and \code{\link{disBorder}}
}


\examples{\donttest{
spMapBook(c('AWT', 'NSW'), "~/Desktop/sp_mapbook.pdf")
}
}

\keyword{Map, spatial}

