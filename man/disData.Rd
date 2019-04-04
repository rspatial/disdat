\name{disData}

\alias{disData}
\alias{getDisData}


\title{
Load or get disdat datasets
}

\description{
Simple helper functions. disData loads all three datasets for one or more regions into the global environment. getDisData returns a single dataset (region / type). 
}

\usage{
disData(...)
getDisData(region, type)
}
 
\arguments{
  \item{...}{Character. 'ALL', or one or more of 'AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI' }
  \item{region}{Character. One of 'AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI' }
  \item{type}{Character. One of 'train', 'background', 'test' }
}


\value{
disData does not return anything, it is used for the side effect of loading data into the global environment. 
getDisData returns a data.frame
}


\author{
Robert J. Hijmans
}

\examples{
disData('NSW')
NSWtest[1, 1:10]
d <- getDisData('NSW', 'test')
d[1,1:10]

# standard approach
data(NSWtest)
}

\keyword{data}

