\name{disData}

\alias{disData}
\alias{getDisData}
\alias{getBorder}


\title{
Load or get disdat datasets
}

\description{
Simple helper functions. \code{disData} loads all datasets for one or more regions into the global environment. \code{getDisData} returns a single dataset (region / dataset / type / group). \code{getBorder} returns the spatial border of each region as a sf object.
}

\usage{
disData(...)
getDisData(region, dataset, type, group)
getBorder(region)
}
 
\arguments{
  \item{...}{Character. 'ALL', or one or more of 'AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI' }
  \item{region}{Character. One of 'AWT', 'CAN', 'NSW', 'NZ', 'SA', 'SWI' }
  \item{dataset}{Character. One of 'train', or 'test' }
  
  \item{type}{Character. One of 'po' or 'bg' (for train dataset); 'pa' or 'env' (for test dataset)}
  
  \item{group}{Character. One of 'ba', 'db', 'nb', 'ot', 'ou', 'rt', 'ru', 'sr' for test dataset of NSW region or 'bird', 'plant' for test dataset of AWT region. The rest of the regions have no groups.}
}


\value{
disData does not return anything, it is used for the side effect of loading data into the global environment. 
getDisData returns a data.frame
}


\author{
Robert Hijmans, Roozbeh Valavi, Jane Elith
}

\examples{
disData('NSW')
NSWtest_pa_db[1, 1:10]

d <- getDisData('NSW', 'test', 'pa', 'sr')
d[1,1:10]

b <- getBorder('NSW')

}

\keyword{data}

