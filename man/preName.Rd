\name{preName}
\alias{preName}
\title{
Alter name order
}
\description{
Reverse the order of the first and last names of the items.
}
\usage{
preName(X)
}
\arguments{
  \item{X}{A character vector with "name, prename" contents.}
}
\details{
This function reverses the order of the first and last names of the items: i.e., "Weber, Max" turns into "Max Weber".
}
\value{
A character vector of reordered names.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## To reconvert a single name:
preName("Weber, Max")

## It is possible to work with several items, as in here:
A <- c("Weber, Max", "Descartes, Rene", "Locke, John")
preName(A)
}