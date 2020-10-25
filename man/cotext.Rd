\name{cotext}
\alias{cotext}
\title{Dynamic netCoin graph of elements of texts }
\description{
This function produces an interactive netCoin network from a data frame or matrix with text and dates, connecting their elements differentiated by a separator.
}
\usage{
d_cotext(data, text="text", sep=" ", min=1, ...)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{min}{Minimum number of ocurrences of the text to be include in the graph.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used in texts with differentiated elements.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced.See \code{\link{netCoin}}.
}

\value{
This function creates a netCoin object, which can be ploted with the plot() function, creating a folder in the computer with an HTML file named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## This example uses a sample of original tweets using 
## the hashtag #cuentalo:

data(cuentalo)
cuentalo$elements <- hashtag_extract(cuentalo$text)
cotext(data=cuentalo, text="elements", sep="|")

## You can include all type of arguments from netCoin 
## and/or allNet:

# plot(cotext(data = cuentalo, 
#                text = "elements", sep="|",
#                size = "degree",
#                zoom = 2, repulsion = 32, 
#                distance= 18, dir = "~/temp")
}