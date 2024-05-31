\name{cotext}
\alias{cotext}
\title{Static netCoin graph of elements of texts}
\description{
This function produces an interactive netCoin network from a data frame or matrix with text and dates, connecting their elements differentiated by a separator.
}
\usage{
cotext(data, text = "text", sep = " ", min = 1, ...)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{text}{Vector name from data containing the messages (tweets) or other relevant text.}
  \item{sep}{Separator between values/entries, defaults to space (" ").}
  \item{min}{Minimum number of ocurrences of the text to be include in the graph, default = 1.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used in texts with differentiated elements. It has been designed to be used with Twitter messages, but this function can classify and connect any other type of text depending on what is selected in the "sep" argument (e.g., Blog posts, Instagram's captations ...).

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
## the hashtag #Shakespeare:

data(Shakespeare)
Shakespeare$elements <- hashtag_extract(Shakespeare$text)
t <- cotext(data = Shakespeare, text = "elements", sep="|", min = 15)

## You can include all type of arguments from netCoin 
## and/or allNet:

library(netCoin)
t2 <- addNetCoin(t, size = "degree",
      zoom = .9, repulsion = 32, 
     distance= 18)

## We extracted the hashtags first using hashtag_extract() function,
## but it can also be directly used like this:

t3 <- cotext(Shakespeare, text = "text", sep = "#", min = 15)
plot(t3)
}
