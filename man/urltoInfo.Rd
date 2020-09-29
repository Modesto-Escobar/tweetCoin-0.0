\name{urltoInfo}
\alias{urltoInfo}
\title{URL to iframe Wiki converter}
\description{
Converts a Wikipedia URL to an HTML iframe.
}
\usage{
urltoInfo(url)
}
\arguments{
  \item{url}{Character vector of URLs.}
}
\details{
This function converts an available Wikipedia URL direction to the corresponding HTML iframe, i.e., \url{"https://es.wikipedia.org/wiki/Socrates"} changes into "<iframe src=\"https://es.wikipedia.org/wiki/Socrates\" width=\"100(...)"."
}
\value{
A character vector of names' iframe links.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## To convert a single URL direction:
urltoInfo( "https://es.wikipedia.org/wiki/Socrates" )

## It is possible to work with several items, as in here:
A <- c("https://es.wikipedia.org/wiki/Socrates", 
       "https://es.wikipedia.org/wiki/Plato",
       "https://es.wikipedia.org/wiki/Aristotle")
urltoInfo(A)
}