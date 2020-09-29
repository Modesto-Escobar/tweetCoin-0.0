\name{nametoWiki}
\alias{nametoWiki}
\title{Wiki iframe of an entry}
\description{
Convert names into a Wikipedia's iframe.}
\usage{
nametoWiki( name, language = "en")}
\arguments{
  \item{name}{A vector consisting of one or more Wikipedia's entry (i.e., topic or person).}
  \item{language}{The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").}
}
\details{
This function adds the Wikipedia's iframe to a entry or name, i.e., "Max Weber" converts into "<iframe src=\"https://es.m.wikipedia.org/wiki/Max_Weber\" width=\"100...". It also manages different the languages of Wikipedia thru the abbreviated two-letter language parameter, i.e., "en" = "english".
}
\value{
A character vector of Wikipedia's iframes.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## When extracting a single item;
nametoWiki("Computer", language = "en")

## When extracting two objetcs;
A <- c("Computer", "Operating system")
nametoWiki(A)

## Same when three or more items;
B <- c("Socrates", "Plato", "Aristotle")
nametoWiki(B)
}