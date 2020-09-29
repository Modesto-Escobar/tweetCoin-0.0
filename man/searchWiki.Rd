\name{searchWiki}
\alias{searchWiki}
\title{Check existence of a Wikipedia entry}
\description{
Finds if there is a Wikipedia page of a name(s) in the selected language. 
}
\usage{
searchWiki(name, language = c("en", "es", "fr", "it", "de", "pt", "ca"), 
           all=FALSE, maxtime=0)
}
\arguments{
  \item{name}{A vector consisting of one or more Wikipedia's entry (i.e., topic or person).}
  \item{language}{The language of the Wikipedia page version. This should consist of an ISO language code.}
  \item{all}{If all, all the languages are checked. If false, once a term is found, there is no search of others, so it's faster.}
  \item{maxtime}{In case you want to apply a random waiting between consecutive searches.}
}
\details{
This function checks any page or entry in order to find if it has a Wikipedia page in a given language. 

It manages the different the languages of Wikipedia thru the two-letters abbreviated language parameter, i.e, "en" = "english". It is possible to check multiple languages in order of preference; in this case, only the first available language will appear as TRUE.
}
\value{
A Boolean data frame of TRUE or FALSE.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{

## When you want to check an entry in a single language:
searchWiki("Manuel Vilas", language = "es")

## When you want to check an entry in several languages:
searchWiki("Manuel Vilas", language = c( "en", "es", "fr", "it", "de", "pt", "ca"))

## When you want to check several entries and languages:
A<-c("Manuel Vilas", "Julia Navarro", "Rosa Montero")
searchWiki(A, language = c("en", "es", "fr", "it", "de", "pt", "ca"))
}