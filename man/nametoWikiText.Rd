\name{nametoWikiText}
\alias{nametoWikiText}
\title{Wiki html link of an entry}
\description{
This function converts names into a Wikipedia's html link.
}
\usage{
nametoWikiText(name, language = "en")
}
\arguments{
  \item{name}{A vector consisting of one or more Wikipedia's entry (i.e., topic or person).}
  \item{language}{The language of the Wikipedia page version. This should consist of an ISO language code (default = "en").}
}
\details{
This function adds the Wikipedia's html link to a entry or name, i.e., "Max Weber" converts into "<a href='https://es.wikipedia.org/wiki/Max_Weber', target='_blank'>Max Weber</a>". It also manages different the languages of Wikipedia thru the abbreviated two-letter language parameter, i.e., "en" = "english".
}
\value{
A character vector of names' html links.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{

## When extracting a single item;
nametoWikiText("Computer", language = "en")

## When extracting two objetcs;
A <- c("Computer", "Operating system")
nametoWikiText(A)

## Same when three or more items;
B <- c("Socrates", "Plato","Aristotle" )
nametoWikiText(B)
}