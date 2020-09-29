\name{filext}
\alias{filext}
\title{
Find file extension
}
\description{
Explore the type of extension of a file.
}
\usage{
filext(fn)
}
\arguments{
  \item{fn}{Character vector of files links to explore.}
}
\details{
This function explores the extension of a file. 
}
\value{
A character vector of extension names.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## For a single item:
filext( (getWikidata( "Albert Einstein"))$pics)

## You can do the same for a vector:
A <- c("Hillary Duff", " Britney Spears", "Avril Lavigne")
filext( (getWikidata(A))$pics)

## Or obtain the extension of an file's URL:
filext("https://sociocav.usal.es/me/Stata/example.dta")
filext("https://sociocav.usal.es/me/Stata/coin.pkg")
}
