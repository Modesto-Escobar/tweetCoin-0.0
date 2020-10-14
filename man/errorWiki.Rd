\name{errorWiki}
\alias{errorWiki}
\title{
Download and check errors in Wikipedia
}
\description{
This function downloads the Wiki pages and discovers if there are any naming errors in Wikipedia.
}
\usage{
errorWiki(X, language = c( "es", "en", "fr"), directory="./", maxtime = 0)
}
\arguments{
  \item{X}{A vector consisting of one or more Wikipedia's entry (i.e., topic or person).}
  \item{language}{The language of the Wikipedia page version. This should consist of an ISO language code.}
  \item{directory}{Directory}
  \item{maxtime}{Max time interval between one check and the following.}
}
\details{

This function checks the Wiki URLs and downloads all the content into a folder called "paginas". This folder needs to be created in advance. All errors are reported as outcomes (NULL= no error). It also manages different the languages of Wikipedia thru the abbreviated two-letter language parameter, i.e., "en" = "english".
}
\value{
It returns a integer vector of errors, if any. All Wikipedia pages are download in the folder /paginas.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{

## You can download a single entry:
errorWiki(X = "Pencil", language = "en")

## Or a set of two or more names:
A<- c("Camaron de la Isla", "Lola Flores", "Estrella Morente")
errorWiki(X = A, language = "es")
}