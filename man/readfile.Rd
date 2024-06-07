\name{readfile}
\alias{readfile}
\title{Read text file}
\description{
Reads a plain text file.
}
\usage{
readfile(file, encoding = "UTF-8")
}
\arguments{
  \item{file}{Path to file to be exported.}
  \item{encoding}{A character string describing the encoding format.}
}
\details{
This function allows to read a plain text file.
}
\value{
Returns a character string.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{

testfile <- system.file("templates/Text.txt", package="tweetCoin")
readfile(file = testfile)

}
