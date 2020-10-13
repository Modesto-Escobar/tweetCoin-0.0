\name{cc}
\alias{cc}
\title{List to character converter}
\description{
This function converts a list into a vector of characters.
}
\usage{
cc(wordlist)
}
\arguments{
  \item{wordlist}{A list of items to convert.}
}
\details{
This function allows to convert any list into a vector of characters.
}
\value{
A character vector.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## For a large list:
cc("Picasso, Velazquez, Dali")
}