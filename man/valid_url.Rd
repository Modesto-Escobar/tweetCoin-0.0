\name{valid_url}
\alias{valid_url}
\title{Check existence of a Wikipedia URL}
\description{
Finds if a Wiki URL link is valid.
}
\usage{
valid_url( url_in, t = 2 )
}
\arguments{
  \item{url_in}{A vector links to Wikipedia's entries (i.e., to a topic or person).}
  \item{t}{Numeric. The timeout (in seconds) to be used for this connection. Default = 2.}
}
\details{
This function checks a URL link in order to find if it there is a valid Wikipedia page associated.
}
\value{
A Boolean data.frame of TRUE or FALSE.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Check if the URL is valid:
valid_url(url_in ="https://es.wikipedia.org/wiki/Weber,_Max", t=2)
}