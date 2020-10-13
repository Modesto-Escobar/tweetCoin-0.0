\name{linkTwitter}
\alias{linkTwitter}
\title{Twitter user/hashtag to HTML}
\description{
This function creates a HTML link from a Twitter user or hashtag to.
}
\usage{
linkTwitter(entity)
}
\arguments{
  \item{entity}{A vector consisting of one or more Twitter's search (i.e., user or hashtag).}
}
\details{
This function adds the Twitter's html link to a entry or name. You can select a profile or user, thus preceded by "@", or a query or hashtag, thus preceded by "#".
}
\value{
A character vector of names' html links.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## When cheking a profile or user:
linkTwitter( "@cristiano")

## When cheking a query or hashtag:
linkTwitter( "#AppleEvent")

## It is possible to work with several queries, as in here:
A <- c("@cristiano", 
       "@realmadrid",
       "@3gerardpique",
       "@FCBarcelona")
linkTwitter(A)
}