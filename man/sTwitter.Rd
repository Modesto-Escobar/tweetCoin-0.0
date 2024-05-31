\name{sTwitter}
\alias{sTwitter}
\title{Twitter main statistics}
\description{
This function produces some of the basic statistics that can be extracted from a Twitter network, including the number of followers and following, mentions, etc.
}
\usage{
sTwitter(original, retweets = NULL,
         statistics = c("external", "internal",
         "mentions", "centrality", "wcentrality"),
         id = "status_id", author = "author")
}
\arguments{
  \item{original}{A data frame with at least one vector of names.}
  \item{retweets}{Vector name from data containing the messages (tweets) or other relevant text.}
  \item{statistics}{Separator between values/entries, defaults to space (" ").}
  \item{id}{Minimum number of ocurrences of the text to be include in the graph, default = 1.}
  \item{author}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
Applying this function to a dataset will produce, unless contrary designed, a dataset with 7 variables including followers (people following a profile), following (people followd by the profile), statuses (total number of messages written by the profile), issues (messages and retuits collected in the dataset), profilesm (number of profiles mentioned by the profile) and mentioned (number of times the profile has been mentioned).

When applied to a\code{\link[netCoin]{netCoin}} object, 
}

\value{
This function creates a netCoin object, which can be ploted with the plot() function, creating a folder in the computer with an HTML file named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
\dontrun{
## This example uses a sample of original tweets using 
## the hashtag #cuentalo:
data(cuentalo)

## Once you have your data, you can produce the statistics:
A<- sTwitter(cuentalo)

}
}
