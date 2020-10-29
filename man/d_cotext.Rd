\name{d_cotext}
\alias{d_cotext}
\title{Dynamic netCoin graph of elements of texts through time}
\description{
This function produces a dynamic and interactive netCoin network from a data frame or matrix with text and dates, conecting their elements differentiated by a separator.
}
\usage{
d_cotext(data, text = "text", sep =" ", min = 1, date = "date", 
          beginDate = NULL, endDate = NULL, interval = 3600, 
          tzone = "Europe/Paris", limits = NULL, nFrames = Inf,
          seed = 1, n = min(5000,nrow(data)), minlabel = 5, ...)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{min}{Minimum number of ocurrences of the text to be include in the graph, default = 1.}
  \item{date}{Vector from data containing the date when the text was written.}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The end-points of the interval (default = 3600).}
  \item{tzone}{Time zone (i.e. "Europe/Paris").}
  \item{limits}{Date vector with the cutpoints for dynamic graphs.}  
  \item{nFrames}{The number of frames to render (default = Inf).}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than #minlabel (default = 5)..}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with texts through time.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced.See \code{\link{netCoin}}.
}

\value{
This function creates a multigraph object (several graphs through time), which can be ploted with the plot() function, creating a folder in the computer with an HTML file named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Example using a sample of tweets and retweets with 
## the hashtag #Shakespeare:

data(Shakespeare)
search_tweets_Shakespeare$elements <- hashtag_extract(search_tweets_Shakespeare$text)
n <- d_cotext(data=Shakespeare, text="elements", sep="|", 
              date= "date", support= 10, interval = 3600 * 24)
              
## You can include all type of arguments from netCoin 
## and/or allNet:

n2 <- d_cotext(data = Shakespeare, 
               text = "elements", sep="|", date = "date",
               size = "degree", zoom = 2, repulsion = 32, 
               distance= 18)
}