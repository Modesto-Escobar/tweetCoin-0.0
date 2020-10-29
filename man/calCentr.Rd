\name{calCentr}
\alias{calCentr}
\title{Categorize a network}
\description{
This function explores and catalogues a network.
}
\usage{
calCentr(graph, measures=c("degree","closeness","betweenness","eigen"), order = "")
}
\arguments{
  \item{graph}{A network graph or \code{\link[netCoin]{netCoin}} object.}
  \item{measures}{Character vector of the measures to be calculated (See details).}
  \item{order}{Sort the data.frame by "degree", "closeness", "betweenness" or "eigen".}
}
\details{
This function reproduces some of the most significant classic Social Network Theory's centrality measures. See Wasserman (1994),  Freeman (1978), or Bonacich & Lloyd (2001) to know more:

a) Degree =  Degree centrality is measured by the total amount of direct links with the other nodes.

b) Closeness = Closeness centrality is meant to measure one node to the others nodes' sum distances

c) Betweenness = Betweenness centrality measures one node undertaking "mediation" role in a network. 

d) Eigen = Eigenvector centrality measures a node's importance while giving consideration to the importance of its neighbors.

By default, measures = "all", thus all the measures will be calculated.
}
\value{
This function creates a list containing two elements: 1) a data.frame (nodes) with all the centrality measures applied to the twitter users and 2) another data.frame (graph) with this measures applied to the whole network. 
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## This example uses a sample tweets using 
## the hashtag "#Shakespeare":

data(Shakespeare)
R <- cotweet(Shakespeare, "author", "text")
S <- calCentr(R, "all")
S$nodes
S$graph

## You can select only the measurement you want:
calCentr(R, measures = "closeness")   
}