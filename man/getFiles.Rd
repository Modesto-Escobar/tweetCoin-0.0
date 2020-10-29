\name{getFiles}
\alias{getFiles}
\title{
Download files
}
\description{
Downloads a list of files and return the errors (if any).
}
\usage{
getFiles(lista, path = "./", ext = NULL)
}
\arguments{
  \item{lista}{A list or data frame of files' URLs to be download (See details).}
  \item{path}{Directory to export the files to.}
  \item{ext}{Select desired extension of the files. Default= NULL.}
}
\details{
This function allows download a file of files directly into your directory. 

This function needs a preexistent data frame of names and pictures' URL. It must be a list (or data.frame) with two values: "name" (specifying the names of the files) and "url" (containing the urls to the files to download). This links can be obtained thru \code{\link[tweetCoin]{getWikidata}} function or other alternative sources. 

All the errors are reported as outcomes (NULL= no errors). The files are donwload into your chosen directory.
}
\value{
It returns a vector of errors, if any. All pictures are download into the selected directory (NULL= no errors).
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Not run: 

## In case you want to download a file directly from an URL:

# dta <- data.frame(name = "Data", url = "https://sociocav.usal.es/me/Stata/example.dta")
# dta$url <- as.character(paste(dta$url))
# getFiles(dta, path = "./", ext = NULL)

##  You can can also combine this function with getWikidata (among others).
## In case you want to download a picture of a person:

# A <- data.frame(name= getWikidata(c("Rembrandt"))$label, url=getWikidata(c("Rembrandt"))$pics)
# A$url <- as.character( paste (A$url))
# getFiles(A, path = "./", ext = "png")

## Or the pics of multiple authors: 

# B <- getWikidata( c("Monet", "Renoir", "Caillebotte"))
# data <- data.frame(name = B$label, url = B$pics)
# data$url <- as.character( paste( data$url))
# getFiles(data, path = "./", ext = NULL)

## End(Not run)
}