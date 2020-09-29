\name{credentials}
\alias{credentials}
\title{Twitter credentials}
\description{
Identify and save your Twitter credentials
}
\usage{
credentials(file = "credentials.txt")
}
\arguments{
  \item{file}{Path to the file containing your credentials (See details).}
}
\details{
This function adds the Twitter authentication protocol credentials required to work through the Twitter API on a token. 

You must create a .txt file containing five things: "appname" (name of the app), "consumer_key" (API key), "consumer_secret" (API secret key), "access_key" (access token), and "access_secret" (access token secret). 

This requires that you to already know the access tokens for your Twitter app. In case you don't know your tokens, they can be obtained following the instructions explained here: \href{https://developer.twitter.com/en/docs/getting-started}{instructions}. 

It is recommended to save the new enviroment created with this function as "token" in order to facilitate the compatibility with the other package functions.
}
\value{
Newly created environment containing the tokens.
}
\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}
}
\examples{
## Once you have created a .txt containing the info 
## explained in details, name it "credentials.txt"
## and import it like this:

## credentials()
}