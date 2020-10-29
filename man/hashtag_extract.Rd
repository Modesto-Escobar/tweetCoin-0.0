\name{hashtag_extract}
\alias{hashtag_extract}
\title{
Hashtags and mentions extractor
}
\description{
Extracts the hashtags and mentions from the rest of the message.
}
\usage{
hashtag_extract( texto, characters = "#@", 
              excludeRT = TRUE, oldText = NULL, 
              newText = NULL)
}
\arguments{
  \item{texto}{Base text (messages).}
  \item{characters}{Desired prefixes, normally "=#@".}
  \item{excludeRT}{Do not count retweets as mentions.}
  \item{oldText}{Text from the original message to be replaced.}
  \item{newText}{New text to replace the original message.}
}
\details{
This function allows to select and separate all the hashtags (#) and/or mentions (@) from the rest of the information of a message.

It is possible to select another kind of prefix changing the argument "characters". The argument "excludeRT = TRUE" enable to drop the retweets ( market as RT @x:) as mentions. The argument "oldText" and "newText" allows to change parts of the message to what's desired (see example).
}
\value{
A character vector of hashtags and/or mentions. In case of error, it's returned empty.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Single message example:
hashtag_extract(texto = "Good afternoon #everybody @mundo", characters = "#@")

##Vector extraction:
A <- c( "@WoutvanAert strikes again!", 
        "@DisneysMulan Disney's #Mulan",
        "I really enjoyed #Merli")
hashtag_extract(A, characters= "#@")

## Example excluding retweets and replacing the text:

B <- "RT @DisneysMulan: Just finished #Mulan #movie!"
hashtag_extract(B, characters = "#@", excludeRT = TRUE, oldText = "#|@", newText = "")

## It is possible to use this argument to change the separator of the outcomes:
hashtag_extract(B, characters = "#@", oldText = "\\|", newText = ";")
}