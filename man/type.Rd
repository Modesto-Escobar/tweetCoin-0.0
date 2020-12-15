\name{type}
\alias{type}
\title{Categorize Twitter users}
\description{
This function explores and catalogues the Twitter users.
}
\usage{
type(Profiles, followers = "followers", 
      following = "following", retweets = "Retweets", 
      tweets = "Tweets", retweeted = "Retweeted", X="X", Y="Y")
}
\arguments{
  \item{Profiles}{A data.frame containing the profiles (users' names) and needed variables.}
  \item{followers}{Number of Twitter accounts that follow this profile.}
  \item{following}{Number of Twitter accounts followed by the profile.}
  \item{retweets}{Number of retweets made by a profile.}
  \item{tweets}{Number of tweets wrote by the profile (Stauses).}
  \item{retweeted}{Number of retweets recieved by the profile.}
  \item{X}{Name for the horizontal coordinate (followers/following users).}
  \item{Y}{Name for the vertical coordinate (retweeted/sent tweets).}
}
\details{
This user classification follows the method proposed by \href{https://journals.sagepub.com/doi/10.1177/0002764213479371}{Gonzalez-Bailon & Borge-Hothoefer (2013)}. To do so, the function creates three new variables; "sent", "x" & "y", where:

1) sent = tweets + retweets

2) X = log10(followers / following)

3) Y = log10(retweeted / sent)

This function clasifies the profiles according to the number of follows and followers, retweets made and recieved and number of tweets written in five broad categories:

a) Broadcaster. Receive fewer retweets than expected based on their number of followers: followers / following >= 0 & retweeted / sent  <= 0

b) Common user. Receive as few retweets as one would expect, given their low number of followers: followers / following < 0 & retweeted / sent  <= 0

c) Influential users. Receive as many retweets as you would expect given their high number of followers: followers / following >= 0  & retweeted / sent  > 0

d) Hidden influentials. Received more retweets than they retweeted others' tweets while they have fewer followers than followees: followers / following < 0  & retweeted / sent  > 0
  
e) Not class. All those that don't fit into any of the previous typologies.

Please, make sure all your variables are saved as numeric before performing the function.
}
\value{
This function creates a "tibble" or "tbl_df" data frame with four new variables, including the outcome "type" where the type of users is specified according to the previous typology.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{

## This example uses a sample of original tweets 
## using the hashtag #cuentalo:
data(users)
R <- type(users)
table(R$type)
}