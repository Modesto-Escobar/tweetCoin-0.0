\name{urltoText}
\alias{urltoText}
\title{URL to HTML link Wiki converter}
\description{
Converts a Wikipedia URL to an HTML link.

}
\usage{
urltoText(url, text = NULL)
}
\arguments{
  \item{url}{Character vector of URLs.}
  \item{text}{A vector with name of the correspondent Wikipedia's page of the url (See details).}
}
\details{

This function converts an available Wikipedia URL direction to the corresponding HTML link, i.e., ""https://es.wikipedia.org/wiki/Socrates" changes into "<a href='https://es.wikipedia.org/wiki/Socrates', target='_blank'>Socrates</a>".

It is possible to change the showing name of the link directly using the argument /text. When not specified, it is extracted directly from the URL.
}

\value{
A character vector of names' HTML links.
}

\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}

\examples{

## When you have a single URL:

urltoText( "https://es.wikipedia.org/wiki/Socrates", text = "Socrates")

## It is possible to work with several items:

A <- c("https://es.wikipedia.org/wiki/Socrates", 
       "https://es.wikipedia.org/wiki/Plato", 
       "https://es.wikipedia.org/wiki/Aristotle" )
urltoText ( A, text = c("Socrates", "Plato", "Aristotle"))

## And  you can also directly extract the info from nametoWikiURL():

urltoText(nametoWikiURL("Plato", "en"), "Plato" )
urltoText(nametoWikiURL(c("Plato", "Socrates", "Aristotle"), 
                        language="en"), 
                        c("Plato", "Socrates", "Aristotle"))
}
