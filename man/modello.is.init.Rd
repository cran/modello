% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{modello.is.init}
\alias{modello.is.init}
\title{Modello Is Initialised}
\usage{
modello.is.init()
}
\value{
Returns TRUE/FALSE
}
\description{
Checks if the modello sesssion is initialised
}
\examples{
modello.init(10, 10, 10, 10) # Initilaises the modello session with arrays of the given size
modello.is.init() # TRUE
modello.reset()   # restart the modello session with initial array sizes 
modello.is.init() # TRUE
modello.close()   # Closes modello
modello.is.init() # FALSE
}
\author{
Filippo Monari
}
