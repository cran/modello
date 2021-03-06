% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{adam.opt}
\alias{adam.opt}
\title{Create Adam optmiser}
\usage{
adam.opt(pars)
}
\arguments{
\item{pars}{list of reference objects of class 'number' representing
the parameters to optmise.}
}
\value{
a reference object of class 'adam.opt'
}
\description{
Creates an Adam optimiser.
}
\examples{
modello.init(10, 10, 10, 10)
mdl = module.lm$new(1, 3, 1)
optimizer = adam.opt(mdl$pars())
is.opt(optimizer) # TRUE
modello.close()
}
