% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numbers_math.R
\name{tanh.modello_number}
\alias{tanh.modello_number}
\title{Number TanH}
\usage{
\method{tanh}{modello_number}(x)
}
\arguments{
\item{x}{a reference object of class 'number'}
}
\value{
Returns a reference object of class 'number'
}
\description{
Calculates the hyperbolic tangent of a \code{number}
}
\examples{
modello.init(10, 10, 10, 10)
## For sclars
x1 = number(pi, scalar=TRUE)
y1 = tanh(x1)
print(y1)
print(y1$v)
## For arrays
x2 = number(matrix(pi, 3, 3))
y2 = tanh(x2)
print(y2)
print(y2$v)
modello.close()
}
\author{
Filippo Monari
}
