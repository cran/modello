% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numbers_math.R
\name{mse.modello_number}
\alias{mse.modello_number}
\alias{mse.default}
\alias{mse}
\title{Mean Squared Error}
\usage{
\method{mse}{modello_number}(y, yh)

\method{mse}{default}(y, yh)

mse(y, yh)
}
\arguments{
\item{y}{target values}

\item{yh}{predictions}
}
\value{
Returns the mse
}
\description{
Mean Squared Error between target values and predictions
\deqn{mse = \sum_i^N{(y_i - \hat y_i)^2} / N}
}
\examples{
modello.init(10, 10, 10, 10)
## For modello_numbers
y = number(rnorm(10), dx=FALSE)
yh = number(rnorm(10))
h = mse(y, yh)
print(h)
print(h$v)
modello.close()
## For numerics
y = rnorm(10)
yh = rnorm(10)
h = mse(y, yh)
print(h)
}
\author{
Filippo Monari
}
