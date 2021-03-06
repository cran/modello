% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numbers_math.R
\name{cross.entropy.modello_number}
\alias{cross.entropy.modello_number}
\alias{cross.entropy.default}
\alias{cross.entropy}
\title{Cross-entropy}
\usage{
\method{cross.entropy}{modello_number}(y, yh)

\method{cross.entropy}{default}(y, yh)

cross.entropy(y, yh)
}
\arguments{
\item{y}{predicted probability}

\item{yh}{target classes (0 or 1)}
}
\value{
Returns the entropy
}
\description{
Cross-entropy (Negaitve Multinomial log-likelihood)
\deqn{\sum_i{y_i * log(\hat y_i)}}
}
\examples{
modello.init(10, 10, 10, 10)
## For modello_numbers
y = number(matrix({x=sample(c(1, 0), 5, replace=TRUE); c(x, 1-x)}, 5, 2), dx=FALSE)
yh = number(t(apply(matrix(runif(10), 5, 2), 1, function(x)x/sum(x))))
h = cross.entropy(y, yh)
print(h)
print(h$v)
modello.close()
## For numerics
y = matrix({x=sample(c(1, 0), 5, replace=TRUE); c(x, 1-x)}, 5, 2)
yh = t(apply(matrix(runif(10), 5, 2), 1, function(x)x/sum(x)))
h = cross.entropy(y, yh)
print(h)
}
\author{
Filippo Monari
}
