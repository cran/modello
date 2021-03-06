% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{graph.open}
\alias{graph.open}
\title{Open Graph}
\usage{
graph.open(g = NULL)
}
\arguments{
\item{g}{a reference object of class 'graph'}
}
\value{
Returns a reference object of class 'graph' linked to the open \code{graph}
}
\description{
Open a \code{graph}
}
\details{
If \code{g} is NULL a new \code{graph} is open. Otrewise
the \code{graph} liked to \code{g} is open.
}
\examples{
modello.init(10, 10, 10, 10)
x1 = number(1)
x2 = number(3)
x3 = number(2)
g = graph.open()
y = log((x1 + x3)**x2)
graph.close()
print(y$v)
x1$v = 2
g$op()
print(y$v)
y$dv = 1
g$bw()
print(x1$dv)
print(x2$dv)
print(x3$dv)
modello.close()
}
\author{
Filippo Monari
}
