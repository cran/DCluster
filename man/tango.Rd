\name{tango}

\alias{tango}

\title{Tango's statistic for general clustering}

\description{
Tango's statistic to perform a general clustering test is expressed as
follows:

\deqn{T = (r-p)^{'} A (r-p)}{T = (r-p)' A (r-p)}

where \eqn{r^{'} = [O_1/O_+, \ldots, O_n/O_+]}{r' = [O_1/O_+, ..., O_n/O_+]}, 
\eqn{p^{'}=[E_1/E_+, \ldots, E_n/E_+]}{p' = [E_1/E_+, ..., E_n/E_+]} 
and \eqn{A}{A} is a matrix of closeness which measures
the cloneness between two zones (the higher the closer).

Tango proposes to take \eqn{A_{ij}=\exp\{-D_{ij}/\phi\}}{A_ij=exp(-D/phi)},
where \eqn{D_{ij}}{D_ij} is the distance between centroids of regions \emph{i}
and \emph{j}, and \eqn{\phi}{phi} is a constant that measures how strong is the
relationship between regions in a general way.

}

\seealso{
DCluster, tango.stat, tango.boot, tango.pboot
}

\references{
Tango, Toshiro (1995). A Class of Tests for Detecting 'General' and 'Focused' Clustering of Rare Diseases. Statistics in Medicine 14, 2323-2334.
}

\keyword{spatial}
