\name{pottwhitt.stat}

\alias{pottwhitt.stat}

\title{Compute Potthoff-Whittinghill's statistic}


\description{

Compute Pottwhoff-Whittinghill's statistic.

}


\usage{
pottwhitt.stat(data)
}

\arguments{
\item{data}{A dataframe containing the data, as specified in the 
\bold{DCluster}manpage.}
}

\value{
A list with the following elements:

\item{T}{The value of the statistic.}
\item{asintmean}{Mean of the asymptotical Normal distribution.}
\item{asintvar}{Variance of the asymptotical Normal distribution.}
\item{pvalue}{Significance of the statistic.}
}

\seealso{
DCluster
}


\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

pottwhitt.stat(sids)
}

\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
