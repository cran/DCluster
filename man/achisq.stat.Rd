\name{achisq.stat}

\alias{achisq.stat}

\title{Another implementation of Pearson's Chi-square statistic}


\description{
Compute Pearson's Chi-square statistic. See \emph{achisq} manual page
for more details.
}


\usage{
achisq.stat(data, lambda=NULL)
}

\arguments{
\item{data}{A dataframe containing the data, as specified in the 
\bold{DCluster}manpage.}
\item{lambda}{The value of the relative risks under the null hypotheses.
If its NULL, the second hypotheses commented above is considered
and the expected number of cases will automatically be corrected.}
}

\value{
A list with three components
\item{T}{The value of the statistic.}
\item{df}{Degrees of freedom of the asinthotic Chi-square distribution.}
\item{pvalue}{Related pvalue.}
}


\seealso{
DCluster, achisq, achisq.boot, achisq.pboot
}


\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))

#Compute the statistic under the assumption that lambda = 1.
achisq.stat(sids, lambda=1)
}

\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
