\name{achisq.stat}

\alias{achisq.stat}
\alias{achisq.test}

\title{Another implementation of Pearson's Chi-square statistic}


\description{
Compute Pearson's Chi-square statistic. See \emph{achisq} manual page
for more details.

achisq.stat computes the test statistic and the test using a hi-square
distribution whilst achisq.test performs a bootstrap test. 

}


\usage{
achisq.stat(data, lambda=NULL)
achisq.test(formula, data, model, R, ...)
}

\arguments{
\item{formula}{Formula that specifies the underlying model. The observed
cases are the response and the expected number of cases must be specified
as an offset in the log scale (see example below).  Note that now it
is not necessary to use Observed and Expected and that any other 
names can be used to specify the observed and expected cases.
}
\item{model}{Parametric model to be used in the bootstrap test. One of
"param", "multinom", "poisson" or "negbin". See the \bold{DCluster} manpage for
details.}
\item{...}{The remaining arguments in 'achisq.stat' not included in
'achisq.test'. This is done so because achisq.test calls achisq.stat in order to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the 
\bold{DCluster} manpage.}
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

#Perform test
achisq.test(Observed~offset(log(Expected)), sids, model="poisson", R=99)
}

\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
