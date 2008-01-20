\name{pottwhitt.stat}

\alias{pottwhitt.stat}
\alias{pottwhitt.test}

\title{Compute Potthoff-Whittinghill's statistic}


\description{

Compute Pottwhoff-Whittinghill's statistic.

pottwhitt.stat computes the test statistic and the test using a hi-square
distribution whilst pottwhitt.test performs a bootstrap test.

}


\usage{
pottwhitt.stat(data)
pottwhitt.test(formula, data, model, R, ...)

}

\arguments{
\item{formula}{Formula that specifies the underlying model. The observed
cases are the response and the expected number of cases must be specified
as an offset in the log scale (see example below). Note that now it
is not necessary to use Observed and Expected and that any other 
names can be used to specify the observed and expected cases.

}
\item{model}{Parametric model to be used in the bootstrap test. One of
"param", "multinom", "poisson" or "negbin". See the \bold{DCluster} manpage for details.}
\item{...}{The remaining arguments in 'achisq.stat' not included in
'achisq.test'. This is done so because achisq.test calls achisq.stat in order to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
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

pottwhitt.test(Observed~offset(log(Expected)),sids, model="poisson", R=99)
}

\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
