\name{moranI.stat}

\alias{moranI.stat}
\alias{moranI.test}

\title{Compute Moran's I Autocorrelation Statistic}


\description{
Compute Moran's I autocorrelation statistic using \bold{residuals}
or \bold{SMRs} by means of function \emph{moran} from package 
\emph{spdep}.

moranI.stat computes the test statistic and the test using a hi-square
distribution whilst moranI.test performs a bootstrap test.
}


\usage{
moranI.stat(data, applyto="SMR", ...)
moranI.test(formula, data, model, R, ...)
}

\arguments{
\item{formula}{Formula that specifies the underlying model. The observed
cases are the response and the expected number of cases must be specified
as an offset in the log scale (see example below).  Note that now it
is not necessary to use Observed and Expected and that any other
names can be used to specify the observed and expected cases.}
\item{model}{Parametric model to be used in the bootstrap test. One of
"param", "multinom", "poisson" or "negbin". See the \bold{DCluster} manpage for
details.}
\item{...}{Arguments needed by
function \emph{moran} from package \emph{spdep}. In addition, when calling 
'moranI.test' the remaining arguments in 'moranI.stat' not included in
'moranI.test'. This is done so because moranI.test calls moranI.stat in order
to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
\item{applyto}{A string with the name of the statistic with
which calculate Moran's Index. It may be either \emph{residulas}
or \emph{SMR}.}
}


\value{
The value of the statistic computed.
}
\seealso{
DCluster, moran, moranI, moranI.boot, MoranI.pboot
}

\examples{
library(spdep)
data(nc.sids)
col.W <- nb2listw(ncCR85.nb, zero.policy=TRUE)


sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74) )

moranI.stat(data=sids, listw=col.W, n=length(ncCR85.nb), S0=Szero(col.W) )

moranI.stat(data=sids, applyto="residuals", listw=col.W, n=length(ncCR85.nb), 
	S0=Szero(col.W) )

moranI.test(Observed~offset(log(Expected)), sids, model="poisson", R=99,
   listw=col.W, n=length(ncCR85.nb), S0=Szero(col.W) )

}

\references{
Moran, P. A. P. (1948). The interpretation os statistical maps. Journal of the Royal Statistical Society, Series B 10, 243-251.
}

\keyword{spatial}
