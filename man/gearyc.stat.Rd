\name{gearyc.stat}

\alias{gearyc.stat}
\alias{gearyc.test}

\title{Compute Moran's I autocorrelation statistic}


\description{
Compute Moran's I autocorrelation statistic using either \bold{residuals}
or \bold{SMRs} by means of cuntion \emph{geary} from package \emph{spdep}.

gearyc.stat computes the test statistic and the test using a hi-square
distribution whilst gearyc.test performs a bootstrap test.
}


\usage{
gearyc.stat(data, applyto="SMR", ...)
gearyc.test(formula, data, model, R, ...)
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
'gearyc.test' the remaining arguments in 'gearyc.stat' not included in
'gearyc.test'. This is done so because gearyc.test calls gearyc.stat in order
to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
\item{applyto}{A string with the name of the statistic with
which calculate Moran's Index. It may be either \emph{residulas}
or \emph{SMR}.}
}

\seealso{
DCluster, geary, gearyc, gearyc.boot, gearyc.pboot
}


\examples{
library(spdep)
data(nc.sids)
col.W <- nb2listw(ncCR85.nb, zero.policy=TRUE)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))

gearyc.stat(data=sids, listw=col.W, n=length(ncCR85.nb), n1=length(ncCR85.nb)-1,
	S0=Szero(col.W) )

gearyc.stat(data=sids, applyto="SMR", listw=col.W, n=length(ncCR85.nb), 
	n1=length(ncCR85.nb)-1,S0=Szero(col.W) )

gearyc.test(Observed~offset(log(Expected)), data=sids, model="poisson", R=99,
   applyto="SMR", listw=col.W, n=length(ncCR85.nb), 
	n1=length(ncCR85.nb)-1,S0=Szero(col.W) )
}

\references{
Geary, R. C. (1954). The contiguity ratio and statistical mapping. The Incorporated Statistician 5, 115-145.
}

\keyword{spatial}
