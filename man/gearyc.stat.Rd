\name{gearyc.stat}

\alias{gearyc.stat}

\title{Compute Moran's I autocorrelation statistic}


\description{
Compute Moran's I autocorrelation statistic using either \bold{residuals}
or \bold{SMRs} by means of cuntion \emph{geary} from package \emph{spdep}.
}


\usage{
gearyc.stat(data, applyto="residuals", ...)
}

\arguments{
\item{data}{A dataframe containing the data, as specified in the 
\bold{DCluster} manpage.}
\item{applyto}{A string with the name of the statistic with
which calculate Moran's Index. It may be either \emph{residulas}
or \emph{SMR}.}
\item{...}{Additional arguments needed by function \emph{moran} from package
\emph{spdep}}
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

}

\references{
Geary, R. C. (1954). The contiguity ratio and statistical mapping. The Incorporated Statistician 5, 115-145.
}

\keyword{spatial}
