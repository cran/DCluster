\name{moranI}

\alias{moranI.stat}

\title{Compute Moran's I autocorrelation statistic}


\description{
Compute Moran's I autocorrelation statistic using \bold{residuals}
or \bold{SMRs} by means of function \emph{moran} from package 
\emph{spdep}.
}


\usage{
moranI.stat(data, applyto="residuals", ...)
}

\arguments{
\item{data}{A dataframe containing the data, as specified in the 
\bold{DCluster}manpage.}
\item{applyto}{A string with the name of the statistic with
which calculate Moran's Index. It may be either \emph{residulas}
or \emph{SMR}.}
\item{...}{Additional arguments needed by function \emph{moran} from package
\emph{spdep}}
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

moranI.stat(data=sids, applyto="SMR", listw=col.W, n=length(ncCR85.nb), 
	S0=Szero(col.W) )
}

\references{
Moran, P. A. P. (1948). The interpretation os statistical maps. Journal of the Royal Statistical Society, Series B 10, 243-251.
}

\keyword{spatial}
