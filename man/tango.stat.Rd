\name{tango.stat}

\alias{tango.stat}

\title{Compute Tango's statistic for general clustering}

\description{
Compute Tango's statistic for general clustering. See \emph{tango} manual
page for details.
}

\usage{
tango.stat(data, listw, zero.policy)
}

\arguments{
\item{data}{Dataframe with the data as described in \emph{DCluster}.}
\item{listw}{Neighbours list with spatial weights created, for example,
by 'nb2listw' (package \emph{spdep}).}
\item{zero.policy}{See \emph{nb2listw} in package \emph{spdep}.}
}


\examples{
library(spdep)
data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74) )
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#Calculate neighbours based on distance
coords<-as.matrix(sids[,c("x", "y")])

dlist<-dnearneigh(coords, 0, Inf)
dlist<-include.self(dlist)
dlist.d<-nbdists(dlist, coords)

#Calculate weights. They are globally standardised but it doesn't
#change significance.
col.W.tango<-nb2listw(dlist, glist=lapply(dlist.d, function(x) {exp(-x)}),
        style="C")

niter<-100

#use exp(-D) as closeness matrix
tango.stat(sids, col.W.tango, zero.policy=TRUE)

}

\seealso{
DCluster, tango, tango.boot, tango.pboot
}

\references{
Tango, Toshiro (1995). A Class of Tests for Detecting 'General' and 'Focused' Clustering of Rare Diseases. Statistics in Medicine 14, 2323-2334.
}

\keyword{spatial}
