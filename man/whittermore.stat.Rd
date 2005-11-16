\name{whittermore.stat}

\alias{whittermore.stat}

\title{Compute Whittermore's statistic}

\description{
Compute Whittermore's statistic. See \emph{whittermore} manual page
for more details.
}


\usage{
whittermore.stat(data, listw, zero.policy=FALSE)
}

\arguments{
\item{data}{Dataframe with the data, as desribed in \emph{DCluster}
manual page.}
\item{listw}{Neighbours list with spatial weights created, for example,
by 'nb2listw' (package \emph{spdep}).}
\item{zero.policy}{See \emph{nb2listw} in package \emph{spdep}.}
}

\value{
The value of the statistic.
}

\seealso{
DCluster, whittermore, whittermore.boot, whittermore.pboot
}

\examples{
library(spdep)
data(nc.sids)
col.W <- nb2listw(ncCR85.nb, zero.policy=TRUE)

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
col.W.whitt<-nb2listw(dlist, glist=dlist.d, style="C")


whittermore.stat(sids, col.W.whitt, zero.policy=TRUE)
}

\references{
Whittermore, A. S. and Friend, N. and Byron, W. and Brown, J. R. and Holly, E. A. (1987). A test to detect clusters of disease. Biometrika 74, 631-635.
}


\keyword{spatial}
