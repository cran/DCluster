\name{tango.stat}

\alias{tango.stat}
\alias{tango.test}

\title{Compute Tango's Statistic for General Clustering}

\description{
Compute Tango's statistic for general clustering. See \emph{tango} manual
page for details.

tango.stat computes the test statistic and the test using a hi-square
distribution whilst tango.test performs a bootstrap test.
}

\usage{
tango.stat(data, listw, zero.policy)
tango.test(formula, data, model, R, ...)
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
\item{...}{The remaining arguments in 'tango.stat' not included in
'tango.test'. This is done so because tango.test calls tango.stat in order to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
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

#use exp(-D) as closeness matrix
tango.stat(sids, col.W.tango, zero.policy=TRUE)

tango.test(Observed~offset(log(Expected)), sids, model="poisson", R=99, 
   list=col.W.tango, zero.policy=TRUE)
}

\seealso{
DCluster, tango, tango.boot, tango.pboot
}

\references{
Tango, Toshiro (1995). A Class of Tests for Detecting 'General' and 'Focused' Clustering of Rare Diseases. Statistics in Medicine 14, 2323-2334.
}

\keyword{spatial}
