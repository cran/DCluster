\name{whittermore.stat}

\alias{whittermore.stat}
\alias{whittermore.test}

\title{Compute Whittermore's Statistic}

\description{
Compute Whittermore's statistic. See \emph{whittermore} manual page
for more details.

whittermore.stat computes the test statistic and the test using a hi-square
distribution whilst whittermore.test performs a bootstrap test.
}


\usage{
whittermore.stat(data, listw, zero.policy=FALSE)
whittermore.test(formula, data, model, R, ...)
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
\item{...}{The remaining arguments in 'whittermore.stat' not included in
'whittermore.test'. This is done so because whittermore.test calls whittermore.stat in order to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
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

whittermore.test(Observed~offset(log(Expected)), sids, model="poisson", R=99, 
   listw=col.W.whitt, zero.policy=TRUE)
}

\references{
Whittermore, A. S. and Friend, N. and Byron, W. and Brown, J. R. and Holly, E. A. (1987). A test to detect clusters of disease. Biometrika 74, 631-635.
}


\keyword{spatial}
