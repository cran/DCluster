\name{get.knclusters}

\alias{get.knclusters}

\title{Get areas in a cluster detected with Kulldorff's statistic}

\description{
When \emph{kn.iscluster} is called from \emph{opgam} to use Kulldorff's scan
statistic for the detection of clusters of disease, \emph{get.knclusters} can
be used to get the areas included in each cluster. \emph{opgam} only returns
the cluster centres, size and related information but not the areas in the
cluster.

}


\usage{
get.knclusters(d, knresults)
}


\arguments{
\item{d}{Data frame with the data, used in the call to \emph{opgam}.}
\item{knresults}{Data frame returned by a call to \emph{opgam}.}
}

\value{
Returns a list with the same length as the number of rows in 'knresults'.  Each
element in the list is a vector of integers with the row indices of 'd' of the
areas in the cluster. The order of the indices reflects the distance to the
cluster centre.
}
\seealso{
DCluster, kullnagar, kullnagar.stat, kullnagar.boot, kullnagar.pboot, opgam
}


\examples{
library(boot)
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, Population=nc.sids$BIR74, x=nc.sids$x, y=nc.sids$y)

#K&N's method over the centroids
mle<-calculate.mle(sids, model="poisson")
knresults<-opgam(data=sids, thegrid=sids[,c("x","y")], alpha=.05, 
	iscluster=kn.iscluster, fractpop=.15, R=99, model="poisson", mle=mle)

#Plot all centroids and significant ones in red
plot(sids$x, sids$y, main="Kulldorff and Nagarwalla's method")
points(knresults$x, knresults$y, col="red", pch=19)

#Plot first cluster with the highest likelihood ratio test in green
clusters<-get.knclusters(sids, knresults)
idx<-which.max(knresults$statistic)
points(sids$x[clusters[[idx]]], sids$y[clusters[[idx]]], col="green", pch=19)
}

\references{
Kulldorff, Martin and Nagarwalla, Neville (1995). Spatial Disease Clusters: Detection and Inference. Statistics in Medicine 14, 799-810.
}

\keyword{spatial}
