\name{besagnewell}

\alias{besagnewell}

\title{Besag and Newell's Statistic for Spatial Clustering}

\description{
Besag & Newell's statistic looks for clusters of size \emph{k}, i. e., where
the number of observed cases is \emph{k}. At every area where a case has
appeared, the number of neighbouring regions needed to reach $k$ cases is
calculated.  If this number is too small, that is, too many observed cases in
just a few regions with low expected cases, then it is marked as a cluster.
}


\seealso{
DCluster, besagnewell.stat, besagnewell.boot, besagnewell.pboot, bn.iscluster
}

\references{
Besag, J. and Newell, J.(1991). The detection of clusters in rare diseases. 
Journal of the Royal Statistical Society A  154, 143-155.
}

\examples{
#B&N must use the centroids as grid.
#The size of teh cluster is 20.
#100  bootstrap simulations are performed
#Poisson is the model used in the bootstrap simulations to generate the
#observations.
#Signifiance level is 0'05, even though multiple tests are made.

library(boot)
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

bnresults<-opgam(sids, thegrid=sids[,c("x","y")], alpha=.05, 
	iscluster=bn.iscluster, set.idxorder=TRUE, k=20, model="poisson", 
	R=100, mle=calculate.mle(sids) )

#Plot all the centroids
plot(sids$x, sids$y)

#Plot signifiant centroids in red
points(bnresults$x, bnresults$y, col="red", pch=19)
}

\keyword{spatial}
