\name{kn.iscluster}

\alias{kn.iscluster}
\alias{kn.gumbel.iscluster}

\title{Clustering Function for Kulldorff and Nagarwalla's Statistic}

\description{
\emph{kn.iscluster} is called from \emph{opgam} when studying the whole
area. At every point of the grid, which may be all the centroids, this
function is called to determine whether it is a cluster or not by
calculating Kulldorff and Nagarwalla's statistic.

See \emph{opgam.iscluster.default} for more details. \emph{kn.gumbel.iscluster}
uses a Gumbel distribution to compute the p-values ofr each possible
cluster.

}


\usage{
kn.iscluster(data, idx, idxorder, alpha, fractpop, use.poisson=TRUE,
 model="poisson", R, mle, ...)
kn.gumbel.iscluster(data, idx, idxorder, alpha, fractpop, use.poisson=TRUE,
 model="poisson", R, mle)

}


\arguments{
\item{data}{A dataframe with the data as explained in \emph{DCluster}.}
\item{idx}{A boolean vector to know the areas in the current circle.}
\item{idxorder}{A permutation of the rows of data to order the regions
according to their distance to the current center.}
\item{alpha}{Test signifiance.}
\item{fractpop}{Maximum fraction of the total population used when
creating the balls.}
\item{use.poisson}{Use the statistic for Poisson (default) or Bernouilli case.}
\item{model}{Thge model used to generate random observations. It can be
'permutation', 'multinomial', 'poisson' or 'negbin'. See \emph{observed.sim} manual page for details.}
\item{R}{The number of bootstrap replicates to generate.}
\item{mle}{Parameters need by the bootstrap procedure.}
\item{...}{Extra arguments to be passed to kullnagar.stat().}
}

\value{
A vector of four elements, as describe in \emph{iscluster} manual page.
}
\seealso{
DCluster, kullnagar, kullnagar.stat, kullnagar.boot, kullnagar.pboot
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
  iscluster=kn.iscluster, fractpop=.5, R=100, model="poisson", mle=mle)

kngumbelres<-opgam(data=sids, thegrid=sids[,c("x","y")], alpha=.05, 
  iscluster=kn.gumbel.iscluster, fractpop=.5, R=100, model="poisson", 
  mle=mle)

#Plot all centroids and significant ones in red
plot(sids$x, sids$y, main="Kulldorff and Nagarwalla's method")
points(knresults$x, knresults$y, col="red", pch=19)
points(knresults$x, knresults$y, col="blue", pch=20)
}

\references{
Kulldorff, Martin and Nagarwalla, Neville (1995). Spatial Disease Clusters: 
 Detection and Inference. Statistics in Medicine 14, 799-810.
Abrams A, Kleinman K, Kulldorff M (2010). Gumbel based p-value approximations
 for spatial scan statistics. International Journal of Health Geographics, 9:61.
}

\keyword{spatial}
