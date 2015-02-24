\name{opgam}

\alias{opgam}
\alias{opgam.intern}

\title{Openshaw's GAM}

\description{
Scan an area with Openshaw's Geographical Analysis Machine to look
for clusters.

\emph{opgam} is the main function, while \emph{gam.intern} is called
from there.
}


\details{
The \emph{Geographical Analysis Machine} was developed by Openshaw
et al. to perform geographical studies of the relationship between
different types of cancer and their proximity to nuclear plants.

In this method, a grid of a fixed step is built along the study region, and
small balls of a given radius are created at each point of the grid. Local
observed and expected number of cases and population are calculated and a
function is used to assess whether the current ball is a cluster or not.  For
more information about this function see \emph{opgam.iscluster.default}, which
is the default function used.

If the obverved number of cases excess a critical value, which is calculated
by a function passed as an argument, then that circle is marked as a possible
cluster. At the end, all possible clusters are drawn on a map. Clusters may be
easily identified then.


Notice that we have follow a pretty flexible approach, since user-implemented
functions can be used to detect clusters, such as those related to
ovedispersion (Pearson's Chi square statistic, Potthoff-Whittinghill's
statistic) or autocorrelation (Moran's I statistic and Geary's c statistic),
or a bootstrap procedure, although it is not recommended because it can
be VERY slow.
}


\usage{
opgam(data, thegrid=NULL, radius=Inf, step=NULL, alpha,
 iscluster=opgam.iscluster.default, set.idxorder=TRUE, ...)
opgam.intern(point, data, rr, set.idxorder, iscluster, alpha, ...)
}

\arguments{
\item{data}{A dataframe with the data, as described in \emph{DCluster} manual
page.}

\item{thegrid}{A two-columns matrix containing
the points of the grid to be used. If it is null, a rectangular grid of step 
\emph{step} is built.}

\item{radius}{The radius of the circles used in the computations.}

\item{step}{The step of the grid.}

\item{alpha}{Significance level of the tests performed.}

\item{iscluster}{Function used to decide whether the current circle
is a possible cluster or not. It must have the same arguments
and return the same object than \emph{gam.iscluster.default}}.

\item{set.idxorder}{Whether an index for the ordering by distance
to the center of the current ball is calculated or not.}

\item{point}{Point where the curent ball is centred.}

\item{rr}{rr=radius*radius .}

\item{...}{Aditional arguments to be passed to \emph{iscluster}.}
}


\value{
A dataframe with five columns:

\item{x}{Easting coordinate of the center of the cluster.}

\item{y}{Northing coordinate of the center of the cluster.}

\item{statistic}{Value of the statistic computed.}

\item{cluster}{Is it a cluster (according to the criteria used)? It should
be always TRUE.}

\item{pvalue}{Significance of the cluster.} 
}


\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)


#GAM using the centroids of the areas in data
sidsgam<-opgam(data=sids,  radius=30, step=10, alpha=.002)

#Plot centroids
plot(sids$x, sids$y, xlab="Easting", ylab="Northing")
#Plot points marked as clusters
points(sidsgam$x, sidsgam$y, col="red", pch="*")

}

\seealso{
DCluster, opgam.iscluster.default
}

\references{
Openshaw, S. and Charlton, M. and Wymer, C. and Craft, A. W. (1987). A mark I geographical analysis machine for the automated analysis of point data sets. International Journal of Geographical Information Systems 1, 335-358.

Waller, Lance A. and Turnbull, Bruce W. and Clarck, Larry C. and Nasca, Philip (1994). Spatial Pattern Analyses to Detect Rare Disease Clusters. In 'Case Studies in Biometry'. Chapter 1, 3-23.
} 

\keyword{spatial}
