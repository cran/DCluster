\name{besagnewell.stat}

\alias{besagnewell.stat}

\title{Besag and Newell's Statistic for Spatial Clustering}

\description{
\emph{besagnewell.stat} computes the statistic around a single location.
Data passed must be sorted according to distance to central region,
which is supposed to be the first row in the dataframe. Notice that the
size of the cluster is \emph{k+1}.

}


\usage{
besagnewell.stat(data, k)
}


\arguments{
\item{data}{A dataframe with the data, as explained in \emph{DCluster}.}
\item{k}{Cluster size.}
}


\value{
A vector of two elements: the value of the statistic and the size of the
cluster (which is equal to the value of the statistic).
}

\seealso{
DCluster, besagnewell, besagnewell.boot, besagnewell.pboot
}

\references{
Besag, J. and Newell, J.(1991). The detection of clusters in rare diseases. 
Journal of the Royal Statistical Society A  154, 143-155.
}

\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

besagnewell.stat(sids, k=20)
}

\keyword{spatial}
