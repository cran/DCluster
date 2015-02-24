\name{kullnagar.stat}

\alias{kullnagar.stat}
\alias{kullnagar.stat.poisson}
\alias{kullnagar.stat.bern}


\title{Kulldorff and Nagarwalla's Statistic for Spatial Clustering.}

\description{
Compute Kulldorff and Nagarwalla's spatial statistic for cluster detection
around a single region, which is supposed to be the first row of the
dataframe. The other regions are supposed to be sorted by distance to 
the centre in the data frame.

Two possible function are provided: \emph{kullnagar.stat.poisson}, for th
Poisson case, and \emph{kullnagar.stat.bern}, for the Bernouilli case.

See \emph{kullnagar} manual page for details.
}


\usage{
kullnagar.stat(data, fractpop, use.poisson=TRUE, log.v=FALSE)
}


\arguments{
\item{data}{A dataframe with the data as explained in \emph{DCluster}.}
\item{fractpop}{Maximum fraction of the total population used when
creating the balls.}
\item{use.poisson}{Use the statistic for Poisson (default) or Bernouilli case.}
\item{log.v}{Whether the logarithm of the statistic is returned or not.}
}

\value{
Returns a vector of two elements:  the value of the statistic and the
size (in number of regions) of the cluster.
}


\seealso{
DCluster, kullnagar, kullnagar.stat, kullnagar.boot, kullnagar.pboot
}


\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, Population=nc.sids$BIR74, x=nc.sids$x, y=nc.sids$y)


dist<-(sids$x-sids$x[1])^2+(sids$y-sids$y[1])^2
index<-order(dist)
#Compute the statistic around the first county
kullnagar.stat(sids[index,], fractpop=.5)
}


\references{
Kulldorff, Martin and Nagarwalla, Neville (1995). Spatial Disease Clusters: Detection and Inference. Statistics in Medicine 14, 799-810.
}

\keyword{spatial}
