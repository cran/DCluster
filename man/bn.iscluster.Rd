\name{bn.iscluster}

\alias{bn.iscluster}

\title{Clustering function for Besag and Newell's method}

\description{
This function is used to calculate the significance of the agregation
of cases around the current area when scanning the whole
area by means of function \emph{opgam}.

When data sampling distribution is  \emph{multinomial} or \emph{poisson} 
the exact p-value is computed. In the other cases (i.e.,
permutation and negative binomial) it is aproximated by bootstrap.

This function must be passed to function \emph{opgam} as argument
\emph{iscluster}.
}


\usage{
bn.iscluster(data, idx, idxorder, alpha, k, model="poisson", R=999, mle)
}


\arguments{
\item{data}{A dataframe with the data as explained in \emph{DCluster}.}
\item{idx}{A boolean vector to know the areas in the current circle.}
\item{idxorder}{A permutation of the rows of data to order the regions
according to their distance to the current centre.}
\item{alpha}{Test significance.}
\item{k}{Size of the cluster.}
\item{model}{Thge model used to generate random observations. It can be
'permutation', 'multinomial', 'poisson' or 'negbin'.}
\item{R}{Number of bootstrap replicates made to compute pvalue if
the local test.}
\item{mle}{Parameters needed to compute the Negative Binomial distribution (if used). See \emph{negbin.sim} manual page for details.}
}

\value{
A vector of four elements, as described in \emph{iscluster} manual page.
}



\references{
Besag, J. and Newell, J.(1991). The detection of clusters in rare diseases.
Journal of the Royal Statistical Society A  154, 143-155.
}


\examples{
library(boot)
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#B&N's method
bnresults<-opgam(data=sids, thegrid=sids[,c("x","y")], alpha=.05, 
	iscluster=bn.iscluster, k=20, R=100, model="poisson", 
	mle=calculate.mle(sids))

#Plot all centroids and significant ones in red
plot(sids$x, sids$y, main="Besag & Newell's method")
points(bnresults$x, bnresults$y, col="red", pch=19)
}

\seealso{
DCluster, besagnewell, besagnewell.boot, besagnewell.pboot
}

\keyword{spatial}
