\name{kullnagar.boot}

\alias{kullnagar.boot}
\alias{kullnagar.pboot}

\title{Generate bootstrap replicates of Kulldorff and Nagarwalla's statistic}

\description{
Generate bootstrap replicates of Kulldorff and Nagarwalla's statistic,
by calling functions \emph{boot} and \emph{kullnagar.stat}.

\emph{kullnagar.boot} is used when using non-parametric bootstrap to estimate
the distribution of the statistic.

\emph{kullnagar.pboot} is used when performing parametric bootstrap.
}


\usage{
kullnagar.boot(data, i, ...)
kullnagar.pboot(...)
}


\arguments{
\item{data}{A dataframe with the data as explained in \emph{DCluster}.}
\item{i}{Permutation created in non-parametric bootstrap.}
\item{...}{Additional arguments passed to the functions.}
}



\value{
Both functions return the value of the statistic.
}


\seealso{
DCluster, boot, kullnagar, kullnagar.stat, kn.iscluster
}


\examples{
library(boot)
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, Population=nc.sids$BIR74, x=nc.sids$x, y=nc.sids$y)

niter<-100

#Permutation  model
kn.perboot<-boot(sids, statistic=kullnagar.boot, R=niter, fractpop=.2)
plot(kn.perboot)#Display results

#Multinomial model
kn.mboot<-boot(sids, statistic=kullnagar.pboot, sim="parametric", 
	ran.gen=multinom.sim,  R=niter, fractpop=.2)
plot(kn.mboot)#Display results

#Poisson model
kn.pboot<-boot(sids, statistic=kullnagar.pboot, sim="parametric", 
	ran.gen=poisson.sim,  R=niter, fractpop=.2)
plot(kn.pboot)#Display results

#Poisson-Gamma model
kn.pgboot<-boot(sids, statistic=kullnagar.pboot, sim="parametric", 
	ran.gen=negbin.sim, R=niter, fractpop=.2)
plot(kn.pgboot)#Display results


}



\references{
Kulldorff, Martin and Nagarwalla, Neville (1995). Spatial Disease Clusters: Detection and Inference. Statistics in Medicine 14, 799-810.
}

\keyword{spatial}
