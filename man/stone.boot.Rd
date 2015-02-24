\name{stone.boot}

\alias{stone.boot}
\alias{stone.pboot}

\title{Generate Boostrap Replicates of Stone's Statistic}

\description{
Generate bootstrap replicates of Stone's statictic, by means of function
\emph{boot} from \emph{boot} package. Notice that these functions should not
be used separately but as argument \emph{statistic} when calling function
\emph{boot}.


\emph{stone.boot} is used when performing a non-parametric bootstrap.

\emph{stone.pboot} is used when performing a parametric bootstrap.
}


\usage{
stone.boot(data, i, ...)
stone.pboot(...)
}


\value{
Both functions return the value of the statistic.
}

\arguments{
\item{data}{ A dataframe with all the data, as explained in the \emph{DCluster}
manual page.}
\item{i}{Permutation created in non-parametric bootstrap.}
\item{...}{Additional arguments passed to the functions.}

}


\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

niter<-100

#All Tests are performed around county 78.


#Permutation  model
st.perboot<-boot(sids, statistic=stone.boot, R=niter, region=78)
plot(st.perboot)#Display results

#Multinomial model
st.mboot<-boot(sids, statistic=stone.pboot, sim="parametric",
  ran.gen=multinom.sim,  R=niter, region=78)
plot(st.mboot)#Display results

#Poisson model
st.pboot<-boot(sids, statistic=stone.pboot, sim="parametric",
  ran.gen=poisson.sim,  R=niter, region=78)
plot(st.pboot)#Display results

#Poisson-Gamma model
st.pgboot<-boot(sids, statistic=stone.pboot, sim="parametric",
  ran.gen=negbin.sim, R=niter, region=78)
plot(st.pgboot)#Display results


}

\seealso{
DCluster, boot, stone.stat
}

\references{
Stone, R. A. (1988). Investigating of excess environmental risks around putative sources: Statistical problems and a proposed test. Statistics in Medicine 7,649-660.
}

\keyword{spatial}
