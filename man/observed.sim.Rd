\name{observed.sim}

\alias{multinom.sim}
\alias{poisson.sim}
\alias{negbin.sim}

\title{Randomly generate observed cases from different statistical distributions}

\description{
Simulate Observed number of cases according to a Multinomial, Poisson or
Negative Binomial distribution.


These functions are used when performing a parametric bootstrap and
they must be passed as argument \emph{ran.gen} when calling function \emph{boot}.

\emph{multinom.sim} generates observations from a Multinomial distribution.

\emph{poisson.sim} generates observations from a Poisson distribution.

\emph{negbin.sim} generates observations from a Negative Binomial distribution.
}

\usage{
multinom.sim(data, mle=NULL)

poisson.sim(data, mle=NULL)

negbin.sim(data, mle=NULL)
}

\arguments{
\item{data}{A dataframe as described in the \emph{DCluster} manual page.}
\item{mle}{
List containing the parameters of the distributions to be used.  If
they are not provided, then they are calculated from the data. Its value 
argument \emph{mle} in function \emph{boot}. 

The elements in the list depend on the distribution to be used:

\itemize{
\item Multimonial

Total observed cases (\emph{n}) and vector of probabilities (\emph{p}).

\item Poisson

Total number of regions (\emph{n}) and vector of means (\emph{lambda}).

\item Negative Binomial

Total number of regions (\emph{n}) and parameters \emph{nu} and \emph{alpha}
of the Gamma distribution.
}%\itemize
}

}%\arguments


\value{
A dataframe equal to the argument \emph{data}, but in which the Observed
column has been substituted by sampled observations. See \emph{DCluster}
manual page for more details.
}


\seealso{
DCluster
}

\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#Carry out simulations
datasim<-multinom.sim(sids, mle=calculate.mle(sids, model="multinomal") )

#Estimators for Poisson distribution
datasim<-poisson.sim(sids, mle=calculate.mle(sids, model="poisson") )

#Estimators for Negative Binomial distribution
datasim<-negbin.sim(sids, mle=calculate.mle(sids, model="negbin") )

}


\keyword{distribution}
