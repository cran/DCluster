\name{calculate.mle}

\alias{calculate.mle}

\title{Calculate Parameters Involved in Sampling Procedures}

\description{
When boostrap is used to sample values of the statistic under study,
it is possible to use argument \emph{mle} to pass the values of
the parameters involved in the sampling procedure.
}

\usage{
calculate.mle(d, model="poisson")
}

\arguments{
\item{d}{A dataframe as described in the \emph{DCluster} manual page.}
\item{model}{Model used to sample data. It can be either "multinomial",
"poisson" or "negbin".}
}


\value{
A list with the estimates of the parameters involved in the model:

\item{Multimonial}{Total observed cases (\emph{n}) and vector of probabilities
(\emph{p}).}
\item{Poisson}{Total number of regions (\emph{n}) and vector of means 
(\emph{lambda}).}
\item{Negative Binomial (Poisson-Gamma)}{Total number of regions (\emph{n}), 
size and probabilites, calculated after estimating parameters parameters 
\emph{nu} and \emph{alpha} of the Gamma distribution following equations 
proposed by Clayton and Kaldor (1989).}
}

\seealso{
DCluster, observed.sim
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
