\name{Tests for Overdispertion}

\alias{test.nb.pois}
\alias{DeanB}
\alias{DeanB2}

\title{Likelihood ratio test and Dean's tests for Overdispertion}

\description{
When working with count data,  the assumption of a Poisson model is 
common. However, sometimes the variance of the data is significantly 
higher that their mean which means that the assumption of that data have
been drawn from a Poisson distribution is wrong. 

In that case is is usually said that data are overdispersed and a better
model must be proposed. A good choice is a Negative Binomial distribution
(see, for example, \code{\link[MASS]{negative.binomial}}.

Tests for overdispersion available in this package are the Likelihood Ratio
Test (LRT) and Dean's \eqn{P_B} and  \eqn{P'_B} tests.  
}


\details{
The LRT is computed to compare a fitted Poisson model against a fitted
Negative Binomial model.

Dean's \eqn{P_B} and  \eqn{P'_B} tests are score tests. These
two tests were proposed for the case in which we look for overdispersion
of the form 
\eqn{var(Y_i)=\mu_i(1+\tau \mu_i)}, where 
\eqn{E(Y_i)=\mu_i}.
See Dean (1992) for more details.

}


\usage{
test.nb.pois(x.nb, x.glm)
DeanB(x.glm, alternative="greater")
DeanB2(x.glm, alternative="greater")
}

\arguments{
\item{x.nb}{Fitted Negative Binomial.}
\item{x.glm}{Fitted Poisson model.}
\item{alternative}{Alternative hipothesis to be tested. It can be
"less", "greater" or "two.sided", although the usual choice will
often be "greater".}
}


\value{
An object of type \emph{htest} with the results (p-value, etc.).
}


\examples{
library(spdep)
library(MASS)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

x.glm<-glm(Observed~1+offset(log(sids$Expected)), data=sids, family=poisson())
x.nb<-glm.nb(Observed~1+offset(log(Expected)), data=sids)

print(test.nb.pois(x.nb, x.glm))
print(DeanB(x.glm))
print(DeanB2(x.glm))

}

\seealso{
DCluster, achisq.stat, pottwhit.stat, negative.binomial (MASS), glm.nb (MASS)
}

\references{
Dean, C.B. (1992), Testing for overdispersion in Poisson and binomial regression models, \emph{J. Amer. Statist. Assoc.} 87, 451-457.
} 

\keyword{htest}
