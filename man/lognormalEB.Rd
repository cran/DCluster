\name{lognormalEB}

\alias{lognormalEB}


\title{Empirical Bayes Smoothing using a log-normal model}

\description{
Smooth  relative risks from a set of expected and observed number of cases
using a log-Normal model as proposed by \emph{Clayton and Kaldor} (1987).
There are estimated by 
\eqn{\tilde{\beta}_i =\log((O_i+1/2)/E_i)}{betatilde_i =log((O_i+1/2)/E_i)}
in order to prevent taking the logarithm of zero.

If this case, the log-relative risks are assumed be independant and to have a
normal distribution with mean \eqn{\varphi}{\varphi} and variance
\eqn{\sigma^2}{sigma2}.  Clayton y Kaldor (1987) use the EM algorithm to
develop estimates of these two parameters which are used to compute the
Empirical Bayes estimate of \eqn{b_i}{b_i}. The formula is not listed here, but
it can be consulted in Clayton and Kaldor (1987).

}

\usage{
lognormalEB(Observed, Expected, maxiter = 20, tol = 1e-05)
}

\arguments{
\item{Observed}{Vector of observed cases.}
\item{Expected}{Vector of expected cases.}
\item{maxiter}{Maximum number of iterations allowed.}
\item{tol}{Tolerance used to stop the iterative procedure.}
}


\value{
A list of four elements:
\item{n}{Number of regions.}
\item{phi}{Estimate of \eqn{\varphi}{phi}.}
\item{sigma2}{Estimate of \eqn{\sigma^2}{sigma2}.}
\item{smthrr}{Vector of smoothed relative risks.}
}

\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))

smth<-lognormalEB(sids$Observed, sids$Expected)
}

\references{
Clayton, David and Kaldor, John (1987). Empirical Bayes Estimates of Age-standardized Relative Risks for Use in Disease Mapping. Biometrics 43, 671-681.
}

\keyword{models}
