\name{empbaysmooth}

\alias{empbaysmooth}


\title{Empirical Bayes Smoothing}

\description{
Smooth  relative risks from a set of expected and observed number of cases
using a Poisson-Gamma model as proposed by \emph{Clayton and Kaldor} (1987) .

If \eqn{\nu}{nu} and \eqn{\alpha}{alpha} are the two parameters of the 
prior Gamma distribution, smoothed relative risks are
\eqn{\frac{O_i+\nu}{E_i+\alpha}}{(O_i+nu)/(E_i+alpha)}.

\eqn{\nu}{nu} and \eqn{\alpha}{alpha} are estimated via Empirical Bayes,
by using mean and variance, as described by \emph{Clayton and Kaldor}(1987).

Size and probabilities for a Negative Binomial model are also calculated (see
below).

See \emph{Details} for more information.
}

\usage{
empbaysmooth(Observed, Expected, maxiter=20, tol=1e-5)
}

\arguments{
\item{Observed}{Vector of observed cases.}
\item{Expected}{Vector of expected cases.}
\item{maxiter}{Maximum number of iterations allowed.}
\item{tol}{Tolerance used to stop the iterative procedure.}
}


\details{
The Poisson-Gamma model, as described by \emph{Clayton and Kaldor},
is a two-layers Bayesian Hierarchical model:

\deqn{O_i|\theta_i \sim Po(\theta_i E_i)}{O_i|theta_i ~ Po(theta_i E_i)}

\deqn{\theta_i \sim Ga(\nu, \alpha)}{theta_i ~ Ga(nu, alpha)}

The posterior distribution of \eqn{O_i}{O_i},unconditioned to
\eqn{\theta_i}{theta_i}, is Negative Binomial with size \eqn{\nu}{nu} and
probability \eqn{\alpha/(\alpha+E_i)}{alpha/(alpha+E_i)}.

The estimators of relative risks are
\eqn{\widehat{\theta}_i=\frac{O_i+\nu}{E_i+\alpha}}{thetahat_i=(O_i+nu)/(E_i+alpha)}.
Estimators of \eqn{\nu}{nu} and \eqn{\alpha}{alpha}
(\eqn{\widehat{\nu}}{nuhat} and \eqn{\widehat{\alpha}}{alphahat},respectively)
are calculated by means of an iterative procedure using these two equations
(based on mean and variance estimations):

\deqn{\frac{\widehat{\nu}}{\widehat{\alpha}}=\frac{1}{n}\sum_{i=1}^n
\widehat{\theta}_i}{nuhat/alphahat=(1/n)*sum_i(thetahat_i)}

\deqn{\frac{\widehat{\nu}}{\widehat{\alpha}^2}=\frac{1}{n-1}\sum_{i=1}^n(1+\frac{\widehat{\alpha}}{E_i})(\widehat{\theta}_i-\frac{\widehat{\nu}}{\widehat{\alpha}})^2}{nuhat/alphahat^2 = (1/(n-1))*sum_i[(1+alphahat/E_i)*(thetahat_i-nuhat/alphahat)^2]}



}%\details

\value{
A list of four elements:
\item{n}{Number of regions.}
\item{nu}{Estimation of parameter \eqn{\nu}{nu}}
\item{alpha}{Estimation of parameter \eqn{\alpha}{alpha}}
\item{smthrr}{Vector of smoothed relative risks.}
\item{size}{Size parameter of the Negative Binomial. It is equal to 
\deqn{\widehat{\nu}}}{nuhat}.
\item{prob}{It is a vector of probabilities of the Negative Binomial,
calculated as
\deqn{\frac{\widehat{\alpha}}{\widehat{\alpha}+E_i}}{alphahat/(alphahat+E_i} .}
}

\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))

smth<-empbaysmooth(sids$Observed, sids$Expected)
}

\references{
Clayton, David and Kaldor, John (1987). Empirical Bayes Estimates of Age-standardized Relative Risks for Use in Disease Mapping. Biometrics 43, 671-681.
}

\keyword{models}
