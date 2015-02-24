\name{pottwhitt}

\alias{pottwhitt}

\title{Potthoff-Whittinghill's Statistic for Overdispersion}


\description{

This statistic can be used to test for homogeinity among all
the relative risks. The test statistic is:


\deqn{E_+ \sum_{i=1}^n \frac{O_i(O_i-1)}{E_i}}{E_+ * sum_i [O_i(O_i-1)/E_i]}

If we supposse that the data are generated from a multinomial model,
this is the locally U.M.P. when considering the next hypotheses:

\tabular{lcl}{
 \eqn{H_0}{H_0} \tab : \tab \eqn{\theta_1 = \ldots = \theta_n=\lambda}{theta_1 = ... = theta_n)=lambda} \cr
 \eqn{H_1}{H_1} \tab : \tab \eqn{\theta_i \sim Ga(\lambda^2/\sigma^2, \lambda/\sigma^2)}{theta_i ~ Ga(lambda^2/sigma^2, lambda/sigma^2)}
}

Notice that in this case, \eqn{\lambda}{lambda} is supposed to be unknown.
The alternative hypotheses means that relative risks come all from a Gamma
distribution with mean \eqn{\lambda}{lambda} and variance
\eqn{\sigma^2}{sigma^2}.

\emph{pottwhitt.stat} is the function to calculates the value of the statistic
for the data.

\emph{pottwhitt.boot} is used when performing a non-parametric bootstrap.

\emph{pottwhitt.pboot} is used when performing a parametric bootstrap.
}


\seealso{
DCluster, pottwhitt.stat, pottwhitt.boot, pottwhitt.pboot
}


\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
