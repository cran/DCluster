\name{stone}

\alias{stone}

\title{Stone's Test}

\description{

Stone's Test is used to assess risk around given locations (i. e., a putative
pollution source). The null hypotheses is that relative risks are constant
across areas, while the alternative is that there is descending trend in
relative risks as distance to the focus increases. That is

\tabular{lcl}{
\eqn{H_0}{H_0} \tab : \tab \eqn{\theta_1 = \ldots = \theta_n = \lambda}{theta_1 = ... = theta_n = lambda} \cr
\eqn{H_1}{H_1} \tab : \tab \eqn{\theta_1 \geq \ldots \geq \theta_n}{theta_1 >= ... >= theta_n}

}
Supposing data sorted by distance to the putative pollution source, Stone's
statistic is as follows:

\deqn{\max_{j}(\frac{\sum _{i=1}^j O_i}{\sum _{i=1}^j E_i)}}{max_j [(sum_i=1 ^j O_i) / (sum_i=1 ^j E_i)]}

Depending on whether \eqn{\lambda}{lambda} is known (usually 1) or not,
\eqn{E_i}{E_i} may need a minor correction, which are not done automatically.
See \emph{achisq} manual page for details.
}


\seealso{
DCluster, stone.stat, stone.boot, stone.pboot
}

\references{
Stone, R. A. (1988). Investigating of excess environmental risks around putative sources: Statistical problems and a proposed test. Statistics in Medicine 7,649-660.
}

\keyword{spatial}
