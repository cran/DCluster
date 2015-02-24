\name{achisq}

\alias{achisq}

\title{Another Implementation of Pearson's Chi-square Statistic}


\description{
Another implementation of Pearson's Chi-square has been written
to fit the needs in package \emph{DCLuster}.

\emph{achisq.stat} is the function that calculates the value of the statistic
for the data.

\emph{achisq.boot} is used when performing a non-parametric bootstrap.

\emph{achisq.pboot} is used when performing a parametric bootstrap.
}

\details{
This statistic can be used to detect whether observed data
depart (over or above) expected number of cases significantly.
The test considered stands for relative risks among areas
to be equal to an (unknown) constant \eqn{\lambda}{\lambda}, while
the alternative hypotheses is that not all relative risks are equal.

The actual value of the statistic depends on null hypotheses.
If we consider that all the relative risks are equal to 1, the 
value is

T=\deqn{\sum_i\frac{(O_i-E_i)^2}{E_i}}{sum_i ( (O_i-E_i)^2/E_i )}

and the degrees of freedom are equal to the number of regions.


On the other hand, if we just consider relative risks to be equal, without
specifying their value (i.e., \eqn{\lambda}{lambda} is unknown),
\eqn{E_i}{E_i} must be substituted by \eqn{E_i\frac{O_+}{E_+}}{E_i*(O_+/E_+)}
and the number of degrees of freedom is the number of regions minus one.

When internal standardization is used, null hypotheses must 
be all relative risks equal to 1 and the number of degrees
of freedom is the number of regions minus one. This is due to the
fact that, in this case, \eqn{O_+=E_+}{O_+=E_+}.
}

\seealso{
DCluster, achiq.stat, achisq.boot, achisq.pboot
}

\references{
Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: I. The Binomial and Multinomial Distributions. Biometrika 53, 167-182.

Potthoff, R. F. and Whittinghill, M.(1966). Testing for Homogeneity: The Poisson Distribution. Biometrika 53, 183-190.
}

\keyword{htest}
