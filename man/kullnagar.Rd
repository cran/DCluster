\name{kullnagar}

\alias{kullnagar}

\title{Kulldorff and Nagarwalla's statistic for spatial clustering.}

\description{
This method is based on creating a grid over the study area. Each point of
the grid is taken to be the centre of all circles that contain up to a
fraction of the total population. This is calculated by suming all the
population of the regions whose centroids fall inside the circle. For each one
of these balls, the likelihood ratio of the next test hypotheses is computed:

\tabular{lcl}{
\eqn{H_0}{H_0} \tab : \tab \eqn{p=q}{p=q} \cr
\eqn{H_1}{H_1} \tab : \tab \eqn{p>q}{p>q}
}
where \emph{p} is the probability of being a case inside the ball and
\emph{q} the probability of being a case outside it. Then, the ball
where the maximum of the likelihood ratio is achieved is selected and its
value is tested to assess whether it is significant or not.

There are two possible statistics, depending on the model assumed for the
data, which can be Bernouilli or Poisson. The value of the likelihood ratio 
statistic is

\deqn{\max_{z \in Z}\frac{L(z)}{L_0}}{max_z[L(z)/L_0]}

where \emph{Z} is the set of ball at a given point, \emph{z} an element of
this set, \eqn{L_0}{L_0} is the likelihood under the null hypotheses and
\eqn{L(z)}{L(z)} is the likelihood under the alternative hypotheses. The
actual formulae involved in the calculation can be found in the reference
given below.
}

\seealso{
DCluster, kullnagar.stat, kullnagar.boot, kullnagar.pboot
}

\references{
Kulldorff, Martin and Nagarwalla, Neville (1995). Spatial Disease Clusters: Detection and Inference. Statistics in Medicine 14, 799-810.
}

\keyword{spatial}
