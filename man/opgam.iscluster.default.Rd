\name{iscluster}

\alias{opgam.iscluster.default}
\alias{opgam.iscluster.negbin}

\title{Local clustering test function}

\description{
This function is passed to function \emph{gam} as argument \emph{iscluster}
to decide whether the current circle must be marked as a cluster or not.

\emph{opgam.iscluster.default} is the function used by default, based on 
quantiles of the Poisson distribution.

\emph{opgam.iscluster.negbin} is similar to the previous one but based
on the Negative Binomial distribution. Local significance
is estimated using bootstrap since it involves the sum of Negative Binomial
variables.
}


\details{
These functions take a number of arguments to be able to assess
whether the current ball is a cluster or not. We have follow this 
approarch to create a common framework for all scan methods.

The vector returned by this functions can be of size higher than four,
but the first four elements must be those stated in this manual
page (and in the same order).

More example can be found in the implementations of other scan methods,
such as Besag and Newell's, and Kulldorff and Nagarwalla's.
}

\usage{
opgam.iscluster.default(data, idx, idxorder, alpha,  ...)
opgam.iscluster.negbin(data, idx, idxorder, alpha, mle, R=999, ...)
}

\arguments{
\item{data}{A dataframe with the data, as explained in \emph{DCluster} manual page.}
\item{idx}{A boolean vector to know the areas in the current circle.}
\item{idxorder}{A permutation of the rows of data to order the regions
according to their distance to the current center.}
\item{alpha}{Test signifiance.}
\item{mle}{Estimators of some parameters needed by the Negative Binomial
distribution. See \emph{negbin.sim} manual page for details.}
\item{R}{Number of simulations made used to estimate local pvalues.}
\item{...}{Any other arguments required.}
}

\value{
A vector with four values:
  \item{statistic}{Value of the statistic computed.}
  \item{result}{A boolean value, which is \emph{TRUE} for clusters.}
  \item{pvalue}{The pvalue obtained for the test performed.}
  \item{size}{Size of the cluster in inumber of regions from the centre.}
}

\seealso{opgam, besagnewell, bn.iscluster, kullnagar, kn.iscluster, turnbull, tb.iscluster}

\keyword{spatial}
