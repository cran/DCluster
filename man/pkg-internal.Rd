\name{DCluster-internal}

\alias{dotest}

\title{Internal functions in the DCluster package.}

\description{
These functions are used by the package internally and they are
not intended to be used by the final user. You can check the
source code if you need more details or contact the maintainer.
}

\usage{
dotest(formula, stat, data, model, R, ..., alternative=NULL)
}

\arguments{
\item{formual}{Formula that relates the observed cases (response)
to the expected cases (as an offset in the logscale, i.e., 
offset(log(expected))}
\item{stat}{Name of the statistic to be computed.}
\item{data}{Data frame with the data.}
\item{model}{Model to be used when resampling in the test.}
\item{R}{Number of simulations used in the test.}
\item{...}{Any other argument needed by the test statistic. Check its
particular manual page for the list of extra arguments and examples.}
\item{alternative}{Alternative hipotesis. This will depend on the test
statistic being used.}
}

\keyword{internal}

