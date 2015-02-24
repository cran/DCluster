\name{dcluster}

\alias{plot.dcluster}
\alias{print.dcluster}
\alias{summary.dcluster}


\title{Class for Results from a Test for the Detection of Disease Clusters}


\description{
Class 'dcluster' is used to store the main information when a (boostrap) test
is performed to detect clusters of disease. Essentially, this class has the
same structure and contents as class 'boot' (see in package 'boot') plus
some additional information on the test performed.

Additional functions to plot and summarise the results of the test are 
supplied as well.

}

\usage{
\method{plot}{dcluster}(x, ...)
\method{print}{dcluster}(x, ...)
\method{summary}{dcluster}(object, ...)
}


\arguments{
\item{x}{A 'dcluster' object.}
\item{object}{A 'dcluster' object.}
\item{...}{Any other additional arguments needed (for example, to pass
additional arguments to the plot function).}
}

\value{
These functions do not return anything but produce some plots or print
asummary of the test.
}

\keyword{classes}
