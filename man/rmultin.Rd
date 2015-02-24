\name{rmultin}

\alias{rmultin}

\title{Generate Random Observations from a Multinomial Distribution}


\description{
 This function generates a random observation from a multinomial
distribution.

}

\usage{
rmultin(n, p)
}

\arguments{
\item{n}{Total size (and NOT the number of variables involved in the multinomial distribution).}
\item{p}{Vector of probabilities. The sum of all its elements must be one.}
}


\value{
A vector with the sample which has been generated.
}

\examples{
for(i in 1:10)
	print(rmultin(10, c(1/3, 1/3, 1/3) ))
}


\keyword{distribution}
