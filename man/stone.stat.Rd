\name{stone.stat}

\alias{stone.stat}

\title{Compute Stone's statistic}

\description{
Calculate Stone's statistic. See \emph{stone} manual page for details.
}


\usage{
stone.stat(data, region, sorted=FALSE, lambda)
}


\value{
A vector of two elements with the value of the statistic and the region
(counting from the centre) where it was achieved.
}

\arguments{
\item{data}{A dataframe with all the data, as explained in the \emph{DCluster}
manual page.}
\item{region}{Region where around which we want to test for a cluster. It must
a row number of \emph{data}.}
\item{sorted}{Whether the data are already sorted by distance to \emph{region}.}
\item{lambda}{Value of the null hypotheses. It may NULL (i. e., not known) or a number.}
}


\seealso{
DCluster
}

\example{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#Compute Stone's statistic around the 78th county
stone.stat(sids, region=78, lambda=1)
}


\references{
Stone, R. A. (1988). Investigating of excess environmental risks around putative sources: Statistical problems and a proposed test. Statistics in Medicine 7,649-660.
}

\keyword{spatial}
