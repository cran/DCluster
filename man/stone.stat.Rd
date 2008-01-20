\name{stone.stat}

\alias{stone.stat}
\alias{stone.test}

\title{Compute Stone's statistic}

\description{
Calculate Stone's statistic. See \emph{stone} manual page for details.

stone.stat computes the test statistic and the test using a hi-square
distribution whilst stone.test performs a bootstrap test.
}


\usage{
stone.stat(data, region, sorted=FALSE, lambda)
stone.test(formula, data, model, R, ...)
}


\value{
A vector of two elements with the value of the statistic and the region
(counting from the centre) where it was achieved.
}

\arguments{
\item{formula}{Formula that specifies the underlying model. The observed
cases are the response and the expected number of cases must be specified
as an offset in the log scale (see example below).  Note that now it
is not necessary to use Observed and Expected and that any other
names can be used to specify the observed and expected cases.
}
\item{model}{Parametric model to be used in the bootstrap test. One of
"param", "multinom", "poisson" or "negbin". See the \bold{DCluster} manpage for
details.}
\item{...}{The remaining arguments in 'stone.stat' not included in
'stone.test'. This is done so because stone.test calls stone.stat in order to perform the test.}
\item{R}{Number of replicates used in the test to compute the significance of
the observed value of the test statistic.}
\item{data}{A dataframe containing the data, as specified in the
\bold{DCluster} manpage.}
\item{region}{Region where around which we want to test for a cluster. It must
a row number of \emph{data}.}
\item{sorted}{Whether the data are already sorted by distance to \emph{region}.}
\item{lambda}{Value of the null hypotheses. It may NULL (i. e., not known) or a number.}
}


\seealso{
DCluster
}

\examples{
library(spdep)

data(nc.sids)

sids<-data.frame(Observed=nc.sids$SID74)
sids<-cbind(sids, Expected=nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74))
sids<-cbind(sids, x=nc.sids$x, y=nc.sids$y)

#Compute Stone's statistic around Anson county
region<-which(row.names(nc.sids)=="Robeson")
stone.stat(sids, region=region, lambda=1)


stone.test(Observed~offset(log(Expected)), sids, model="poisson", R=99, 
   region=region, lambda=1)
}


\references{
Stone, R. A. (1988). Investigating of excess environmental risks around putative sources: Statistical problems and a proposed test. Statistics in Medicine 7,649-660.
}

\keyword{spatial}
