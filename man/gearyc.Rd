\name{gearyc}

\alias{gearyc}

\title{Moran's I autocorrelation statistic}


\description{
Geary's c statistic is used to measure autocorrelation between areas within
a region, as follows:

\deqn{
c=\frac{(n-1)\sum_i \sum_j W_{ij}(Z_i-Z_j)^2}{2(\sum_i\sum_jW_{ij})\sum_k (Z_k-\overline{Z})^2}
}{
c = (n-1) [sum_i sum_j W_ij (Z_i-Z_j)^2]/[2(sum_i sum_j W_ij) sum_k (Z_k-mean({Z))^2}
}

\eqn{W}{W} is a squared matrix which represents the relationship between each
pair of regions. An usual approach is set \eqn{w_{ij}}{w_ij} to 1 if regions
\eqn{i}{i} and \eqn{j}{j} have a common boundary and 0 otherwise, or it may
represent the inverse distance between the centroids of that two regions.

Small values of this statistic may indicate the presence of highly 
correlated areas, which may be a cluster.
}


\seealso{
DCluster, gearyc.stat, gearyc.boot, gearyc.pboot
}

\references{
Geary, R. C. (1954). The contiguity ratio and statistical mapping. The Incorporated Statistician 5, 115-145.
}

\keyword{spatial}
