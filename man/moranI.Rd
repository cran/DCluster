\name{moranI}

\alias{moranI}

\title{Moran's I autocorrelation statistic}


\description{
Moran's I statistic measures autocorrelation between areas within
a region. It is similar to the correlation coefficient:

\deqn{
I=\frac{n\sum_i\sum_j W_{ij}(Z_i-\overline{Z})(Z_j-\overline{Z})}{2(\sum_i\sum_jW_{ij})\sum_k (Z_k-\overline{Z})^2}
}{
I= n * [sum_i ( sum_j W_ij(Z_i-mean(Z))*(Z_j-mean(Z))]/[2 * (sum_i sum_j W_ij) * sum_k (Z_k-mean(Z))^2]
}

\eqn{W}{W} is a squared matrix which represents the relationship between each
pair of regions. An usual approach is set \eqn{w_{ij}}{w_ij} to 1 if regions
\eqn{i}{i} and \eqn{j}{j} have a common boundary and 0 otherwise, or it may
represent the inverse distance between the centroids of these two regions.

High values of this statistic may indicate the presence of groups of zones
where values are unusually high. On the other hand, low values
of the Moran's statistic will indicate no correlation between neighbouring
areas, which may lead to indipendance in the observations. 


\emph{moranI.stat} is the function to calculate the value of the statistic for
residuals or SMRs of the data.

\emph{moranI.boot} is used when performing a non-parametric bootstrap.

\emph{moranI.pboot} is used when performing a parametric bootstrap.
}


\seealso{
DCluster, moranI.stat, moranI.boot, moranI.pboot
}

\references{
Moran, P. A. P. (1948). The interpretation os statistical maps. Journal of the Royal Statistical Society, Series B 10, 243-251.
}

\keyword{spatial}
