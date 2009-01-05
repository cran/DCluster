\name{DCluster}

\alias{DCluster}

\title{A package for the detection of  spatial clusters of diseases
for count data}


\description{
DCluster is a collection of several methods related to the detection
of spatial clusters of diseases. Many widely used methods, such as Openshaw's
GAM, Besag and Newell, Kulldorff and Nagarwalla, and others
have been implemented.

Besides the calculation of these statistic, bootstrap can be used
to test its departure from the null hypotheses, which will be
no clustering in the study area. For possible sampling methods can be used to perform the
simulations: permutation, Multinomial, Poisson and Poisson-Gamma.

Minor modifications have been made to the methods to use standardized expected
number of cases instead of population, since it provides a better approach to
the expected number of cases.
}%description


\section{Introduction}{

We'll always suppose that we are working on a study region which is divided
into \emph{n} non-overlaping smaller areas where data are measured. Data
measured are usually people suffering from a disease or even deaths. This will
be refered as \emph{Observed number of cases}. For a given area, its observed
number of cases will be denoted by \eqn{O_i}{O_i} and the sum of these
quantities over the whole study region will be \eqn{O_+}{O_+}.

In the same way can be defined \emph{Population} and \emph{Standardized
Expected number of cases}, which will be denoted by \eqn{P_i}{P_i} and
\eqn{E_i}{E_i}, respectively. The sum of all these quantities 
are represented by \eqn{P_+}{P_+} and \eqn{E_+}{E_+}.

The basic assumption for the data is that they are independant
observations from a Poisson distribution, whose mean is 
\eqn{\theta_iE_i}{\theta_i E_i}, where \eqn{\theta_i}{theta_i}
is the relative risk. That is,

\deqn{O_i \sim Po(\theta_i E_i); \ i=1, \ldots , n}{O_i ~ Po(theta_i E_i); i=1, ..., n}

}%section{Introduction}


\section{Null hypotheses}{

Null hypotheses is usually equal relative risks, that is

\deqn{H_0: \theta_1= \ldots = \theta_n = \lambda}{H_0: theta_1= ... = theta_n = lambda}

\eqn{\lambda}{lambda} may be considered to be known (one, which means standard
risk) or unknown. In the last case, \eqn{E_i}{E_i} must slightly be corrected
by multiplying it by the overall relative risk \eqn{\frac{O_+}{E_+}}{O_+/E_+}.

}%section{Null hypotheses}


\section{Code structure}{
Function names follow a common format, which is a follows:

\itemize{
\item{\emph{method name}.stat}{Calculate the statistic itself.}
\item{\emph{method name}.boot}{Perform a non-parametric bootstrap.}
\item{\emph{method name}.pboot}{Perform a parametric bootstrap.}
}

Openshaw's G.A.M. has generally been implemented in a function called
\emph{gam}, which some methods ( Kulldorff & Nagarwalla, Besag & Newell) also
use, since they are based on a window scan of the whole region. At every point
of the grid, a function is called to determine whether that point is a cluster
or not. The name of this function is \emph{shorten method name.iscluster}.

This function calculates the local value of the statistic involved and
its signifiance by means of bootstrap. The interface provided, through
function \emph{gam}, is quite straightforward to use and it can handle the 
three methods mentioned and other supplied by the users.

}


\section{Bootstrap procedures}{

Four possible bootstrap models have been provided in order to estimate
sampling distributions of the statistics provided. The first one is a
non-parametric bootstrap, which performs permutations over the observed number
of cases, while the three others are parametric bootstrap based on
Multinomial, Poisson and Poisson-Gamma distributions.

Permutation method just takes observed number of cases and permute them among
all regions, to know whether risk in uniform across the whole study area.
It just should be used with care since we'll face the problem of having
more observed cases than population in very small populated areas.

Multinomial sampling is based on conditioning the Poisson framework
to \eqn{O_+}{O_+}. THis way \eqn{(O_1, \ldots, O_n)}{(O_1, ..., O_n}
follows a multinomial distribution of size \eqn{O_+}{O_+} and 
probabilities \eqn{(\frac{E_1}{E_+}, \ldots, \frac{E_n}{E_+})}{(E_1/E_+, ..., E_n/E_+)}.

Poisson sampling just generates observed number of cases from a Poisson
distribution whose mean is \eqn{E_i}{E_i}.

Poisson-Gamma sampling is based on the Poisson-Gamma model proposed
by \emph{Clayton and Kaldor} (1984):

\deqn{O_i|\theta_i \sim Po(\theta_i E_i)}{O_i | theta_i ~ Po(theta_i E_i)}

\deqn{\theta_i \sim Ga(\nu, \alpha)}{theta_i ~ Ga(nu, alpha)}


The distribution of \eqn{O_i}{O_i} unconditioned to \eqn{\theta_i}{theta_i} is
Negative Binomial with size \eqn{\nu}{nu} and probability
\eqn{\frac{\alpha}{\alpha+E_i}}{alpha/(alpha+E_i)}. The two parameters can be
estimated using an Empirical Bayes approach from the Expected and Observed
number of cases. Function \emph{empbaysmooth} is provided for this purpose.


}%section{Bootstrap procedures}


\section{Data}{

One of the parameters, which is usually called \emph{data}, passed to many of
the functions in this package is a dataframe which contains the data for each
of the regions used in the analysis. Besides, its columns must be labeled:


\itemize{

\item{\bold{Observed}}{Observed number of cases.}

\item{\bold{Expected}}{Standardised expected number of cases.}

\item{\bold{Population}}{Population at risk.}

\item{\bold{x}}{Easting coordinate of the region centroid.}

\item{\bold{y}}{Northing coordinate of the region centroid.}

}

}%section{Data}



%\seealso{
%}

\references{
Clayton, David and Kaldor, John (1987). Empirical Bayes Estimates of Age-standardized Relative Risks for Use in Disease Mapping. Biometrics 43, 671-681.

Lawson et al (eds.) (1999). Disease Mapping and Risk Assessment for Public
Health. John Wiley and Sons, Inc.

Lawson, A. B. (2001). Statistical Methods in Spatial Epidemiology. John Wiley and Sons, Inc.
}



\keyword{package}
