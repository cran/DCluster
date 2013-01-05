#include<Rversion.h>
#include<Rinternals.h>
#include<Rdefines.h>
#include<Rmath.h>

#include"R_ext/Applic.h"
#include"R_ext/RS.h"
#include<R_ext/Random.h>

void mycumsum(double *x, int *n, double *na, double *csumx);

SEXP Rkn_poisson(SEXP Observed, SEXP Expected, SEXP fractpop);

void kn_poisson(double *Observed, double * Expected, int n, double fractpop, double *logvalue, int *size);
	
SEXP Ropgam_iscluster_negbin(SEXP Observed, SEXP Expected, SEXP size, SEXP prob, SEXP nsim);

double opgam_iscluster_negbin(double *Observed, double *Expected, int n, double size, double *prob, double nsim);

