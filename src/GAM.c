#include<stdio.h>

#include"DCluster.h"


SEXP Ropgam_iscluster_negbin(SEXP Observed, SEXP Expected, SEXP size, SEXP prob, SEXP nsim)
{
	double pvalue;
	SEXP results;

	pvalue=opgam_iscluster_negbin( (double *)REAL(Observed), (double *)REAL(Expected), GET_LENGTH(Observed), ((double *)REAL(size))[0], (double *)REAL(prob), ((double *)REAL(nsim))[0]);

	PROTECT(results=NEW_NUMERIC(1));

	( (double *)REAL(results))[0]=pvalue;

	UNPROTECT(1);

	return(results);
}

double opgam_iscluster_negbin(double *Observed, double *Expected, int n, double size, double *prob, double nsim)
{
	int count, i;
	double pvalue=0, O=0, sumobs;

	for(i=0;i<n;i++){O+=Observed[i];}

	GetRNGstate();
	
	for(count=0;count<nsim;count++)
	{
		sumobs=0;

		for(i=0;i<n;i++)
			sumobs+=rnbinom(size,prob[i]);

		if(sumobs>O){pvalue++;}
	}

	PutRNGstate();

	pvalue=pvalue/(nsim+1);
	return(pvalue);
}
