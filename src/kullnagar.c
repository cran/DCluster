#include<stdio.h>

#include"DCluster.h"


SEXP Rkn_poisson(SEXP Observed, SEXP Expected, SEXP fractpop)
{
	double logvalue;
	int size;
	SEXP results;

	kn_poisson( (double *)REAL(Observed), (double *)REAL(Expected), GET_LENGTH(Observed), ((double *)REAL(fractpop))[0], &logvalue, &size);

	PROTECT(results=NEW_NUMERIC(2));

	( (double *)REAL(results))[0]=logvalue;
	( (double *)REAL(results))[1]=size;

	UNPROTECT(1);

	return(results);
}

void kn_poisson(double *Observed, double * Expected, int n, double fractpop, double *logvalue, int *size)
{
	double O, E;
	int maxregion, count;
	double *csumexp, *csumobs, aux, na=-1;
	double  l0, l, difobs, difexp;
	
	/*Cumulative sums*/
	csumexp=(double *)R_alloc(n, sizeof(double));
	R_cumsum(Expected, &n, &na, csumexp);
	E=csumexp[n-1];

	csumobs=(double *)R_alloc(n, sizeof(double));
	R_cumsum(Observed, &n, &na, csumobs);
	O=csumobs[n-1];


	/*Maximum number of regions to which the 'cluster' is extended*/
	aux=fractpop*E;
	maxregion=0;
	while(aux>csumexp[maxregion]){maxregion++;}
	
	/*When all regions are considered the likelihood ratio is 1*/
	if(maxregion>=(n-1))
		maxregion=n-2;

	*logvalue=0;
	l0=O*(log(O)-log(E));

	*size=1;

	for(count=0;count<=maxregion;count++)
	{
		difobs=O-csumobs[count];
		difexp=E-csumexp[count];

		if( (csumobs[count]*difexp) > (csumexp[count]*difobs) )
                {
			/*Likelihood inside ball Z*/
                        l=csumobs[count]*log(csumobs[count]/csumexp[count]);

			/*Likelihood outside ball Z*/
			l=l+difobs*log(difobs/difexp);

			/*Divide by L_0*/
			l=l-l0;
		}
		else
		{
			l=0;
		}

		if(l>*logvalue)
		{
			*logvalue=l;
			*size=count+1;
		}

	}
}
