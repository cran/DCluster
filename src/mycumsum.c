#include<stdio.h>

#include"DCluster.h"

void mycumsum(double *x, int *n, double *na, double *csumx)
{
	int i;

	if(x[0]!=*na)
		csumx[0]=x[0];
	else
		csumx[0]=0;


	for(i=1;i<*n;i++)
	{
		if(x[i]!=*na)
			csumx[i]=csumx[i-1]+x[i];
		else
			csumx[i]=csumx[i-1]+0;
	}
}
