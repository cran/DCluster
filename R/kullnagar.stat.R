kn.iscluster<-function(data, idx, idxorder, alpha, fractpop, model="poisson", R, mle)
{
        #Indexes of regions in the balls ordered by distance to the centre
        localidx<-idxorder[idx[idxorder]]


	knboot<-switch(model,
	permutation = boot(data[idxorder,], statistic=kullnagar.boot, R=R, fractpop=fractpop),
	multinomial=boot(data[localidx,], statistic=kullnagar.pboot, sim="parametric", ran.gen=multinom.sim,  R=R, fractpop=fractpop, mle=list(n=mle$n, p=mle$p[localidx]) ),
	poisson = boot(data[localidx,], statistic=kullnagar.pboot, sim="parametric", ran.gen=poisson.sim,  R=R, fractpop=fractpop, mle=list(n=mle$n, lambda=mle$lambda[localidx]) ),
	negbin = boot(data[localidx,], statistic=kullnagar.pboot, sim="parametric", ran.gen=negbin.sim,  R=R, fractpop=fractpop, mle=list(n=mle$n, size=mle$size,prob=mle$prob[localidx]) )
	)

	if(is.null(knboot$t0))
		return(c(NA, NA, NA, NA))

	pvalue<-sum(knboot$t[,1]>knboot$t0[1] )/(R+1)
	return(c( knboot$t0[1], alpha>pvalue, pvalue, knboot$t0[2]) )
}


#Compute the statistic around the first location(row) in the dataframe
#Data must be order according to distance to the center of the circle
kullnagar.stat<-function(data, fractpop, log.v=FALSE)
{
	csumpop<-cumsum(data$Population)
	P<-csumpop[length(csumpop)]

	p<-length( which( (csumpop<=fractpop*P) == TRUE ) )

	L<-0
	O<-sum(data$Observed)
	csumobs<-cumsum(data$Observed[1:p])
	csumexp<-cumsum(data$Expected[1:p])


	#log(L_0)
	l0<-O*log(O)+(P-O)*log(P-O)-P*log(P)

	#Size of the cluster, number of regions from the centre
	size<-1
	for(i in 1:p)
	{
		if( (csumobs[i]*(P-csumpop[i])) > (csumpop[i]*(O-csumobs[i])) )
		{
			#Likelihood in ball Z
			l<-csumobs[i]*log(csumobs[i])
			l<-l+ (csumpop[i]-csumobs[i])*log(csumpop[i]-csumobs[i]) 
			l<-l- csumpop[i]*log(csumpop[i])
				
			
			#Likelihood outside ball Z
			l<-l+(O-csumobs[i])*log(O-csumobs[i])
			l<-l+(P-csumpop[i]-O+csumobs[i])*log(P-csumpop[i]-O+csumobs[i])
			l<-l-(P-csumpop[i])*log(P-csumpop[i])	

			#L_0
			l<-l-l0

		}
		else
		{
			l<-0
		}

		if(l>L)
		{
			L<-l
			size<-i
		}
	}

	if(!log.v) L<-exp(L)

	return(c(value=L, size=size))
}
