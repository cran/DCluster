achisq.stat<-function(data, lambda=NULL)
{
	attach(data)

	df<-length(Observed)


	#If internal standardization was  used then lambda=1 and df=n-1
	if(sum(data$Observed)==sum(data$Expected))
	{
		lambda<-1
		df<-df-1
	}
	else 
	{
		#If lambda is unknown then we must slightly modify E_i
		if(is.null(lambda))
		{
			lambda<-sum(Observed)/sum(Expected)
			df<-df-1
		}
	}
		
	Elambda<-Expected*lambda

	T<-sum((Observed-Elambda)^2/Elambda)
	pvalue<-pchisq(T, df, lower.tail=FALSE)

	detach(data)
	
	return( list(T=T, df=df, pvalue=pvalue) )
}
