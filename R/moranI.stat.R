moranI.stat<-function(data, applyto="residuals", ...)
{
	if(applyto == "residuals")
	{
		Z<- data$Observed - data$Expected
	}
	else
	{
		Z<- data$Observed/data$Expected 
		Z[!is.finite(Z)]<-0
	}

	return(moran(x=Z,...)$I)
}
