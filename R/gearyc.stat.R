gearyc.stat<-function(data, applyto="residuals", ...)
{
	n<-length(data$Observed)

	if(applyto == "residuals")
	{
		Z<- data$Observed - data$Expected
	}
	else
	{
		Z<- data$Observed/data$Expected 
		Z[!is.finite(Z)]<-0
	}

	return(spdep::geary(x=Z, ...)$C)
}
