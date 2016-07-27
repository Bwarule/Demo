## -- understand the type of each variable
get_class <- function(dataIn){
	mydata <- data.frame(variables= names(dataIn),type= sapply(dataIn,class),row.names = NULL)
	return(mydata)
}
# simple sapply(dataIn,class)

## -- calculate the count of missing value
NA_count <- function(x){return(sum(is.na(x)))}

## -- calculate the min, max, summary for each variable

each_summary <- function(dataIn){
	Output <- data.frame()
	for(i in 1:ncol(dataIn)){
		summaryout <- 
		data.frame(cbind(summary(dataIn[,i]),names(dataIn)[i]))
		if(nrow(Output)<1){
			Output <- summaryout
		}else{
			Output <- rbind(Output, summaryout)		
		}
	}
	names(Output) <- c("value","variable")
	return(Output)
}

## -- generate the frequency table for categorical variable
## write the missing value impute code for each variable
