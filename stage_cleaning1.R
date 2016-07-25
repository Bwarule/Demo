## -- understand the type of each variable
get_class <- function(dataIn){
	mydata <- data.frame(variables= names(dataIn),type= sapply(dataIn,class),row.names = NULL)
	return(mydata)
}

## -- calculate the count of missing value
NA_count <- function(x){return(sum(is.na(x)))}

## -- calculate the min, max, summary for each variable

## -- generate the frequency table for categorical variable
## write the missing value impute code for each variable
