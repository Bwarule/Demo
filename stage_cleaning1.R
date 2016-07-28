## -- understand the type of each variable
get_class <- function(dataIn){
	mydata <- data.frame(variables= names(dataIn),type= sapply(dataIn,class),row.names = NULL)
	return(mydata)
}
# simple sapply(dataIn,class)

## -- calculate the count of missing value
NA_count <- function(x){return(sum(is.na(x)))}

## -- calculate the min, max, summary for each variable
# perpose is just programing 
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
# use table for categorical variable

# prop.test => test event rate over the different interval time for each qauter
# check the stability of event rate belong to conf. interval or not.
# why in some specific interval event rate is very high or low

## write the missing value impute code for each variable
ImputationMissingvalue <- 
function (dataIn, Method = "Mean", variable_List, MissingknoutP = 0.25) 
{
    library(VIM)
    org_Method <- Method
    a <- aggr(dataIn[,variable_List], plot = FALSE)
    Missing_Data <- a$missings
    rm(a);  
	gc();
	
    rownames(Missing_Data) <- NULL
    eachsummary <- data.frame()
    Missing_Data$Percentage <- Missing_Data$Count/dim(dataIn)[1]
	
	if(length(variable_List)==1){
	    Missing_Data$Variable <- variable_List
		Missing_Data <- Missing_Data[which(Missing_Data$Variable == 
        variable_List), ]
	}
    DataSeLvars <- Missing_Data[which(0 < Missing_Data$Percentage & Missing_Data$Percentage < 1), ]
    inputation <- DataSeLvars$Variable
	
	dropsSeLvars <- Missing_Data[which(Missing_Data$Percentage > 
        MissingknoutP), ]
    drops <- dropsSeLvars$Variable
	
    if (length(inputation) == 0 && length(drops) == 0) {
	    warning("There is no missing value in ", variable_List, 
        "\n")
		return(list(dataout = dataIn, Summary_MissingData = Missing_Data))
    }
        ## && length(inputation) != 0
    if (length(drops) != 0 ) {
		## we don't want to drop the variable flag will be rejected!
        ## dataIn <- dataIn[, !(names(dataIn) %in% drops)]
        cat("Variable ", drops,
 			"is rejected because of Max missing percentage exceeds cut-off", MissingknoutP, "\n")
		if(length(inputation) == 0){
			return(list(dataout = dataIn,
						variable_drops_knout = drops,	
						Summary_MissingData = Missing_Data))
		}
    }
        
    if (length(inputation) > 0) {
		variable_List <- inputation
		
		for (i in 1:length(variable_List)) {
			var_imputation <- variable_List[i]
            classObj <- class(dataIn[, var_imputation])
			
            if (classObj == "numeric" ) {
                 Method <- org_Method
				if (Method == "Mean") {
                    Mean_value <- mean(dataIn[, var_imputation], 
                      na.rm = TRUE)
                    dataIn[is.na(dataIn[, var_imputation]), var_imputation] <- Mean_value
                }
                if (Method == "TrimmedMean") {
                    Mean_value <- mean(dataIn[, var_imputation], 
                      na.rm = TRUE, trim = 0.1)
                    dataIn[is.na(dataIn[, var_imputation]), var_imputation] <- Mean_value
                }
				if (Method == "Median") {
                   median_value <- median(dataIn[, var_imputation], 
                     na.rm = TRUE)
                   dataIn[is.na(dataIn[, var_imputation]), var_imputation] <- median_value
                }
                if (Method == "Zero") {
                    dataIn[is.na(dataIn[, var_imputation]), var_imputation] <- 0
                }
            
			}else{
                 Method <- "Mode"
                 if (classObj == "factor" | classObj == "logical" | 
                    classObj == "character" | classObj == "integer") {
                    mode_value <- which.max(table(dataIn[, var_imputation]))
                    dataIn[is.na(dataIn[, var_imputation]), var_imputation] <- names(mode_value)
                  }
                }
                used <- cbind(variableIM = var_imputation, methodIM = Method)
                eachsummary <- rbind(eachsummary, used)
        }
        
        return(list(dataout = dataIn,
 			        Summary_MissingData = Missing_Data,
 					variable_drops_knout = drops,
 					Variable_Method_Imputation = eachsummary))
    }

}
