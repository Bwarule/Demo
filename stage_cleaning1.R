## write the missing value impute code for each variable
## -- understand the type of each variable,
## -- calculate the count of missing value
NA_count <- function(x){return(sum(is.na(x)))}

## -- calculate the min, max, summary for each variable
## -- generate the frequency table for categorical variable
