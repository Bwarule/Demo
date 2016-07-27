## handle the outliers and group the categorical variables for analysis
## -- perform the outliers analysis on numeric variable
Capping_Flooring_Approach <- 
function(DataIn,VarForcapping=NULL,min_pctle_cut=0.01,max_pctle_cut=0.99,eliminate=FALSE){
	### Note class should be "data.frame" not [1] "tbl_df"     "tbl"        

	##  Code based on percentile cut-offs(Capping/Flooring Approach)	
	if(!(min_pctle_cut > 0  && min_pctle_cut < 1
 		&& max_pctle_cut > 0  && max_pctle_cut < 1 )){
			stop('min_pctle_cut & max_pctle_cut range will be from zero to one!')
		}
		
	if(is.null(VarForcapping)){
		VarForcapping <- names(DataIn)
	}
	
	if(length(VarForcapping)==1){
	   eliminate=FALSE
	}
	quantile_fun <- function(x12)if(is.numeric(x12))quantile(x12,
		 	c(min_pctle_cut, max_pctle_cut), na.rm = TRUE) 
		
	Variable_Cantinous  <- 
	VarForcapping[which(lapply(DataIn[VarForcapping],class)!='factor' &
		  lapply(DataIn[VarForcapping],class)!='character')]
		  
	if(length(Variable_Cantinous)!=1){
    	quantile_value <- sapply(DataIn[,Variable_Cantinous], quantile_fun)
	}else{
		quantile_value <- as.matrix(quantile_fun(DataIn[,Variable_Cantinous]))
		quantile_value <- data.frame(quantile_value)
		names(quantile_value) <- Variable_Cantinous
	}
	
	
	for (i in  1 :length(Variable_Cantinous)){
		DataIn[,Variable_Cantinous[i]] <- 
		pmax(quantile_value[1,Variable_Cantinous[i]],
			pmin(quantile_value[2,Variable_Cantinous[i]], DataIn[,Variable_Cantinous[i]]))
	}
	
    if(eliminate & length(VarForcapping)>1){
		## CODE FOR OUTLIER ELIMINATION CAPPING OPTIONS
		vect <- as.matrix(DataIn_org[,VarForcapping]==DataIn[,VarForcapping])
		gc()
		vect <- 1 - vect*1
		fun_sum <- function(x){ return(sum(x, na.rm = TRUE))}
		vectmatrix <- apply(vect,1,fun_sum)
		para <- max_pctle_cut  -  min_pctle_cut
		quantile_fun <- function(x12)if(is.numeric(x12))quantile(x12,
		 					c(para), na.rm = TRUE) 
						
		outIn <- quantile_fun(vectmatrix)				
		DataIn <- DataIn_org[which(vectmatrix <= outIn),]
	}
	gc()
    return(DataIn)
}

## transform log scale variable (if required)
## -- club the sub categories into one of the corresponding category based event rate
