## stage_dataprocess1.R
## do the univariate Regression  on each variable
## calculate the IV, gain ratio for each variable
## perform the stepwise Regression

## effect calculation of variable 
## suppose you finilise list of variable which are significant 
## and then you need to drop some variable to incarse the accuracy from old model 
effect_calculation <- function(mynew_vars,dataIn=mydata,Resp_var="target"){
    require(forward)

	formulas <- paste(Resp_var, sep="~",
	fwd.combn(mynew_vars, length(mynew_vars)-1, fun=function(x){paste(x,collapse="+")}))
	
	AForModel <- function(model,Resp_var,dataIn){
		A_train <- sum(diag(table(dataIn[,Resp_var],round(predict(model, type='response', dataIn)))))/nrow(dataIn)
		return(A_train)
	}
	Output_updateffect <- data.frame()
	
	for(j in 1:length(formulas)){
		model_varformula <- as.character(formulas[j])
		model <- glm(eval(parse(text=model_varformula)),family=binomial(link = "logit"),data =dataIn)
		# summary(model)
		AForModel(model,Resp_var,dataIn) ## 70 and 73
		allvars <- strsplit(unlist(strsplit(model_varformula,"~"))[[2]],"\\+")
		dropVars <- mynew_vars[!(mynew_vars %in% unlist(allvars))]
		updateffect <- data.frame(dropVars= dropVars , Accuracy =AForModel(model,Resp_var,dataIn) )
		# print(updateffect)
		if(nrow(Output_updateffect)==0){
			Output_updateffect <- updateffect
		}else{
			Output_updateffect <- rbind(Output_updateffect, updateffect)
		}

	}
	return(Output_updateffect)

}

effect_calculation(mynew_vars,dataIn,Resp_var="target")
## calculate VIF for model
