## stage_dataprocess1.R
## do the univariate Regression  on each variable(p value <0.15)
univariate_regression <- function(mynew_vars,dataIn=mydata,Resp_var="target"){
    require(forward)

	formulas <- paste(Resp_var, sep="~",
	fwd.combn(mynew_vars, 1, fun=function(x){paste(x,collapse="+")}))
	
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
		pvalue <- summary(model)$coefficients[[2,"Pr(>|z|)"]]
		Zvalue <- summary(model)$coefficients[[2,"z value"]]
		StdError <- summary(model)$coefficients[[2,"Std. Error"]]
		Est <- summary(model)$coefficients[[2,"Estimate"]]


		updateffect <- data.frame(usedVar = unlist(allvars),
								Est = Est, StdError = StdError,
								Zvalue = Zvalue, pvalue = pvalue,
								Accuracy = AForModel(model,Resp_var,dataIn) )
		# print(updateffect)						
		if(nrow(Output_updateffect)==0){
			Output_updateffect <- updateffect
		}else{
			Output_updateffect <- rbind(Output_updateffect, updateffect)
		}

	}
	return(Output_updateffect)
}

# call function :univariate_regression(mynew_vars,dataIn,Resp_var="target")

## calculate the IV, gain ratio for each variable

## use of library FSelector for varies measure 
library(FSelector)

weights <- information.gain(target~., train_sample)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "target")
print(f)

weights <- gain.ratio(target~., train_sample)
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "target")
print(f)

weights <- symmetrical.uncertainty(target~., train_sample)
print(weights)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "target")
print(f)

## perform the stepwise Regression

myModel <- glm(eval(parse(text=formulas)),
			data=train_sample,family=binomial(link="logit")) 
summary(step(myModel))			

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
