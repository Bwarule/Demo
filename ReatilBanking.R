## consider we want to develop the model for account attrition for bank
## creat the big sample file
trns_file <- read.csv("D:\\Training\\bank_rowdata\\Transaction_file.csv")
head(trns_file)

names(trns_file)		

 [1] "Date"            "TransferType"    "Chq..Ref.No."    "Value.Dt"        "Withdrawal.Amt." "Deposit.Amt."    "Closing.Balance"
 [8] "Cust_name"       "City"            "PINCODE"         "Nomination"      "Cust.ID"         "Account.No"      "Currency"       
[15] "A.C.Open.Date"   "Email_id"        "Account.Status"  "Account.Branch"  "JOINT.HOLDERS" 


City_information <- c("Mumbai","Pune","Nagpur","Nashik","Thane","Palghar","Aurangabad", "Navi Mumbai", 
  "Solapur", "Amravati", "Nanded", "Kolhapur", "Sangli", "Jalgaon", "Akola", "Latur", 
  "Malegaon", "Dhule", "Ahmednagar", "Chandrapur", "Parbhani") 
 
Nomination <- c("Registered", "Not Registered")  
final_output <- data.frame()

for(i in 1:1000){
	Cityupdate <- sample(City_information,1)
	Nomination_value <- sample(Nomination,1)
	CustID <- round(runif(1)*100000) 
	AccountNo <- paste("000",round(runif(1)*100000),sep="")

	x3 <- nrow(trns_file)
	x2 <- as.integer(x3*(mean(runif(i))))
	sub <- c(sample(1:x3, x2))
	train_sample <- trns_file[sub,]
	train_sample$City <- Cityupdate
	train_sample$Nomination <- Nomination_value
	train_sample$Cust.ID <- CustID
	train_sample$Account.No <- AccountNo
	
	if(nrow(final_output)==0){
		final_output <- train_sample
	}else{
		final_output <- rbind(final_output, train_sample)
	}
	
}

A.C.OpenDate  
## use dplyr /tidyr packages to summarise the data 
## create the target variable
