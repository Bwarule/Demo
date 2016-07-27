library("xlsx")
library("mice")
library("dplyr")
creditcarddata <- read.xlsx("creditcard.xlsx",sheetName = "creditcard")

varibletype <- function(data)
{
  datatypeofdata <-sapply(data,class)
  print("printing the coulmn names with their type ")
  return(datatypeofdata)
  
}
varibletype(creditcarddata)
Noofcustomer <- length(unique(creditcarddata$ID)) #It will give how many unique customers present in the data
table(creditcarddata$SEX) #Tabling factor data
# Female   male 
# 1356    979 
md.pattern(creditcarddata)
#
  #  ID LIMIT_BAL EDUCATION MARRIAGE PAY_0 PAY_2 PAY_3 PAY_4
#2289  1         1         1        1     1     1     1     1
#   4  1         1         0       1     1     1     1     1
 #  7  1         1         1        1     1     1     1     1
 #  8  1         1         1        1     1     1     1     1
 #  1  1         1         1        1     1     1     1     1
 #  5  1         1         1        1     1     1     1     1
  # 3  1         1         1        1     1     1     1     1
 
sum(is.na(creditcarddata))#It will give total how much missing value present
pMiss <- function(x){sum(is.na(x))/length(x)*100} #It will calculte how many percentage of data missing for each variable.If it is more than 5 % ignore it.
apply(creditcarddata,2,pMiss)

#Creating new data set using mice() and complete()
tempData <- mice(creditcarddata,m=5,maxit=50,meth='pmm',seed=500)
completeData <- complete(tempData,1)
sum(is.na(completeData))

#calculating summary of data
summary(completeData)

#using dplyr package summarise function
summarise(group_by(completeData,SEX), mean(BILL_AMT1, na.rm = TRUE))

summarise(group_by(completeData,EDUCATION), mean(BILL_AMT1, na.rm = TRUE))


#Outlier Detection using IQR code .This function will return how many outliers point prsent in the feature
noofoutlier <- function(feature)
{
   tempsum <- summary(feature)
   firstquartiledata = tempsum [2][1]
   thirdqurtiledata=tempsum[5][1]
   IQR=thirdqurtiledata-firstquartiledata
   slag=1.5*IQR
   range1=firstquartiledata-slag
   range2=thirdqurtiledata+slag
   lenght=length(feature)
   count=0
   for (i in 1:lenght) {if (feature[i] < range1)
    count=count+1
   else if (feature[i] > range2)
    count=count+1  }
    
 
 return (count)
}

