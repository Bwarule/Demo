## stage_dataprocess2.R
## summarise the model 
## calculate the ROC curve
## KS, misclassification table
http://127.0.0.1:10343/library/ROCR/html/performance.html
library(ROCR)
# read more details for above statistics R package ROCR

## rank order 
## HL test 
library("ResourceSelection")
hltest <- hoslem.test(model3$default.payment.next.month, fitted(model))
  
hoslem_test <-   function (x, y, g = 10) 
{
    DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), 
        sep = ", ")
    METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
    yhat <- y
    y <- x
    qq <- unique(quantile(yhat, probs = seq(0, 1, 1/g)))
    cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
    observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
    expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ 
        cutyhat)
    chisq <- sum((observed - expected)^2/expected)
    PVAL = 1 - pchisq(chisq, g - 2)
    PARAMETER <- g - 2
    names(chisq) <- "X-squared"
    names(PARAMETER) <- "df"
    structure(list(statistic = chisq, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed, 
        expected = expected), class = "htest")
}
## check variable stability  
