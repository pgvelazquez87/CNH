##MODEL DIAGNOSTICS

library(boot)
library(ggplot2)
library(coefplot)
library(reshape2)

housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")
housing <- housing[housing$Units<1000, ]

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
houseg1<- glm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing, family=gaussian(link = "identity"))
houseg2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
houseg3 <- glm(ValuePerSqFt ~ Units + SqFt*Boro + Class, data=housing)
houseg4 <-  glm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class, data=housing)
houseg5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing)


for (i in 1:5)  {
  nam <- paste0("houseg", i)
  print(nam)
  assign(paste0("housecv", i), cv.glm(housing, get(nam), K=5))
  print(get(paste0("housecv", i))$delta)
}

cvresults <- as.data.frame(rbind(housecv1$delta, housecv2$delta, housecv3$delta, housecv4$delta, housecv5$delta))
names(cvresults) <- c("Error", "Adjusted.Error")
cvresults$model <- sprintf("houseg%s", 1:5)

#Comparar los distintos modelos a través de distintos mecanismos
cvanova <- anova(houseg1, houseg2, houseg3, houseg4, houseg5)
cvresults$anova <- cvanova$`Resid. Dev`
cvaic <- AIC(houseg1, houseg2, houseg3, houseg4, houseg5)
cvresults$aic <- cvaic$AIC

cvmelt <- melt(cvresults, id.vars="model", variable.name="measure", value.name=)
ggplot(aes(x=model, y=value), data=cvmelt) + geom_line(aes(group=measure, color=measure)) + facet_wrap(~measure, scales="free_y") + theme(axis.text.x=element_text(angle=90, vjust=0.5)) + guides(color=FALSE)



################################################################################
#### modelo general para cross-validation

cv.work <- function(fun, k=5, data, cost = function(y, yhat) mean ((y-yhat)^2), response = "y", ...)
{
  ##generate folds
  folds <- data.frame(fold=sample(rep(x=1:5, length.out=nrow(housing))), row=1:nrow(housing))
  
  ##start the error with 0
  error <- 0
  
  ##loop through each of the folds
  ## for each fold:
  ## fit the model on the training data
  ## predict on the test data
  ## compute the error and accumulate it
  for(f in 1:max(folds$fold))
  {
    # rows that are in test set
    therows <- folds$row[folds$fold == f]
    
    # call fun on data[-therows, ]
    # predict on data[therows, ]
    mod <- fun(data=data[-therows, ], ... )
    pred <- predict(mod, data[therows, ])
    
    # add new error weighted by the number of rows in this fold
    error <- error + cost(data[therows, response], pred) * (length(therows)/nrow(data))
  }
  
  return(error)
}

##Correr los modelos con la función
cv1 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt", formula=ValuePerSqFt ~ Units + SqFt + Boro)
cv2 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt", formula=ValuePerSqFt ~ Units * SqFt + Boro)
cv3 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt", formula=ValuePerSqFt ~ Units + SqFt*Boro + Class)
cv4 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt", formula=ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class)
cv5 <- cv.work(fun=lm, k=5, data=housing, response="ValuePerSqFt", formula=ValuePerSqFt ~ Boro + Class)

cvResults <- data.frame(model=sprintf("house%s", 1:5), error=c(cv1, cv2, cv3, cv4, cv5))





#############################
## BOOTSTRAPPING
#############################

library(plyr)
library(boot)
library(ggplot2)

data(baseball)
baseball <- baseball[baseball$year>=1990, ]
head(baseball)

#write.csv(baseball, file="baseball.csv")
#baseball$battingavg[is.infinite(baseball$battingavg)] <- NaN
#summary(baseball, na.rm=TRUE)

##build a function for calculating batting average
#data is the data
#boot will pass varying sets of indices
#some rows will be represented multiple times in a single pass
#other rows will not be represented at all
#on average, about 63% of the rows will be present
#this function is called repeatedly by boot


bat.avg <- function(data, indices=1:NROW(data), hits="h", at.bats="ab")
{
 sum(data[indices, hits], na.rm=TRUE) / sum(data[indices, at.bats], na.rm=TRUE)
}

bat.avg(baseball)

##bootstrap it
#using the baseball data, call bat.avg 1,200 times
#pass indices to the function
avgboot <- boot(data=baseball, statistic = bat.avg, R=1200, stype="i")

#print original measure and estimates of bias and standard error
avgboot

#print the confidence interval
boot.ci(avgboot, conf=0.95, type="norm")

#graph
ggplot() + geom_histogram(aes(x=avgboot$t), fill='grey', color="grey") + geom_vline()
