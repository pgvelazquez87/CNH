##MODEL DIAGNOSTICS

library(boot)

housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")

houseg1<- glm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing, family=gaussian(link = "identity"))
houseg2 <- glm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
houseg3 <- glm(ValuePerSqFt ~ Units + SqFt*Boro + Class, data=housing)
houseg4 <-  glm(ValuePerSqFt ~ Units + SqFt * Boro + SqFt*Class, data=housing)
houseg5 <- glm(ValuePerSqFt ~ Boro + Class, data=housing)

obj <- list("2"=houseg2, "3"=houseg3)

for (i in 2:3)  {
  assign(paste0("housecv", i), cv.glm(housing, obj[["i"]] , K=5))
}
