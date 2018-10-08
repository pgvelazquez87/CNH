##MODEL DIAGNOSTICS

library(boot)

housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", "SqFt", "Income", "IncomePerSqFt", "Expense", "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")

houseg1<- glm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing, family=gaussian(link = "identity"))
