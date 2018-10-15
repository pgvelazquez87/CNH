#####################
# Non linear models #
# Capítulo 23       #
#####################


## 23.1 NONLINEAR LEAST SQUARES

library(ggplot2)
load(url("http://jaredlander.com/data/wifi.rdata"))
head(wifi)

ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() + scale_color_gradient2(low="blue", mid="white", high="red", midpoint = mean(wifi$Distance))

# specify the square root model
# starting values are at the center of the grid (like an hypothesis, just for the creation of our first model)
wifimod1 <- nls(Distance ~ sqrt((betaX - x)^2 + (betaY - y)^2), data = wifi, start=list(betaX = 50, betaY=50))
summary(wifimod1)
coef(wifimod1)

ggplot(wifi, aes(x=x, y=y, color=Distance)) + geom_point() + scale_color_gradient2(low="blue", mid="white", high="red", midpoint = mean(wifi$Distance)) +
  geom_point(data=as.data.frame(t(coef(wifimod1))), aes(x=betaX, y=betaY), size=5, color="green")
  

## 23.2 SPLINES
# A spline is a function f that is a linear combination of n functions 
#(one for each unique data point) that are transformations of the variable x

data(diamonds)
#fit with a few different degrees of freedom, which must be greater than 1 but less than the number of unique x values in the data

ggplot(aes(x=carat, y=price), data=diamonds) + geom_point()

diaspline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price)
diaspline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaspline3 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=10)
diaspline4 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=20)
diaspline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=50)
diaspline6 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=100)

#we extract the information from the objects, build a data.frame, and then add a new layer on top of the standard scatterplot of the diamonds data

get.spline.info <- function(object)
{
  data.frame(x=object$x, y=object$y, df=object$df)
}

library(plyr)
splinedf <- ldply(list(diaspline1, diaspline2, diaspline3, diaspline4, diaspline5, diaspline6), get.spline.info)  

g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
g + geom_line(data = splinedf, aes(x=x, y=y, color=factor(round(df,0)), group=df)) + scale_color_discrete("Degrees of \nfreedom")
