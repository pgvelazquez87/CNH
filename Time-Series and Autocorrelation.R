###################################
# TIME SERIES AND AUTOCORRELATION #
# 14 de noviembre de 2018         #
# Capítulo 24, página 367         #
###################################


library(WDI)
library(ggplot2)

gdp <- WDI(country=c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"), indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"), start=1960, end=2011)
WDIsearch()
# NY.GDP.PCAP.CD = GDP per capita (current US$)
# NY.GDP.MKTP.CD = GDP growth (annual %)
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")

library(scales)
#per capita GDP
ggplot(data=gdp, aes(x=Year, y=PerCapGDP, color=Country, linetype=Country)) + geom_line()

library(useful)
#absolute GDP
ggplot(gdp, aes(Year, PerCapGDP, color = Country, linetype=Country)) + geom_line() 



# get the US data
us <- gdp$PerCapGDP[gdp$Country == "United States"]
japan <- gdp$PerCapGDP[gdp$Country == "Japan"]

# convert it to a time series
us <- ts(us, start= min(gdp$Year), end = max(gdp$Year))
japan <- ts(japan, start=min(gdp$Year), end=max(gdp$Year))

plot(us, ylab = "Per Capita GDP", xlab = "Year")
plot(japan, ylab = "Per Capita GDP", xlab = "Year")


#the acf shows the correlation of a time with itself at one lag
acf(us)
pacf(us)
acf(japan)


#figure out the correct number of diffs
library(forecast)

ndiffs(x=us)
ndiffs(x=japan)
plot(diff(us, 2))
plot(diff(japan,1))


#checar acf y pacf para la serie desestacionalizada
acf(diff(us, 2))
pacf(diff(us, 2))


## arima function
## diff the series and fit seasonal effects

us.best <- auto.arima(x=us)
coef(us.best)

japan.best <- auto.arima(x=japan)
coef(japan.best)

#asegurarnos con acf y pacf que sólo es white noise, que es el supuesto que cada elemento en una series es un valor tomado aleatoriamente de una población con una media cero y varianza constante
acf(us.best$residuals)
pacf(us.best$residuals)

acf(japan.best$residuals)
pacf(japan.best$residuals)


#utilizamos la prueba de box para revisar si es estacionaria (la Ho es que es estacionaria)
library(stats)
Box.test(us.best$residuals, type="Ljung-Box")



#predict 5 years into the future and include the standard error
predict(us.best, n.ahead = 10, se.fit=TRUE)
predict(japan.best, n.ahead = 5, se.fit=TRUE)

#make a prediction for 5 years uout
forecast.us <- forecast(object=us.best, h=10)
forecast.jpn <- forecast(object=japan.best, h=10)


plot(forecast.us)
plot(forecast.jpn)



#####################################################################
# Generalized Autoregressive Conditional Heteroskedasticity (GARCH) #
#####################################################################

library(quantmod)
att <- getSymbols("T", auto.assign = FALSE)
library(xts)

head(att)
plot(att)

#plot in financial way
chartSeries(att)
addBBands()
addMACD(32, 50, 12)
addMACD(10, 50, 12)
addMACD(60, 50, 12)

#interested only in the closing price
attClose <- att$T.Close

# use of rugarch package
library(rugarch)
#primero definimos las especificaciones
attSpec <- ugarchspec(variance.model = list(model="sGARCH",
                                            garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(1,1)),
                      distribution.model = "std")

attGarch <- ugarchfit(spec=attSpec, data=attClose)
plot(attGarch@fit$residuals, type="l")
plot(attGarch, which=10)


#to judge the quality of our model, we build a few models with different mean specifications and compare their AICs

#ARMA(1,1)
attSpec1 <- ugarchspec(variance.model = list(model="sGARCH",
                                            garchOrder = c(1,1)),
                      mean.model = list(armaOrder = c(1,1)),
                      distribution.model = "std")


#ARMA(0,0)
attSpec2 <- ugarchspec(variance.model = list(model="sGARCH",
                                             garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0)),
                       distribution.model = "std")

#ARMA(0,2)
attSpec3 <- ugarchspec(variance.model = list(model="sGARCH",
                                             garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,2)),
                       distribution.model = "std")

#ARMA(1,2)
attSpec4 <- ugarchspec(variance.model = list(model="sGARCH",
                                             garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(1,2)),
                       distribution.model = "std")

attGarch1 <- ugarchfit(spec=attSpec1, data=attClose)
attGarch2 <- ugarchfit(spec=attSpec2, data=attClose)
attGarch3 <- ugarchfit(spec=attSpec3, data=attClose)
attGarch4 <- ugarchfit(spec=attSpec4, data=attClose)




