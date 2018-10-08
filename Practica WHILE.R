### Práctica en R

library(magrittr)
library(ggplot2)
library(stringr)
data(diamonds)
head(diamonds)


nterms <- as.integer(readline(prompt = "How many terms?"))
n1 <- 0
n2 <- 1
count <- 2

  if(nterms <= 0) {
     print( "Please enter a positive integer")
     } else {
     if (nterms == 1) {
     print("Fibonacci sequence:")
     print(n1)
     } else {
     print("Fibonacci sequence:")
     print(n1)
     print(n2)
       while(count < nterms) {
         nth <- n1 + n2
         print(nth)
         n1 <- n2
         n2 <- nth
         count <- count+1
         }
     }
     }



for (columna in colnames(iris)) {
  print(paste0(columna, "(", nchar(columna), ")"))
}


while(TRUE) {
  x <- rnorm(1)
  if(x<0) {
  next  
    }
  print(x)
  if (x>1) {
    break
}
}

n <- 20
coin_outc <- vector(length = n, mode = "integer")
for (i in 1:20) {
  coin_outc[i] <- print(rbinom(n=1, size=1, prob = 0.5))
}


matriz <- matrix(data = NA, nrow=5, ncol=5)
for (i in 1:5) {
  for (j in 1:5)  {
    matriz[i,j] <- abs(i-j)
  }
}


i <- 1
while(TRUE) {
  j <- i+1
  product <- i*j 
  if (product>10000000) {
    print(j)
    break
    }
  i <- i+1
}

set.seed(22)
price_start <- 100
price <- price_start
n <- 0
target_diff <- 50
while(abs(price-price_start) < target_diff)   {
  change <- rnorm(1, 0, sd = 0.01)
  price <- price + (price*change)
  n <- n+1
  }
n

x <- 0L
while (TRUE) {
  cat(
    "I am thinking of a number between 1 and 10.",
    "\nTake a guess and press enter twice!"
  )
  x <- scan()[1]
  if (x == 5) {
    cat("Right!")
    break
  }
  cat("Wrong\n")
}


total <- 0L
while(TRUE)  {
  if (!total %in% 0:4)  {
    total <- 0L
  }
  n <- sample(c(2:12), 2)
  cat("What is the product of ", n[1], " and ", n[2], "?\n", sep = "")
  x <- as.integer(readline())
  if (x == prod(n)) {
    total <- total + 1L
    cat("Right!")
    if (total == 5) break
    cat("You just need", 5 - total, "more answers. \n\n")
  } else {
    cat("Wrong\n")
  }
}


## Ecuación de Poisson
pois <- data.frame(lambda.1=pois1, lambda.2 = pois2, lambda.5= pois5, lambda.10 = pois10, lambda.20 = pois20)
pois <- melt(data=pois, variable.name = "lambda", value.name = "x")
pois$lambda <- as.factor(as.numeric(str_extract(string = pois$lambda, pattern = "\\d+")))

ggplot(pois, aes(x=x)) + geom_density(aes(group=lambda, color = lambda, fill= lambda), adjust = 4, alpha = 0.5) + scale_color_discrete() + scale_fill_discrete()


## Correlaciones
data(economics)

library(reshape2)
library(scales)

econcor <- cor(economics[, c(2:6)])
econmelt <- melt(econcor, varnames=c("x", "y"), value.name="correlation")
econmelt <- econmelt[order(econmelt$correlation),]

ggplot(econmelt, aes(x=x, y=y)) + geom_tile(aes(fill=correlation)) + scale_fill_gradient2(low=muted("red"), mid="white", high= "steelblue", guide=guide_colorbar(ticks=FALSE, barheight = 10), limits=c(-1,1)) + theme_minimal() + labs(x=NULL, y=NULL)


library(RXKCD)
getXKCD(which="552")
