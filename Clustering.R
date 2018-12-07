###################################
# CLUSTERING                      #
# 20 de noviembre de 2018         #
# Capítulo 25, página 389         #
###################################


wineurl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.table(wineurl, header=FALSE, sep=',', stringsAsFactors = FALSE,
                   col.names = c('Cultivar', 'Alcohol', 'Malic.acid', 'Ash', 'Alcalinity.of.ash', 'Magnesium',
                               'Total.phenols', 'Flavanoids', 'Nonflavanoid.phenols', 'Proanthocyanin', 'Color.intensity',
                               'Hue', 'OD280.OD315.of.diluted.wines', 'Proline'))

head(wine)
str(wine)

winetrain <- wine[, which(names(wine) != 'Cultivar')]

set.seed(278613)
winek3 <- kmeans(x=winetrain, centers=3)

library(useful)
plot(winek3, data=winetrain)
plot(winek3, data=wine, class='Cultivar')


## it is a good practice to start with a random number of starts
set.seed(278613)
winek3_2 <- kmeans(x=winetrain, centers=3, nstart=25)


## Choosing the right number of clusters is important in getting a good partitioning of the data
## 1. Use of Hartingan´s rule to choose right number of clusters

winebest <- FitKMeans(winetrain, max.clusters = 20, nstart = 25, seed = 278613)
PlotHartigan(winebest)

winek13 <- kmeans(x=winetrain, centers=13)

plot(winek13, data=winetrain)

#compare distributions based on Cultivar
table(wine$Cultivar, winek3$cluster)
plot(table(wine$Cultivar, winek3$cluster), 
     main="Confusion matrix for Wine clustering", xlab="Cultivar", ylab="Cluster")

plot(table(wine$Cultivar, winek13$cluster), 
     main="Confusion matrix for Wine clustering", xlab="Cultivar", ylab="Cluster")



## 2. Use of Gap-Statistic to choose right number of clusters
library(cluster)
thegap <- clusGap(winetrain, FUNcluster = pam, K.max = 20)
gapDF <- as.data.frame(thegap$Tab)

gap_clusters <- which(thegap$Tab[ ,3] == min(thegap$Tab[ ,3]))

assign(paste('winek', gap_clusters, sep=''), kmeans(x=winetrain, centers=gap_clusters))
plot(winek5, data=wine, class="Cultivar")
plot(table(wine$Cultivar, winek5$cluster), main="Confusion matrix for wine clustering", xlab="Cultivar", ylab="Cluster")





