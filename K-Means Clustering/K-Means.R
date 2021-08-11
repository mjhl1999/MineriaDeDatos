install.packages("factoextra") 
library(factoextra)
install.packages("tidyverse")
library(tidyverse)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("gridExtra")
library(gridExtra)

install.packages("gridExtra")
library(dplyr)

data("USArrests")    
df <- scale(USArrests) 
head(df, n = 3)

set.seed(123)
km.res <- kmeans(df, 4, iter.max = 1000,nstart = 25)
print(km.res)
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)
km.res$cluster
head(km.res$cluster, 4)
km.res$size
km.res$centers
str(km.res)
fviz_cluster(km.res, data = df)

kmeans(df, 4, iter.max=1000,nstart=25)


d_frame <- USArrests
d_frame <- na.omit(d_frame)  
d_frame <- scale(d_frame)
head(d_frame)
 
kmeans2 <- kmeans(d_frame, centers = 2, nstart = 25)
str(kmeans2)
fviz_cluster(kmeans5, data = d_frame)
  
kmeans3 <- kmeans(d_frame, centers = 3, nstart = 25)  #DataFlair
kmeans4 <- kmeans(d_frame, centers = 4, nstart = 25)  
kmeans5 <- kmeans(d_frame, centers = 5, nstart = 25)  

#Comparing the Plots
plot1 <- fviz_cluster(kmeans2, geom = "point", data = d_frame) + ggtitle("k = 2")
plot2 <- fviz_cluster(kmeans3, geom = "point", data = d_frame) + ggtitle("k = 3")
plot3 <- fviz_cluster(kmeans4, geom = "point", data = d_frame) + ggtitle("k = 4")
plot4 <- fviz_cluster(kmeans5, geom = "point", data = d_frame) + ggtitle("k = 5")
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)


#Exploracion de datos usando 

glimpse(USArrests)

summary(USArrests)

install.packages("corrplot")
library(corrplot)
corrplot(cor(USArrests), order = "hclust")
