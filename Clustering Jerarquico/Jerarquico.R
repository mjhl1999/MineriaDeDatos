library(cluster)
library(purrr)
 
data <- Freedman
data <- na.omit(data)
data
data <- scale(data)
d <- dist(data, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc2 <- agnes(data, method = "complete")
hc2$ac

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(data, method = x)$ac
}
map_dbl(m, ac)    

hc3 <- agnes(data, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes ward")

hc4 <- agnes(data, method = "average")
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of agnes average")

hc5 <- agnes(data, method = "single")
pltree(hc5, cex = 0.6, hang = -1, main = "Dendrogram of agnes single")

hc6 <- agnes(data, method = "complete")
pltree(hc6, cex = 0.6, hang = -1, main = "Dendrogram of agnes complete")

