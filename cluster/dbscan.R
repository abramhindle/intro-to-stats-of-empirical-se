# install.packages('fpc')
library(fpc)
library(cluster)
library(MASS)
library(graphics)                                        #make up some data

n = 300
data = matrix(
             c(
               c(rexp(n),rexp(n,0.6),rexp(n,3),rexp(n,0.01)),
               c(rexp(n,1.1),rexp(n,0.7),rexp(n,3),rexp(n,0.11)),
               c(rexp(n,1.2),rexp(n,0.8),rexp(n,3),rexp(n,0.21))),
             ncol=4)
data.ds = dbscan(data, 0.5)


dims = c(2,3)
cols = topo.colors(4)

# plot 2 dimensions of the data
pdata <- function(dims) {
  plot(data[data.ds$cluster==0,dims],ylim=c(min(data[,dims[2]]),max(data[,dims[2]])),xlim=c(min(data[,dims[1]]),max(data[,dims[1]])),col=cols[1])
  points(data[data.ds$cluster==1,dims],col=cols[2])
  points(data[data.ds$cluster==2,dims],col=cols[3])
  points(data[data.ds$cluster==3,dims],col=cols[4])
}

#plot all of the dimensional combos
pdata(dims = c(1,2))
pdata(dims = c(1,3))
pdata(dims = c(1,4))
pdata(dims = c(2,3))
pdata(dims = c(2,4))
pdata(dims = c(3,4))
clusplot(data, data.ds$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)


# coffee stain, this example is a plain 2D shape
coffee <- read.csv('coffee.csv')
names(coffee) <- c('x','y','class')
coffee.ds = dbscan(coffee, 0.01)
plot(coffee[coffee.ds$cluster==0,c(1,2)],col="blue")
points(coffee[coffee.ds$cluster==1,c(1,2)],col="red")
points(coffee[coffee.ds$cluster==2,c(1,2)],col="orange")

clusplot(coffee[,c(1,2)], coffee.ds$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)

k2 <- kmeans(coffee[,c(1,2)],2)
clusplot(coffee[,c(1,2)], k2$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k3 <- kmeans(coffee[,c(1,2)],3)
clusplot(coffee[,c(1,2)], k3$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k4 <- kmeans(coffee[,c(1,2)],4)
clusplot(coffee[,c(1,2)], k4$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k10 <- kmeans(coffee[,c(1,2)],10)
clusplot(coffee[,c(1,2)], k10$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)



require(MASS)

#log scale the data
logdata = log(0.5+abs(data))
clusplot(logdata, data.ds$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)


# plot the density and the contour of the clusters
plotDensity <- function(data=data,dims=c(1,2)) {
  ncol=100
  colors  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
  d <- kde2d(data[,dims[1]], data[,dims[2]], n = c(300,300), lims=c(range(data[,dims[1]]), range(data[,dims[2]])))
        
  image(d,col=colors)
  for (i in 0:max(data.ds$cluster)) {
    d2 <- kde2d(data[data.ds$cluster==i,dims[1]], data[data.ds$cluster==i,dims[2]], n = c(300,300), lims=c(range(data[,dims[1]]), range(data[,dims[2]])))
    contour(d2, add=T, nlevels=1,labels=c(paste("Cluster",i)),method="simple")
    
  }
}
# explore the densities of the all dimensional combos
plotDensity(data=data,dims = c(1,2))


plotDensity(data=logdata,dims = c(1,2))
plotDensity(data=logdata,dims = c(1,3))
plotDensity(data=logdata,dims = c(1,4))
plotDensity(data=logdata,dims = c(2,3))
plotDensity(data=logdata,dims = c(2,4))
plotDensity(data=logdata,dims = c(3,4))

# coffee!
plotDensity(data=coffee,dims = c(1,2))


require(clusters)
# labels=4 - label the ellipses & interactive
# lines
# another plot of the data using dimensionality reduction
clusplot(logdata, data.ds$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)

# now kmeans
k2 <- kmeans(data,2)
clusplot(logdata, k2$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)

k3 <- kmeans(data,3)
clusplot(logdata, k3$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k4 <- kmeans(data,4)
clusplot(logdata, k4$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k5 <- kmeans(data,5)
clusplot(logdata, k5$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)
k6 <- kmeans(data,6)
clusplot(logdata, k6$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=2)


# Euclidean distance
d <- dist(data)

# compare 2 clustering
cluster.stats(d, k2$cluster, k3$cluster)

# hclust is a pain but the rectangles can help us
# give discreet clusters to hclust
# I am probably reinventing the wheel here
clustersOfHClust <- function(hc, n=3) {
  out <- c(1:length(hc$order))
  r <- rect.hclust(hc, n)
  for( i in 1:length(r)) {
    for ( x in r[[i]] ) {
      out[x] = i
    }      
  }
  out
}

# get 3 hclusts
hc <- hclust(d)
plot(hc)
hcCluster3 <- clustersOfHClust(hc, n=3)
hcCluster6 <- clustersOfHClust(hc, n=6)
hcCluster9 <- clustersOfHClust(hc, n=9)


cd <- hclust(dist(coffee[,c(1,2)]))
plot(hc)
cdhc3 <- clustersOfHClust(cd, n=3)


# jam the clusterings into one list
clustering = list(data.ds$cluster, k2$cluster, k3$cluster, k4$cluster, k5$cluster, k6$cluster,
  hcCluster3,  hcCluster6,      hcCluster9
  )
# name the list
alabels <- c("DBScan0.5", "Kmeans2", "Kmeans3", "Kmeans4", "Kmeans5", "Kmeans6","HClust3","HClust6","HClust9")
# get the within averages
awithin <- sapply(clustering,function(x) { cluster.stats(d, x)$average.within })
# get the between averages
adist   <- sapply(clustering,function(x) { cluster.stats(d, x)$average.between })
# plot within versus between
plot(awithin,adist)
# give them labels
text(awithin,adist,alabels,pos=3)

#don't run this unless you need better stats about cluster differences
pairwiseClusters <- function() {
  res <- lapply(clustering, function(cluster1) {
    lapply(clustering, function(cluster2) {
      cluster.stats(d, cluster1, cluster2)    
    })
  })
}

# install.packages("umap")
library(umap)
cumap = umap(coffee[,c(1,2)])
head(cumap)
plot(cumap$layout,col=c("red","blue")[1+coffee[,3]])

library(colorspace)
color=diverge_hcl(length(data))[rank(data)]
dumap = umap(data)
plot(dumap$layout,col=color)

