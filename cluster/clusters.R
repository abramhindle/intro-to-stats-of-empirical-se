

v <- read.csv("Author_NFRs.csv")
vv <- v[1:18,]
for (i in 2:17) {
  vv[,i] <- as.numeric(vv[,i])
}
tv <- t(vv[,2:8])
authors <- matrix(tv,nrow=7,dimnames=list(labels(tv)[[1]],vv[,1]))
#hc <- hclust(as.dist(cor(authors)),method="ward")
#hc <- hclust(as.dist(cor(authors)),method="centroid")
#

pdf("oldpostgresql-author-cluster.pdf")

#hc <- hclust(as.dist(cor(authors)),method="ward")
#plot(hc,sub="Organized into 2 and 6 clusters",xlab="PostgreSQL Authors")
#rect.hclust(hc,k=2,border="blue")
##rect.hclust(hc,k=3,border="red")
##rect.hclust(hc,k=4,border="green")
#rect.hclust(hc,k=6,border="purple")


hc <- hclust(as.dist(1-cor(authors,method="spearman")),method="ward")
hc <- hclust(dist(t(authors)),method="ward")
plot(hc,sub="Organized into 2 and 6 clusters",xlab="PostgreSQL Authors")
rect.hclust(hc,k=2,border="blue")
#rect.hclust(hc,k=3,border="red")
#rect.hclust(hc,k=4,border="green")
rect.hclust(hc,k=6,border="purple")


dev.off()

asqr <- cor(authors)
asqr <- asqr^2
hc <- hclust(as.dist(asqr),method="ward")
plot(hc,sub="Organized into 2 and 6 clusters",xlab="PostgreSQL Authors")
rect.hclust(hc,k=2,border="blue")
#rect.hclust(hc,k=3,border="red")
#rect.hclust(hc,k=4,border="green")
rect.hclust(hc,k=6,border="purple")


# install.packages('lsa')
library(lsa)
pdf("postgresql-author-cluster-cosine.pdf")
hc <- hclust(as.dist(1-cosine(authors)),method="ward")
plot(hc,sub="Organized into 2 and 6 clusters",xlab="PostgreSQL Authors")
rect.hclust(hc,k=2,border="blue")
#rect.hclust(hc,k=3,border="red")
#rect.hclust(hc,k=4,border="green")
rect.hclust(hc,k=6,border="purple")
dev.off()








nauthors <- authors
normalize <- function(x) { x / sum(x) }

makeAuthorData <- function(x) {
  l <- as.factor(labels(nauthors)[[1]])
  unlist(sapply(c(1:length(l)), function(i) { rep(l[i], x[i]) }))
}


for (author in labels(nauthors)[[2]]) { nauthors[,author] <-
                                          normalize(authors[,author])
                                      }
nauthor <- length(labels(authors)[[2]])


# grr fixed it to this. The easiest to explain clustering
pdf("postgresql-author-cluster.pdf")
hc <- hclust(dist(t(nauthors)),method="ward")
plot(hc,sub="Organized into 2 and 6 clusters",xlab="PostgreSQL Authors")
rect.hclust(hc,k=2,border="black")
#rect.hclust(hc,k=3,border="red")
#rect.hclust(hc,k=4,border="green")
rect.hclust(hc,k=6,border="dimgrey")
dev.off()



authorNames = labels(authors)[[2]]
chisqm <- matrix(c(1:(nauthor*nauthor))*0,ncol=nauthor,dimnames=list(authorNames,authorNames))
ksm <- matrix(c(1:(nauthor*nauthor))*0,ncol=nauthor,dimnames=list(authorNames,authorNames))

for (author1 in labels(authors)[[2]]) {
  for (author2 in labels(authors)[[2]]) {
    print(paste(author1,author2))
    pv   <- chisq.test(authors[,c(author1,author2)])$p.value
    kspv <- ks.test(as.numeric(makeAuthorData(authors[,author1])),
                    as.numeric(makeAuthorData(authors[,author2])))$p.value
    print(paste(pv,kspv))
    chisqm[author1,author2] <- pv
    ksm[author1,author2] <- pv
  }
}
chisqm[!is.finite(chisqm)] <- 0
ksm[!is.finite(ksm)] <- 0
general<-sapply(labels(authors)[[1]],function(x){sum(authors[x,])})
ngeneral<-sapply(labels(nauthors)[[1]],function(x){sum(nauthors[x,])})

sapply( authorNames, function(x) { median(chisqm[,x]) })

pdf("author-distance-from-aggregate.pdf")
d <- sapply(labels(authors)[[2]],function(x){cor(general,authors[,x])})
asize <- sapply(labels(authors)[[2]],function(x){sum(authors[,x])})
print(cor(d,asize))
print(cor(d,asize,method="spearman"))
plot(d,asize,main="Author Correlation with NFR distribution versus # of Author Topics",xlab="Similarity of Author NFR topics to global distribution",ylab="# of Topics associated with Author")
text(d,asize,labels(authors)[[2]],pos=3)
dev.off()


pdf("author-distance-from-aggregate-normalized.pdf")
nd <- sapply(labels(nauthors)[[2]],function(x){cor(general,nauthors[,x])})
nasize <- sapply(labels(nauthors)[[2]],function(x){sum(nauthors[,x])})
print(cor(nd,asize))
print(cor(nd,asize,method="spearman"))
plot(nd,asize,main="Author Correlation with NFR distribution versus # of Author Topics",xlab="Similarity of Author NFR topics to global distribution",ylab="# of Topics associated with Author")
text(nd,asize,labels(authors)[[2]],pos=3)
dev.off()


                                        #
#v
#v[,1:18]
#v[c(2:8),1:18]
#v[1:18,]
#v[1:18,c(2:8)] 
#cor(v[1:18,c(2:8)])
#cor(as.numeric(v[1:18,c(2:8)]))
#v
#v[1:18,]
#vv <- v[1:18,]
#v[,2]
#v[,3]
#vv[,3]
#vv[,3]
#length(vv)
#sapply(2:17,function(x) { vv[,x] <- as.numeric(vv[,x]) })
#vv
#cor(vv[,c(2:8)]
#)
#vv[,8]
#summary(vv[,8])
#summary(vv[,2])
#summary(vv[,3])
#vv[,2] <- as.numeric(vv[,2])
#cor(vv[,c(2:8)]
#)
#vv[,2:8]
#t(vv[,2:8])
#t(vv[,1:8])
#t(vv[,2:8])
#cor(t(vv[,2:8]))
#history
#history(show.max=Inf)
#t(vv[,2:8])
#labels(t(vv[,2:8]))
#labels(t(vv[,2:8]))[[2]] <- v[,1]
#matrix(t(vv[,2:8]),nrow=7)
#tv <- t(vv[,2:8])
#labels(tv)
#labels(tv)[[1]]
#matrix(tv,nrow=7,labels=(vv[,1],labels(tv)[[1]]))
#matrix(tv,nrow=7,labels=list(vv[,1],labels(tv)[[1]]))
#help(matrix)
#matrix(tv,nrow=7,dimnames=list(vv[,1],labels(tv)[[1]]))
#matrix(tv,nrow=7,dimnames=list(labels(tv)[[1]],vv[,1]))
#authors <- matrix(tv,nrow=7,dimnames=list(labels(tv)[[1]],vv[,1]))
#cor(authors)
#image(authors)
#image(cor(authors))
#hclust(cor(authors))
#hclust(authors)
#hclust(cor(authors))
#help(hclust)
#hclust(cor(authors))
#hclust(as.dist(cor(authors)))
#hc <- hclust(as.dist(cor(authors)))
#plot(hc)
#pdf("author-cluster.pdf")
#plot(hc)
#dev.off()
#history(max.show=Inf)
#write(history(max.show=Inf),file="Rcommands")
#help(write)
#history(max.show=Inf,file="Rcommands")
#help(history)
#savehistory("Rcommands.txt")
image(authors)
hc <- hclust(dist(t(authors)))
image(authors[,hc$order])
hc <- hclust(as.dist(1-cosine(authors)),method="ward")
image(authors[,hc$order])
nauthors = t(t(authors)/sapply(1:18,function(i){sum(authors[,i])}))
image(nauthors)
hc <- hclust(dist(t(authors)))
image(nauthors[,hc$order])
hc <- hclust(as.dist(1-cosine(authors)),method="ward")
image(nauthors[,hc$order])
