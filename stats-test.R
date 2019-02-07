t.test(rnorm(100),rnorm(100))
t.test(runif(100),rnorm(100))
t.test(rnorm(100,1),rnorm(100))


wilcox.test(rnorm(100),rnorm(100))
wilcox.test(runif(100),rnorm(100))
wilcox.test(rnorm(100,1),rnorm(100))

         t.test(rnorm(100),rnorm(100))
         t.test(runif(100),rnorm(100))
         t.test(rnorm(100,1),rnorm(100))

chisq.test(c(10,10,10,30),p=c(20,20,20,30),rescale.p=TRUE)
chisq.test(c(10,10,10,30),p=c(4,5,6,7),rescale.p=TRUE)
chisq.test(c(10,10,10,30),p=c(11,11,11,31),rescale.p=TRUE)
chisq.test(c(10,10,10,30),p=c(0,11,11,0),rescale.p=TRUE) # zeros are bad

south <- c(10,20,30,40)
north <- c(5,30,30,40)
nstab <- as.table(rbind(south,north))
chisq.test(nstab)
south <- c(10,20,30,40)
north <- c(90,30,30,40)
nstab <- as.table(rbind(south,north))
chisq.test(nstab)


wilcox.test(rnorm(100),rnorm(100))
wilcox.test(runif(100),rnorm(100))
wilcox.test(rnorm(10,1),rnorm(10))

ks.test(rnorm(100),rnorm(100))
ks.test(runif(100),rnorm(100))
ks.test(rnorm(100,1),rnorm(100))


stbdtypes <- c("Source","Test","Build","Doc")
maint     <- c("Adaptive","Perfective","Corrective")
stbds <- stbdtypes[runif(100)*4 + 1]
maints <- maint[runif(100)*3 + 1]
head(stbds)
head(maints)
st <- table(stbds,maints)
st
chisq.test(st)
st2 <- t(cbind(st[,"Adaptive"],st[,"Corrective"]))
st2
chisq.test(st2)
# example where we make a table with junk results
st3 <- t(cbind(st[,"Adaptive"],max(5,round(st[,"Corrective"]+10*rnorm(4)))))
st3
chisq.test(st3)

# boot strap mean

N=10000
alpha = 0.05
data = rnorm(N)
mean(data)
mean(sample(data,N,replace=TRUE))
booted <- sapply(c(1:N), function(i) { mean(sample(data,N,replace=TRUE)) })
mean(booted)
summary(booted)
quantile(booted,c(alpha/2,1.0 - alpha/2))
plot(density(booted))

# difference of means

N=100
alpha = 0.05
data1 = rnorm(N)
data2 = rnorm(N,mean=0.5)
mean(data1)
mean(data2)
mean(data1) -  mean(data2)    
booted <- sapply(c(1:N*100), function(i) { mean( sample(data1,N,replace=TRUE) ) - mean( sample(data2,N,replace=TRUE)  ) })
mean(booted)
summary(booted)
quantile(booted,c(alpha/2,1.0-alpha/2))
plot(density(booted))

counts <- c(100,100,100,500,500,500,1000,1000,10000)
boots <- sapply(counts, function(N) {
    alpha = 0.05
    data1 = rnorm(N)
    data2 = rnorm(N,mean=0.5)
    data2[1] = 10
    mean(data1)
    mean(data2)
    mean(data1) -  mean(data2)    
    booted <- sapply(c(1:N), function(i) { mean( sample(data1,N,replace=TRUE) ) - mean( sample(data2,N,replace=TRUE)  ) })
    print(mean(booted))
    print(summary(booted))
    print(N)
    print(quantile(booted,c(alpha/2,1.0 - alpha/2)))
    booted
})
plot(density(boots[[length(boots)]]),xlim=c(-1,0.25),ylim=c(0,30))
for (i in c(1:length(boots))) {
    lines(density(boots[[i]]),col=i)
}
legend(0,30,counts,pch=1,col=c(1:length(boots)))

# difference of skews
# install.packages("moments")
library(moments)
    N=100
    alpha = 0.05
    data1 = rnorm(N)
    data2 = rnorm(N,mean=0.5)
    median(data1)
    median(data2)
    median(data1) -  median(data2)    
    booted <- sapply(c(1:N), function(i) { median( sample(data1,N,replace=TRUE) ) - median( sample(data2,N,replace=TRUE)  ) })
    mean(booted)
    summary(booted)
    quantile(booted,c(alpha/2,1.0 - alpha/2))
    plot(density(booted))
