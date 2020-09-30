t.test(rnorm(100),rnorm(100))
t.test(runif(100),rnorm(100))
t.test(rnorm(100,1),rnorm(100))

qq.

t1 <- rnorm(50,0.7)
t2 <- rnorm(50,0.0)
t.test(t1,t2)
ht1 <- hist(t1, plot=FALSE)
ht2 <- hist(t2, plot=FALSE)

# From Data Analytics UK
# https://www.dataanalytics.org.uk/plot-two-overlapping-histograms-on-one-chart-in-r/
c1 <- rgb(173,216,255, max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,255,203, max = 255, alpha = 80, names = "lt.yellow")
par(mfrow=c(2,1))
plot(ht1,col=c1)
plot(ht2,col=c2)
par(mfrow=c(1,1))


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

N=1000
REPS=10000
alpha = 0.05
data = rnorm(N,0.0) + rnorm(N,1.0,0.1)
plot(density(data))
mean(data)
mean(sample(data,N,replace=TRUE))
booted <- sapply(c(1:REPS), function(i) { mean(sample(data,N,replace=TRUE)) })
mean(booted)
summary(booted)
quantile(booted,c(alpha/2,1.0 - alpha/2))
par(mfrow=c(2,1))
plot(density(booted))
plot(density(data))
par(mfrow=c(1,1))

# difference of means

REPS=1000
N=100
alpha = 0.05
data1 = rnorm(N)
data2 = rnorm(N,mean=0.5)
mean(data1)
mean(data2)
mean(data1) -  mean(data2)    
booted <- sapply(c(1:REPS), function(i) {
    mean( sample(data1,N,replace=TRUE) ) -
        mean( sample(data2,N,replace=TRUE)  )
})
booted <- sapply(c(1:REPS), function(i) {
    t.test(sample(data1,N,replace=TRUE) ,
           sample(data2,N,replace=TRUE) )$p.value
})
mean(booted)
summary(booted)
quantile(booted,c(alpha/2,1.0-alpha/2))
par(mfrow=c(3,1))
plot(density(booted))
plot(density(data1))
plot(density(data2))

# Demonstrate difference with sample size
par(mfrow=c(1,1))
counts <- c(100,500,1000,10000)
REPS=1000
boots <- lapply(counts, function(N) {
    alpha = 0.05
    data1 = rnorm(N)
    data2 = rnorm(N,mean=0.5)
    data2[1] = 100 # add an outlier
    mean(data1)
    mean(data2)
    mean(data1) -  mean(data2)    
    booted <- sapply(c(1:REPS), function(i) {
        mean( sample(data1,N,replace=TRUE) ) -
            mean( sample(data2,N,replace=TRUE)  ) })
    print(mean(booted))
    print(summary(booted))
    print(N)
    print(REPS)
    print(quantile(booted,c(alpha/2,1.0 - alpha/2)))
    booted
})
# boots
plot(density(boots[[length(boots)]]),xlim=c(-1,0.25),ylim=c(0,30))
for (i in c(1:length(boots))) {
    lines(density(boots[[i]]),col=i)
}
legend(0,30,counts,pch=1,col=c(1:length(boots)))


# Demonstrate difference with repetitions
par(mfrow=c(1,1))
rcount <- 100
data1 = rnorm(rcount)
data2 = rnorm(rcount,mean=0.5)
data2[1] = 100
REPS <- c(10,100,1000,10000,100000)
boots <- lapply(REPS, function(N) {
    alpha = 0.05
    mean(data1)
    mean(data2)
    mean(data1) -  mean(data2)    
    booted <- sapply(c(1:N), function(i) { mean( sample(data1,rcount,replace=TRUE) ) - mean( sample(data2,rcount,replace=TRUE)  ) })
    print(mean(booted))
    print(summary(booted))
    print(N)
    print(REPS)
    print(quantile(booted,c(alpha/2,1.0 - alpha/2)))
    booted
})
plot(density(boots[[length(boots)]]),xlim=c(-1,0.25),ylim=c(0,30))
for (i in c(1:length(boots))) {
    lines(density(boots[[i]]),col=i)
}
legend(0,30,REPS,pch=1,col=c(1:length(boots)))





# difference of skews
# install.packages("moments")
library(moments)
N=100
REPS=1000
alpha = 0.05
data1 = rnorm(N)
data2 = rnorm(N,mean=0.5)
skewness(data1)
skewness(data2)
skewness(data1) -  skewness(data2)    
booted <- sapply(c(1:REPS), function(i) {
    skewness( sample(data1,N,replace=TRUE) ) - skewness( sample(data2,N,replace=TRUE)  )
})
mean(booted)
summary(booted)
quantile(booted,c(alpha/2,1.0 - alpha/2))
plot(density(booted))
