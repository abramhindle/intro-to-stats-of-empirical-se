#neil and abram annotations!
cores <- 8
pgsqln <- read.csv("../IRR/output/pgsqln.arff.csv",header=TRUE)
pgsqla <- read.csv("../IRR/output/pgsqla.arff.csv",header=TRUE)
nn <- length(pgsqln)
na <- length(pgsqla)
pn <- pgsqln[,(nn-6):nn]
pa <- pgsqla[,(na-6):na]

students <- c()
students$abr   <- c(1,1,0,0,0,0)
students$kal   <- c(0,1,0,0,0,0)
students$sam   <- c(0,1,1,0,0,0)
students$reb   <- c(0,1,0,0,0,0)
students$lui   <- c(0,1,1,0,1,0)
students$art   <- c(0,1,1,0,0,0)

students <- data.frame(students)

library(irr)
#students[,c("lui","abr")]
# http://en.wikipedia.org/wiki/Fleiss%27_kappa
# http://en.wikipedia.org/wiki/Cohen%27s_kappa
# http://en.wikipedia.org/wiki/Inter-rater_reliability#Kappa_statistics
kappa2(matrix(c(c(1:100)*0,c(1:100)*1),ncol=2))
kappa2(matrix(c(c(1:100)*1,c(1:100)*1),ncol=2))
kappa2(matrix(c(c(1:100)*0,c(1:100)*0),ncol=2))
kappa2(matrix(round(c( 1*(runif(600)>.9), 1*(runif(600)>.9))), ncol=2))
kappa2(matrix(c( 1*(runif(600)>.99), 1*(runif(600)>.99)), ncol=2))
kappa2(matrix(c( (runif(600)>.5), (runif(600)>.5)), ncol=2))
kappa2(matrix(c( 1*(runif(600)>.1), 1*(runif(600)>.1)), ncol=2))

normit <- function(x) { x*4 + 1 } 

weight <- "unweighted"
#for (weight in c("unweighted","equal","squared")) {
  for (i in c(1:7)) {
    print(paste(names(pa)[[i]],weight))
    mat <- matrix(c( normit(pa[,i]), normit(pn[,i]) ), ncol=2)
    print(kappa2(mat, weight=weight))
    print(kappam.fleiss(mat))
    #print(cor(pa[,i],pn[,i]))
    print(cor(pa[,i],pn[,i],method="spearman"))
    #print(paste("Difference: ",abs(sum(pa[,i] - pn[,i]))))
    #print(paste("Matching: ",length(pa[pa[,i]==pn[,i],i])))
    #print(paste("Not Matching: ",length(pa[pa[,i]!=pn[,i],i])))
    print(paste("Pos Disagree: ",length(pa[pa[,i]+pn[,i]==1,i])))
    print(paste("Pos Agree: ",length(pa[pa[,i]+pn[,i]==2,i])))

  }
#}

sapply(names(pa),function(i) {
  mat <- matrix(c( as.factor(pa[,i]), as.factor(pn[,i]) ), ncol=2)
  kappam.fleiss(mat)$value
})
sapply(names(pa),function(i) {
  mat <- matrix(c( as.factor(pa[,i]), as.factor(pn[,i]) ), ncol=2)
  kappa2(mat)$value
})

sapply(names(pa),function(i) {
  mat <- matrix(c( as.factor(pa[,i]), as.factor(pn[,i]) ), ncol=2)
  mat
})


sapply(names(pa),function(i) {
  cor(pa[,i],pn[,i],method="spearman")
})
paall <- sapply(c(1:7),function(x){ pa[,x] })
pnall <- sapply(c(1:7),function(x){ pn[,x] })


mat <- matrix(c( paall, pnall ), ncol=2)
kappam.fleiss(mat)$value
kappa2(mat)$value


  for (i in c(1:7)) {
    print(paste(names(pa)[[i]]))
    print(paste("Pos Disagree: ",length(pa[pa[,i]+pn[,i]==1,i])))
    print(paste("Pos Agree: ",length(pa[pa[,i]+pn[,i]==2,i])))
    print(paste("Neg Agree: ",length(pa[pa[,i]+pn[,i]==0,i])))
  }



sapply(names(pa),function(i) {
  mat <- matrix(c( normit(pa[,i]), normit(pn[,i]) ), ncol=2)
  kappa2(mat)$value
})

kappaof <- function(sai,pai) {
  #mat <- matrix(c( sai, pai ), ncol=2)
  mat <- matrix(c( as.factor(sai), as.factor(pai) ), ncol=2)
  kappa2(mat)$value
}


# repeat N times
# sample from the distribution itself
# and see what the average Kappa score is per class
paunion <- pa
for (i in names(pa)) {
  paunion[,i] <- (pa[,i] & pn[,i]) * 1
}
kappaofself <- function( pan, N) {
  sapply(names(pan),
         function(x){
           mean(sapply(c(1:N),
                       function(x){
                         sapply(names(pan),function(i){
                           kappaof(sample(pan[,i],630,replace=T),pan[,i])})})[x,])})
}

library(irr)
library(multicore)

paunion <- pa
for (i in names(pa)) {
  paunion[,i] <- (pa[,i] & pn[,i]) * 1
}

ratings <- list()
ratings[["Abram"]] <- pa
ratings[["Neil"]] <- pn
ratings[["Both"]] <- paunion
panames <- c("Portability","Functionality","Reliability","Maintainability","Efficiency", "Usability", "None")

for (person in c("Both","Abram","Neil")) {
pan <- ratings[[person]]
i <- names(pan)[1]
#
#           mean(
#                mclapply(c(1:N),
#                sapply(c(1:N),
#                       function(x){
#                         sapply(names(pan),function(i){
#                           kappaof(sample(pan[,i],630,replace=T),pan[,i])})})[x,])})
#}


#versus a sample

N <- 100000
#N <- 1000
kappas <- lapply(names(pa),function(acolumn) {
  unlist(
         mclapply(
                  c(1:N),
                  function(run) {
                    kappaof(sample(pan[,acolumn],length(pan[,acolumn]),replace=T),pan[,acolumn])
                  },
                  mc.cores=cores)
         )
})

pankappas <- sapply(names(pa),function(i) {
   kappaof(pa[,i],pn[,i])
})

  pdf(paste("self-sample",person,"pdf",sep="."),width=12,height=8)
  miny <- min(c(sapply(kappas,min), min(pankappas)))
  maxy <- max(c(sapply(kappas,max), max(pankappas)))
  boxplot(kappas, names=panames,ylim=c(miny,maxy))
  title(paste("Human Ratings (",person,") versus Sampled Random Ratings"))
  lines(pankappas, type="o",col="red",lw=3)
  legend("topleft", c("Interrater Reliability per NFR Topic Label"), bty="o",lwd=3, col=c("red"),  lty=c(1));
  dev.off()
  print(paste("Person ", person))
  print(sapply(kappas,mean))
  print(paste("Person ECDF measures", person))
  print(sapply(c(1:7),function(i){ecdf(kappas[[i]])(pankappas[i])}))
}


#now versus unif

for (threshold in c(0.01,0.1,0.1,0.25,0.5,0.9,0.99)) {
  N <- 10000
  kappas <- lapply(names(pa),function(acolumn) {
                                        #acolumn <- i

    unlist(
         mclapply(
         c(1:N),
         function(run) {
             kappaof(
                     ((runif(length(pan[,acolumn])) < threshold) * 1)
                     ,pan[,acolumn])
         },
         mc.cores=cores)
           )
  })
  pankappas <- sapply(names(pa),function(i) {
   kappaof(pa[,i],pn[,i])
  })



  pdf(paste("unif-threshold",threshold,"pdf",sep="."),width=12,height=8)
  miny <- min(c(sapply(kappas,min), min(pankappas)))
  maxy <- max(c(sapply(kappas,max), max(pankappas)))
  boxplot(kappas, names=panames,ylim=c(miny,maxy))
  title(paste("Human Ratings versus Random Ratings with Threshold of ",threshold))
  lines(pankappas, type="o",col="red",lw=3)
  dev.off()
  print(paste("Threshold ", threshold))
  print(sapply(kappas,mean))
  print(paste("Threshold ECDF measures", threshold))
  print(sapply(c(1:7),function(i){ecdf(kappas[[i]])(pankappas[i])}))

}
# at 1000
# [1] "Person  Abram"
# [1] -6.844509e-04 -2.526857e-03 -9.342679e-04 -1.994073e-03 -1.256012e-04
# [6] -5.947729e-05  2.560513e-03
# [1] "Person ECDF measures Abram"
# [1] 1.000 0.377 0.536 0.978 1.000 0.656 0.951
# [1] "Person  Neil"
# [1]  0.0010039020  0.0005305748 -0.0011660652 -0.0015651146 -0.0023653867
# [6]  0.0008542890  0.0016953597
# [1] "Person ECDF measures Neil"
# [1] 1.000 0.375 0.567 0.980 1.000 0.585 0.921




                                        # Abram annotations 1000 times
                                        #    A_portability   A_functionality     A_reliability A_maintainability 
#     0.0084965406      0.0062652051     -0.0055896254     -0.0008711916 
#     A_efficiency       A_usability            A_none 
#    -0.0021828228      0.0031867318     -0.0008958907 

# kappaofself( pn, 1000)
#  A_functionality     A_portability       A_usability     A_reliability 
#     0.0057201264     -0.0062654271     -0.0011111470     -0.0024650803 
#     A_efficiency A_maintainability            A_none 
#    -0.0020136751      0.0082175191     -0.0009733678 
#kappaofself( pn, 1000)
#kappaofself( pa, 1000)
#kappaofself( paunion, 10)
#kappaofself( paunion, 100)
#kappaofself( paunion, 1000)

kappaofunif <- function( pan, N, threshold) {
  sapply(names(pan),
         function(x){
           mean(sapply(c(1:N),
                       function(x){
                         sapply(names(pan),function(i){
                           kappaof(
                                   ((runif(N) < threshold) * 1),
                                   
                                   pan[,i])})})[x,])})
}
#kappaofself( paunion, 1000)
#    A_portability   A_functionality     A_reliability A_maintainability 
#     -0.004717955       0.002173449      -0.004581317      -0.001811884 
#     A_efficiency       A_usability            A_none 
#     -0.001374283      -0.001031894      -0.001149971
#cc <- c(0,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1.0)
#for (i in cc) {
#  kappaofunif(pa,1000,i)
#  kappaofunif(pn,1000,i)
#  kappaofunif(paunion,1000,i)
#}
