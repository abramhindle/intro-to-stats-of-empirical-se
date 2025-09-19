# Basics
x <- 1 # assign 1 to x
square <- function(x) {  x * x } # square x
square(10) # 100
# comment

# if statement
if ( x >= 0 ) { print("x >= 0") } else { print("x < 0") }

# for statement
for (x in 1:10) { print(x) }

# map
summary(sapply(1:10, function(x) { x > 5 }))
summary(sapply(runif(10), function(x) { x > 0.5 }))


# Vectors in R
c(1:10) # vector for 1 to 10
1:10 # vector of 1 to 10
c(1:10,1:10) # vector of 1 to 10, 1 to 10
x <- c(1:10)
x[1] # 1st
x[10] # 10th
x[c(1,5,10)] #get 1st, 5th and 10th
sort(x) # sort em
order(x) #get the order of the indices/rank
x <- runif(10) # 10 uniformly random numbers
order(x) # order of x by indices
x[order(x)] # sort x
sort(x) #sort
x[ 1:length(x) %% 2 > 0] # show odd indices
x[ 1:length(x) %% 2 > 0] <- 1 # assign odd indices to 1
x # see x again

# load CSV
v <- read.csv("Author_NFRs.csv", header=T)
summary(v)
boxplot(v)
v$None # see the None column
names(v)
v$"Portability"

# Plotting
plot(v$Portability)
# Plot in order
plot(v$Portability[order(v$Portability)])
lines(v$Portability[order(v$Portability)])

plot(sort(v$Portability))
# plot the text names on them
text(v$Portability[order(v$Portability)], as.character(v$Authors[order(v$Portability)]))
# plot a histogram
plot(hist(v$Portability))
# plot a boxplot
boxplot(v$Portability)
# plot pdf
plot(density(v$Portability))
# plot ecdf
plot(ecdf(v$Portability))

# Generate Normal Values
x <- rnorm(100)
plot(x)
plot(hist(x))

# Generate Uniform Values
x <- runif(100)
plot(x)
plot(hist(x))

# factors and tables
v <- sapply(1:100, function(x) { if (runif(1) > 0.6) { "Good" } else { if (runif(1) > 0.5) { "Bad" } else { "Ok" } } })
v
v <- as.factor( v )
v
vt <- table(v)
plot(vt)

# useful stats
x <- rnorm(100)
summary(x)
median(x)
mean(x)

# libraries
# load a library
library(e1071)
install.packages("e1071")
library(e1071)
kurtosis(x)

# get help
help(summary)
help('?kurtosis')

v <- read.csv("Author_NFRs.csv", header=T)
install.packages("vioplot")
library(vioplot)
install.packages("beanplot")
library(beanplot)
beanplot(v$Portability)
beanplot(v$Portability,v$Reliability,side="both")
vioplot(v$Portability)
vioplot(v$Portability,v$Reliability)
# ok well it does a different thing
vioplot(v$Portability,side="left")
vioplot(v$Reliability,side="right",add=T)
