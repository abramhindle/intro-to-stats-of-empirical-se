# Based on this http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html
# And this https://www.rdocumentation.org/packages/gmodels/versions/2.18.1/topics/CrossTable
# install.packages("gmodels")
# More information here https://scholarworks.umass.edu/pare/vol20/iss1/8

# Loosely based on:

# Rodriguez, Ariel & Tanaka, Fumiya & Kamei, Yasutaka. (2018).
# Empirical Study on the Relationship Between Developer's Working
# Habits and Efficiency. 10.1145/3196398.3196458.

library(gmodels)
N <- 1000
dow <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
Days <- sample(dow,N,replace=TRUE)
bs <- c("SyntaxError","TestFailure","Success")
Builds <- sample(bs,N,replace=TRUE)
hod <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23")
Times <- sample(hod,N,replace=TRUE)
data <- data.frame(Days,Builds,Times)
# Prints a table with the following rows
# Obs, Exp, Column %, ???, ???, Residual, Std. Residual, Adj Residual
ct <- CrossTable(data$Days, data$Builds, format="SPSS", expected=TRUE,resid=TRUE,sresid=TRUE,asresid=TRUE,chisq=TRUE)

# look for adj residuals of bigger than abs(adj residual) >= 2
# ok but what if we make sundays mostly Success?
data$Builds[data$Days=="Sun"] <- sample(c("SyntaxError","TestFailure","Success","Success","Success","Success"),
                                       sum(data$Days=="Sun"),replace=TRUE)

# ct <- CrossTable(data$Days, data$Builds,format="SPSS",resid=TRUE,sresid=TRUE,asresid=TRUE)
ct <- CrossTable(data$Days, data$Builds, format="SPSS", expected=TRUE,resid=TRUE,sresid=TRUE,asresid=TRUE,chisq=TRUE)


ct <- CrossTable(data$Times, data$Builds, format="SPSS", expected=TRUE,resid=TRUE,sresid=TRUE,asresid=TRUE,chisq=TRUE)
