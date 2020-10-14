# When do you use KruskalWallace?
# - When you want to know the effect of a label or factor on a result.
# You cannot do a correlation of label and a value, so KruskalWallace is here to help.
# - It studies the effect of a factor on a dependent variable
# - It can show if factors matter.
# Common setups:
#            - Data: projects and defects
#            - labels: Java, Python, C
#            - dependent variable: defects
#            - kruskal.test( defects ~ language )
#              - this asks do the different languages have different defect distributions.
#            

kruskalWallaceExample <- function(N) {
    # so < 10% failure rate
    lowfreqratios <- runif(N) * 0.1
    # so < 12% failure rate
    mediumfreqratios <- runif(N) * 0.12
    # so < 15% failure rate
    highfreqratios <- runif(N) * 0.15
    highestfreqratios <- runif(N) * 0.20
    # concat the ratios
    ratios <- c(lowfreqratios, mediumfreqratios, highfreqratios, highestfreqratios)
    # give them a factor as a label (LOW MEDIUM HIGH)
    labels <- as.factor(c(rep("LOW",N), rep("MEDIUM",N), rep("HIGH",N),rep("HIGHEST",N)))
    # meaningless labels that are purely randomly assigned
    meaninglesslabels <- sample(labels, 4 * N, replace=TRUE)
    print(N)    
    # Kruskal Wallace, do the factors of labels significantly affect ratios?
    print(kruskal.test( ratios ~ labels ))
    print(kruskal.test( ratios ~ meaninglesslabels ))
    # If the Kruskal Wallace test says significant you could run these pairwise tests
    print(pairwise.wilcox.test(ratios, labels))
    print(pairwise.wilcox.test(ratios, meaninglesslabels))
}
kruskalWallaceExample(100)
sapply(c(10,20,50,100,500,1000,10000), kruskalWallaceExample)

# should they have binned continuous data?
dontBinExample <- function(N) {
    # so < 10% failure rate
    lowfreqratios <- runif(N) * 0.1
    # so < 12% failure rate
    mediumfreqratios <- runif(N) * 0.12
    # so < 15% failure rate
    highfreqratios <- runif(N) * 0.15
    # concat the ratios
    ratios <- c(lowfreqratios, mediumfreqratios, highfreqratios)
    # give them a factor as a label (LOW MEDIUM HIGH)
    labels <- as.factor(c(rep("LOW",N), rep("MEDIUM",N), rep("HIGH",N)))
    counts <- c(round(1 + runif(N) * 10), round(10 + runif(N) * 50), round(50+runif(N) * 100))
    print(N)
    print(summary(lm(ratios ~ counts)))
    print(cor(ratios,counts))
    print(cor(ratios,counts,method="spearman"))
}

sapply(c(10,20,50,100,500,1000,10000), dontBinExample)
