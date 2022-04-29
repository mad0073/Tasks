setwd('/Users/mykaldaniel/Desktop/Evolution/Tasks/Task_10')
library("phytools")

##Q1-3
trees <- vector(mode = "list", length = 1)
births <- c()
Fractions <- c()
ABL <- c()
?pbtree
for (i in 1:100) {
  births[i] <- runif(1)
  Fractions[i] <- runif(1)
  trees[[i]] <- pbtree(b=births[i], d=births[i]*Fractions[i], n=100)
  ABL[[i]] <- mean(trees[[i]]$edge.length)
} 
plot(trees[[i]])

##Q4
TipLog <- log(sapply(trees, Ntip))
BRate <- births
DRate <- births * Fractions
NetDiv <- BRate - DRate
plot(NetDiv, TipLog, xlab = "net diversification rate", ylab = "tips")
abline(lm(TipLog ~ NetDiv))
cor(NetDiv, TipLog)
## There is a positive correlation of 0.1847034 between the net diversification rate and the log of the total number of tips.

##Q5-6
ABL <- unlist(ABL)
plot(BRate, ABL, xlab = "speciation rate", ylab = "average branch length")
abline(lm(ABL ~ BRate))
cor(BRate, ABL)
## There is a negative correlation of -0.329063 between the Speciation Rate and the Average Branch Length.

##Q7
tips <- sapply(trees, Ntip)
which.max(tips)
LargeTree <- trees[[which.max(tips)]]
plot(LargeTree, type = "radial")
rates <- c()
MeanTraits <- c()
VarTraits <- c()
traits <- vector(mode = "list", length = 1)
for (i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(tree = LargeTree, sig2 = rates[i])
  MeanTraits[[i]] <- mean(traits[[i]])
  VarTraits[[i]] <- var(traits[[i]])
}

##Q8
MeanTraits <- unlist(MeanTraits)
plot(MeanTraits, rates, xlab = "mean of traits", ylab = "rates")
abline(lm(rates ~ MeanTraits))
cor(MeanTraits, rates)
## The correlation between the Mean of Traits and Rates is 0.09398436.

##Q9
VarTraits <- unlist(VarTraits)
plot(VarTraits, rates, xlab = "variance of traits", ylab = "rates")
abline(lm(rates ~ VarTraits))
cor(VarTraits, rates)
## The correlation between the Variance of Traits and Rates is 0.5895803.

##Q10
cor(traits[[1]], traits[[2]])
##The correlation between Trait 1 and Trait 2 is 0.1207645 which is not significant. A strong correlation usually lies with values 0.75 and higher.
plot(traits[[1]], traits[[2]], xlab = "first element of traits", ylab = "second element of traits")
traitMat <- cbind(traits[[1]], traits[[2]])
traitMat

##Extra Credit
?phylomorphospace
phylomorphospace(LargeTree, traitMat, xlab = "first element of traits", ylab ="second element of traits")
