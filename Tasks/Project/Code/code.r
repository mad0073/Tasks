setwd('/Users/mykaldaniel/Desktop/Evolution/Tasks/Project/Data')
Dat <- read.csv("Tyrannidae Family Hypothesis.csv")
library("phytools")
trees <- read.nexus("Tyrannidae Family Trees")
### need: pearson correlation coefficient & scatter plot wing vs tail

# make data match
Dat[,1] <- gsub(" ", "\\_", Dat[,1])
NotInTree <- setdiff(Dat[,1], trees[[1]]$tip.label)
NotInData <- setdiff(trees[[1]]$tip.label, Dat[,1])
InBoth <- intersect(Dat[,1], trees[[1]]$tip.label)

Phy <- drop.tip(trees[[1]], NotInData)
Dat2 <- Dat
rownames(Dat2) <- Dat[,1]
Dat2 <- Dat2[InBoth,]
Dat2 <- Dat2[,-1]
Dat3 <- as.matrix(Dat2)

Obj <- phyl.vcv(Dat3, vcv(Phy), 1)
PearsonR <- cov2cor(Obj$R)[1,2]

phylomorphospace(Phy, Dat3, label="off")