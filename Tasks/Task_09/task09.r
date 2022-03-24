setwd('/Users/mykaldaniel/Desktop/Evolution/Tasks/Task_09')
library("phytools")
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label
tree$edge.length
##Q1: There are 82 tips and branch lengths are present.
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)
data
typeof(data)
dim(data)
##Q2: This object is a list of 82 rows and columns which are species names and svl. SVL stands for "snout-vent length".
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars = TRUE, CI=TRUE)
Ancestors
?fastAnc
##Q3: They are stored under "ace" which is a list containing ancestral state estimates. The CI95 element is a 95-percent confidence interval.
##Q4: The estimation of the ancestral states using fastAnc assumes the correlation struture among trait values is proportional to the extent of shared ancestry for pairs of species, and that this is also the Maximum Likelihood Estimation of the root node.
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(tree, type="fan", lwd=2, show.tip.label = F)
tiplabels(pch = 16, cex=0.25*svl[tree$tip.label])
nodelabels(pch = 16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
##Q5
fossilNodes <- c()
nodeN <- c()
for(i in 1:nrow(fossilData)) {
  Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes[i] <- fossilData[i, "svl"]
  nodeN[i] <- Node
}
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
plot(Ancestors$ace, Ancestors_withFossils$ace, xlab = "without fossils", ylab = "with fossils", pch=1, cex=1)
##Q7: Data with fossils increases the estimated ancestral size.
install.packages("geiger")
library("geiger")
?fitContinuous
##Q8:
fitContinuous(tree, svl, model="BM")
##AIC = -6.512019
fitContinuous(tree, svl, model="OU")
##AIC = -4.512022
fitContinuous(tree, svl, model="EB")
##AIC = -7.235181
fitContinuous(tree, svl, model="rate_trend")
##AIC = -6.981431
fitContinuous(tree, svl, model="lambda")
##AIC = -4.512019
fitContinuous(tree, svl, model="kappa")
##AIC = -4.512019
fitContinuous(tree, svl, model="delta")
##AIC = -6.106546
fitContinuous(tree, svl, model="mean_trend")
##AIC = -4.526957
fitContinuous(tree, svl, model="white")
AIC = 91.391102
##Q9: Model "EB" is the best as lower AIC means a better fit model.
##Q10: The "EB" model was the best fit model where the rate of evolution increases or decreases exponentially through time. However, fastAnc uses the "BM" model which assumes the correlation among trait values is proportional to the extent of shared ancestry for pairs of species.
