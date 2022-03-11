text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width = 2)
nodelabels(frame= "circle", bg='white', cex=1)
#Q1: Shark
vert.tree
#Q2: No, it is rooted
str(vert.tree)
tree <- read.tree(text="(((A, B), (C, D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col = 'black', border='white', main = "", xlab = "edge lengths for the Anolis Tree", ylim = c(0, 50), xlim = c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text = Labs, cex=0.25)
?plot.phylo
#Q3: plot(AnolisTree, cex=0.25, show.tip.label=FALSE)
#Q4: plot(AnolisTree, cex=0.25, type = "radial")
#Q5: plot(AnolisTree, cex=0.25, tip.color = "red")
ShortestEdge <- min(Lengths)
#Q6: which.min(Lengths)
ShortestEdge <- which.min(Lengths)
#Q7: AnolisTree2 <- drop.tip(AnolisTree, tip = ShortestEdge)
#Q8: plot(AnolisTree2, cex = 0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#Q9: The line is increasing and does not go down. This could indicate the lizards are reaching fixation. The slope is always the same which could indicate fixation approaching.
fit.bd(AnolisTree, b=AnolisTree, d=AnolisTree, rho = 0.2)
AnolisFit <- fit.bd(AnolisTree, b=AnolisTree, d=AnolisTree, rho = 0.2)
str(AnolisFit)
#Q10: b=0.8031 d =0 log(L)=132.9163


#Extra Credit


install.packages("treebase")
Dolphins <- search_treebase('Delphinus', by='taxon')

