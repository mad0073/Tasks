library(learnPopGen)
coalescent.plot()
?coalescent.plot
coalescent.plot(n=2, ngen=10, col.order="alternating")
coalescent.plot(n=4, ngen=8, col.order="alternating")
coalescent.plot(n=5, ngen=5, col.order="alternating")
Question 1: Each simulation began with 2, 4, and 5 alleles which I chose by setting the n function for number of haploid individuals or gene copies.
Question 2: The average number of generations it takes for one allele to go to fixation is 3.
Question 3: The average number of offspring is 0.818182 and the variance is 0.81713689.
Question 4: The role of fitness contributes to the number of offspring each haploid individual has.
Question 5: Yes
install.packages("coala")
library(coala)
install.packages("rehh", dep=T)
Yes
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos = NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos = NULL, type="source")
install.packages("phytools")
library(phytools)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + feat_mutation(10) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()
model
stats <- simulate(model, nsim = 1)
stats
Diversity <- stats$pi
Diversity
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
Question 6: The number of individuals does not match because they reproduced.
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees) {
    if(locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1, 1))
densityTree(outPhy)
model3 <- coal_model(10, 50) + feat_mutation(par_prior("theta", sample.int(100, 1))) + sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi, theta)
Line <- lm(mean_pi ~ theta)
abline(Line)
