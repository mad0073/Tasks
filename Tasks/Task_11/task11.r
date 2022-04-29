setwd('/Users/mykaldaniel/Desktop/Evolution/Tasks/Task_11')
##1
x <- rnorm(100, mean = 5, sd = 2)
y <- (x * 5) +2 + runif(100, min = 0, max = 0.1)
plot(x,y)
abline(lm(y ~ x))
lm(y ~ x)
## The y-intercept = 2.055 and the x = 5. Slope is 5 because we chose a mean of 5 and the y-intercept is 2.055 because the variance was not exactly 2.
x <- c()
y <- c()
z <- c()
edit <- c()
slope <- c()
for (i in 1:100) {
  x[i] <- rnorm(100, mean = 5, sd = 2)
  z[i] <- rnorm(1)
  edit[i] <- x * z
  y[i] <- (edit *5) + 2 + runif(100, min = 0, max = 0.1)
  IntX <- lm(y ~ x)
  a <- coef(IntX)
  slope[i] <- a["x"]
}
plot(z, slope)
abline(lm(slope ~ z))
## The plot showed a zero slope between z and the estimated slope.


##2

library(dplyr)
library(ggplot2)
doors <- 1:3
sample_doors <- function() { return(sample(doors, size = 10000, replace = TRUE))}
games <- data.frame(prize = sample_doors(), pick = sample_doors())
games$strategy <- factor(ifelse(games$prize == games$pick, 'stick', 'switch'))
monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))
  return(ifelse(length(remaining)==1,
                remaining,
                sample(remaining, 1)))
}
games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick),
         stay = pick,
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stick', 'switch')))
print(summary(games$strategy) / nrow(games))
qplot(strategy, data = games, geom = 'bar') + 
  xlab('Strategy') +
  ggtitle('Monty Hall Problem')

##3
install.packages("meme")
library(meme)
meme("https://media.npr.org/assets/img/2016/03/29/ap_090911089838_sq-3271237f28995f6530d9634ff27228cae88e3440-s1200-c85.webp", "RStudio: Error in...", size = 4, color = "white")
