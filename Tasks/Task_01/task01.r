# password for git via terminal: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 
install.packages("swirl")
library(swirl)
MykalDaniel
5 + 7
x <- 5 + 7
x
y <- x - 3
z <- c(1.1, 9, 3.14)
?c
z
c(z, 555, z)
z * 2 + 100
my_sqrt <- sqrt(z - 1)
my_sqrt
my_div <- z / my_sqrt
my_div
c(1, 2, 3, 4) + c(0, 10)
c(1, 2, 3, 4) + c(0, 10, 100)
my_div
library(swirl)
swirl()
1
2
getwd()
ls()
x <- 9
list.files()
?list.files
args()
args(list.files)
old.dir <- 1
old.dir <- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
file.exists("mytest.R")
file.info("mytest.R")
file.rename("mytest.R", "mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
file.path("folder1", "folder2")
?dir.create
dir.create(file.path("testdir2", "testdir3"), recursive = TRUE)
setwd(old.dir)
3
1:20
pi:10
15:1
?`:`
seq(1, 20)
seq(0, 10, by=0.5)
my_seq <- seq(5, 10, length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)
5
x <- c(44, NA, 5, NA)
x * 3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na <- is.na(my_data)
my_na
my_data == NA
sum(my_na)
my_data
0 / 0
Inf - Inf
6
x[1:10]
x[is.na(x)]
y <- x[!is.na(x)]
y[y > 0]
x[x > 0]
x[!is.na(x) & x > 0]
x[c(3, 5, 7)]
x[0]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]
vect <- c(foo = 11, bar = 2, norf = NA)
names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
identical(vect, vect2)
3
vect["bar"]
vect[c("foo", "bar")]
7
my_vector <- 1:20
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4, 5)
attributes(my_vector)
class(my_vector)
my_matrix <- my_vector
?matrix
my_matrix2 <- matrix(1:20, nrow=4, ncol=5)
identical(my_matrix, my_matrix2)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames
8
TRUE==TRUE
(FALSE == TRUE) == FALSE
6==7
6<7
10<=10
3
1
5!=7
!5 == 7
4
FALSE & FALSE
TRUE & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5 > 8 || 6!= 8 && 4 > 3.9
4
isTRUE(6 > 4)
5
identical('twins', 'twins')
2
xor(5 == 6, !FALSE)
ints <- sample(10)
ints
ints > 5
which(ints > 7)
any(ints < 0)
all(ints > 0)
Sys.Date()
mean(c(2, 4, 5))
submit()
boring_function('My first function!')
boring_function
my_mean(c(4, 5, 10))
remainder(5)
remainder(11, 5)
remainder(divisor = 11, num = 5)
remainder(4, div = 2)
args(remainder)
evaluate(sd, c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x[1]}, c(8, 4, 0))
evaluate(function(x){x[length(x)]}, c(8, 4, 0))
?paste
paste("Programming", "is", "fun!")
telegram()
mad_libs(adjective = "", place = "", noun = "")
'I' %p% 'love' %p% 'R!'
15
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x = cars$speed, y = cars$dist)
plot(x = cars$dist, y = cars$speed)
plot(speed ~ dist, cars)
plot(x = cars$speed, y = cars$dist, xlab = "Speed")
plot(x = cars$speed, y = cars$dist, ylab = "Stopping Distance")
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance")
plot(cars, main = "My Plot")
plot(cars, sub = "My Plot Subtitle")
plot(cars, col = 2)
plot(cars, xlim = c(10, 15))
plot(cars, pch = 2)
data(mtcars)
?boxplot
boxplot(mpg ~ cyl, mtcars)
hist(mtcars$mpg)