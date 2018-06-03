setwd("C:/drval/mstat/stepik")

v <- c(1,NA,3,NA)
is.na(v)
length(which(is.na(v)))


NA.position <- function(x){
  # put your code here  
  return (length(which(is.na(x))))
}

filtered.sum <- function(x){
  sum(x[!is.na(x) & x > 0])
}

v1 <- c(-1, 2, NA, 3, -6)
sum(v1[!is.na(v1) & v1>0])
filtered.sum(v1)


v2 <- c(3, 5, 4, 2, 6, 11, 20, -20)
v2_3 <- quantile(v2, probs = c(0.75))
v2_1 <- quantile(v2, probs = c(0.25))
irq <- IQR(v2)*1.5
v2[!(v2 - v2_3 > irq | v2_1 - v2 > irq)]
v2[v2 > IQR(v2)*1.5]
boxplot(v2)
IQR(v2)
quantile(v2, probs = c(0.75))

outliers.rm <- function(x){
  x_3 <- quantile(x, probs = c(0.75))
  x_1 <- quantile(x, probs = c(0.25))
  irq <- IQR(x)*1.5
  x[!(x - x_3 > irq | x_1 - x > irq)]
}
outliers.rm(v2)



