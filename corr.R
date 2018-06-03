setwd("C:/drval/mstat/stepik")

df  <- mtcars
cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)
cor.test(~ mpg + hp, df)

plot(x = df$mpg, y = df$hp)

library(ggplot2)
ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)
  +facet_grid(. ~ am)
###########################################
df  <- mtcars
df_numeric  <- df[, c(1,3:7)]
pairs(df_numeric)
cor(df_numeric)
library(psych)
fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust
###########################################
df <- mtcars[, c(1,5)]
df[,1]
df[,2]
fit <- cor.test(~df[,1] + df[,2])
c(fit$estimate,fit$p.value)

corr.calc <- function(x){
  fit <- cor.test(~x[,1] + x[,2])
  as.vector(c(fit$estimate,fit$p.value))
}
corr.calc(iris[,1:2])

tdat <- read.csv('https://stepic.org/media/attachments/lesson/11504/step6.csv')
tdat_num <- tdat[,sapply(tdat, function(x) is.numeric(x))]
ct <- corr.test(tdat_num)
ctr <- ct$r
diag(ctr) <- 0
mx <- max(abs(ctr))
which.max(mx)

library(psych)
filtered.cor <- function(x){
  x_num <- x[,sapply(x, function(i) is.numeric(i))]
  cx <- corr.test(x_num)
  cxr <- cx$r
  diag(cxr) <- 0
  cxr[which.max(abs(cxr))]
}

iris$Petal.Length <- -iris$Petal.Length
filtered.cor(tdat)


smart_cor <- function(x){
  
}
