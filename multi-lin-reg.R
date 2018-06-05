setwd("C:/drval/mstat/stepik")

hist(swiss$Fertility, col = 'red')

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2)
###############################
df <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

fit <- lm(y ~ x_1 + x_2, subset(df, !is.na(y)))
df$full_y <- ifelse(is.na(df$y), predict(fit, df), df$y)
  
fill_na <- function(x){
  fit <- lm(y ~ x_1 + x_2, subset(x, !is.na(y)))
  x$y_full <- ifelse(is.na(x$y), predict(fit, x), x$y)
  x
}
fill_na(df)

mtcars
df <- mtcars[c('wt','mpg', 'disp', 'drat', 'hp')]
cv <- c('mpg', 'disp', 'drat', 'hp')


#формирование комбинаций
fms <- paste('wt ~ ', unlist(lapply(sapply(1:length(cv), function(i) combn(cv,i)), function(j) {
  s <- vector()
  for (k in 1:length(j[1,])) {
    s <- c(s, paste(j[,k], collapse = '+'))
  }
  s
})))

#список моделей и R^2 adjusted
models <- lapply(fms, FUN=function(i) {
  fit <- lm(i, df)
  c(fitt=summary(fit), sq=summary(fit)$adj.r.squared)
})

#получение модели с наиб. R^2 adjusted
models[[which.max(unlist(lapply(models, function(x) x$sq)))]]

