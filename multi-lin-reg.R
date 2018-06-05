setwd("C:/drval/mstat/stepik")

hist(swiss$Fertility, col = 'red')

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2)
