setwd("C:/drval/mstat/stepik")

fit_full <- lm(Fertility ~ ., data=swiss)
summary(fit_full)

fit_re1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data=swiss)
summary(fit_re1)

fit_re2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data=swiss)
summary(fit_re2)

anova(fit_full, fit_re2)
#model selection
step(fit_full, direction = 'backward')

##################################
model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)

summary(model_full)
str(attitude)
opt <- step(object = model_full, direction='backward', scope=scope)
summary(opt)
a <- anova(opt, model_full)
a$F

LifeCycleSavings
summary(lm(sr ~ (pop15 + pop75 + dpi + ddpi)^2, data=LifeCycleSavings))
