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


my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 
               0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 
               0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 
               0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 
               0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

shapiro.test(log(my_vector))
hist(log(my_vector))

m <- scale(swiss[,c(1)])
m2 <- scale(swiss[,c(4)])
df <- data.frame(m,m2)
model <- summary(lm(m ~ m2, data = df))
cf <- model$coefficients
cf[1,1]
cf[2,1]


beta.coef <- function(x){
  c1 <- scale(x[,c(1)])
  c2 <- scale(x[,c(2)])
  df <- data.frame(c1,c2)
  model <- summary(lm(c1 ~ c2, data = df))
  cf <- model$coefficients
  c(cf[1,1],cf[2,1])
}

beta.coef(swiss[,c(1,4)])


unlist(lapply(iris[,-5], function(v) shapiro.test(v)$p.value ))

normality.test  <- function(x){
  unlist(lapply(x, function(v) shapiro.test(v)$p.value ))
}
normality.test(iris[,-5])
