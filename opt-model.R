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
#############################################################
library(ggplot2)
ggplot(swiss, aes(Examination, Education))+
  geom_point()+geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)
swiss$Examination_squared <-  (swiss$Examination)^2
lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)
 
anova(lm2, lm1)

swiss$lm1_fitted <- lm1$fitted.values
swiss$lm2_fitted <- lm2$fitted.values

ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point(size=3)+
  geom_line(aes(x =Examination, y=lm1_fitted), col='red', lwd=1)+
  geom_line(aes(x =Examination, y=lm2_fitted), col='blue', lwd=1)

swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid

ggplot(swiss, aes(x=lm1_fitted, y=lm1_resid))+
  geom_point(size=1)+ 
  geom_hline(yintercept =0, col='red', lwd=1)

ggplot(swiss, aes(x=lm2_fitted, y=lm2_resid))+
  geom_point(size=1)+ 
  geom_hline(yintercept =0, col='red', lwd=1)

swiss$obs_number <- 1:nrow(swiss)
ggplot(swiss, aes(x = obs_number, y = lm1_resid))+
  geom_point(size=3)+geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid))+
  geom_point(size=3)+geom_smooth()


ggplot(swiss, aes(x=lm1_fitted, y=lm1_resid))+
  geom_point(size=3)

ggplot(swiss, aes(x=lm2_fitted, y=lm2_resid))+
  geom_point(size=3)


ds <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
install.packages('gvlma')
library(gvlma)
xds <- gvlma(DV~IV, ds)
summary(xds)

ggplot(swiss, aes(x=lm1_resid))+
  geom_histogram(binwidth = 4, fill='white', col='black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)

ggplot(swiss, aes(x=lm2_resid))+
  geom_histogram(binwidth = 4, fill='white', col='black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)


#############################################################
