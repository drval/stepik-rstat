#https://stepic.org/media/attachments/lesson/11505/shops.csv�
#https://stepic.org/media/attachments/lesson/11505/therapy_data.csv�
setwd("C:/drval/mstat/stepik")

shopdata <- read.csv('https://stepic.org/media/attachments/lesson/11505/shops.csv')
#one-way ANOVA
boxplot(price ~ origin, data=shopdata)
library(ggplot2)
ggplot(shopdata, aes(x=origin, y=price))+
  geom_boxplot()

fit <- aov(price ~ origin, shopdata)
summary(fit)

# Two-way ANOVA
fit2w <- aov(price ~ origin + store, data=shopdata)
summary(fit2w)

model.tables(fit2w,"means")

pd = position_dodge(0.1)
ggplot(shopdata, aes(x = store, y = price, color = origin, group = origin)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.2, lwd = 0.8, position = pd)+  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size = 1.5, position = pd) +
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 5, position = pd, pch=15) +
  theme_bw()


fit3 <- aov(price ~ origin + store + origin:store, data=shopdata)
summary(fit3)

fit4 <- aov(price ~ origin * store, data=shopdata)
summary(fit4)

########################
npk
fit_npk <- aov(yield ~ N * P, data=npk)
summary(fit_npk)


fit_npk3 <- aov(yield ~ (N + P + K), data=npk)
summary(fit_npk3)

#Pairwise comparisonss
ggplot(shopdata, aes(x = food, y = price))+
  geom_boxplot()

fitprice <- aov(price ~ food, data=shopdata)
summary(fitprice)

TukeyHSD(fitprice)
#####################
ggplot(iris, aes(x = Species, y = Sepal.Width))+
  geom_boxplot()
fit_iris <- aov(Sepal.Width ~ Species, data=iris)  
summary(fit_iris)
TukeyHSD(fit_iris)
#####################
tedata <- read.csv('https://stepic.org/media/attachments/lesson/11505/therapy_data.csv')
str(tedata)
tedata$subject <- as.factor(tedata$subject)

#модель самочувствие ~ терапия, без учета повт. испытаний
f1 <- aov(well_being ~ therapy, data=tedata)
summary(f1)

#модель самочувствие ~ терапия, с учетом повт. испытаний
f2 <- aov(well_being ~ therapy + Error(subject/therapy), data = tedata)
summary(f2)

#модель самочувствие ~ терапия*цена, без учета повт. испытаний
f11 <- aov(well_being ~ therapy*price, data= tedata)
summary(f11)

ggplot(data= tedata, aes(x = price, y= well_being))+
  geom_boxplot()

#модель самочувствие ~ терапия*цена, с учетом повт. испытаний
f22 <- aov(well_being ~ therapy*price + Error(subject/(therapy*price)), data = tedata)
summary(f22)           

ggplot(data= tedata, aes(x = price, y= well_being))+
  geom_boxplot()+
  facet_grid(~subject)

f3 <- aov(well_being ~ therapy*price*sex, data=tedata)
summary(f3)

f32 <- aov(well_being ~ therapy*price*sex + Error(subject/(therapy*price)), data=tedata)
summary(f32)

###################
pdata <- read.csv('https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv')
str(pdata)
pdata$patient <- as.factor(pdata$patient)
pm <- aov(temperature ~ pill + Error(patient/pill), data=pdata)
summary(pm)

ggplot(data= pdata, aes(x = pill, y= temperature))+
  geom_boxplot()+
  facet_grid(~patient)

pm2 <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data=pdata)
summary(pm2)           


library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col=supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
obj
