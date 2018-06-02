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
