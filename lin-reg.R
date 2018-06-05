setwd("C:/drval/mstat/stepik")

df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)

library(ggplot2)
ggplot(df, aes(hp, mpg, col=cyl))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

#предсказанные значения
fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

#предсказания по модели
new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)
summary(fit)


df <- data.frame(read.table('dataset_11508_12.txt', header = F, sep = ' '))
df
sm <- summary(lm(V1 ~ V2, df))
sm$coefficients

