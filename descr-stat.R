?mtcars
df <- mtcars
str(df)
df$vs <- factor(df$vs, labels = c("V", "S"))
df$am <- factor(df$am, labels=c("Auto", "Manual"))
df$new_var <- NULL
df$even_gear <- NULL
mean(df$qsec[df$cyl != 3 & df$mpg > 20])

mean_hp_vs <- aggregate(x = df$hp, by=list(df$vs), FUN=mean)
colnames(mean_hp_vs) <- c('VS', 'Mean HP')
aggregate(hp ~ vs, df, mean)
#по двум переменным
aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)
aggregate(hp ~ vs + am, df, mean)

aggregate(x = df[,-c(8:9)], by=list(df$am), FUN = median)

aggregate(df[,c(1,3)], by=list(df$am, df$vs), FUN=sd)
aggregate(cbind(mpg,disp) ~ am + vs, df, sd)

aggregate(cbind(hp, disp) ~ am, df, sd)

library(psych)
?describe
describe(df[,-c(8,9)])
describeBy(x = df[,-c(8,9)], group = df$vs)
d2 <- describeBy(x = df[,-c(8,9)], group = df$vs, mat=T, digits = 1)
d3 <- describeBy(x = df[,-c(8,9)], group = df$vs, mat=T, digits = 1, fast=T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, fast=T, mat=T)



