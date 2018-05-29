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

str(airquality)


#result <- length(aq_789$Ozone[is.na(aq_789$Ozone)==0])
aq_789 <-  
result <- aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length)

x <- describeBy(airquality[,-c(5,6)], group = airquality$Month, mat=T)
x[x$group1 == 8,][c('Wind4'),'skew']

describe(iris[iris$Species == 'virginica',])


my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
mean(my_vector[is.na(my_vector)==0])

replace(my_vector, which(is.na(my_vector)),mean(my_vector[is.na(my_vector)==0]))

#графики
#гистограмма
hist(df$mpg, breaks = 5, xlab = "MPG")
hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)
plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)
#боксплот
boxplot(mpg ~ am, df, ylab="MPG")
boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)
boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)
#scatterplot
plot(df$mpg, df$hp)
plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)
plot(~ mpg + hp, df) 

#ggplot2
library(ggplot2)

ggplot(df, aes(x = mpg))+ 
  geom_histogram(fill="white", col="black", binwidth = 3)

ggplot(df, aes(x = mpg, fill=am))+ 
  geom_dotplot(binwidth = 1)

ggplot(df, aes(x = mpg))+ 
  geom_density(fill = 'red')

ggplot(df, aes(x = mpg, fill=am))+ 
  geom_density(alpha = 0.5)

ggplot(df, aes(x = am, y = hp, col=vs))+
  geom_boxplot()

ggplot(df, aes(x = mpg, y = hp, col=vs, size=qsec))+
  geom_point()

