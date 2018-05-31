setwd("C:/drval/mstat/stepik")

df <- iris
str(df)
df1 <- subset(df, Species != 'setosa')
hist(df1$Sepal.Length)

ggplot(df1, aes(x= Sepal.Length)) + 
  geom_histogram(fill='white', color='black', binwidth = 0.4)+
  facet_grid(Species ~.)

ggplot(df1, aes(Sepal.Length, fill= Species))+
  geom_density(alpha=0.4)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

#тест на соотв. нормальности распределения
shapiro.test(df1$Sepal.Length)
shapiro.test(df1$Sepal.Length[df1$Species == 'versicolor'])
shapiro.test(df1$Sepal.Length[df1$Species == 'virginica'])

#тест на гомогенность дисперсии
bartlett.test(Sepal.Length ~ Species, df1)

#тест проверки равенства средних
t.test(Sepal.Length ~ Species, df1)

mean(df1$Sepal.Length)
#тест на равенство средних
t.test(df1$Sepal.Length, mu =5.8)
#тест на зависимость 2-ух перем.
t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test) # проверка на нормальность переменной 

#v.1
m1 <- ToothGrowth$len[ToothGrowth$supp=='OJ' & ToothGrowth$dose==0.5]
m2 <- ToothGrowth$len[ToothGrowth$supp=='VC' & ToothGrowth$dose==2]
t.test(m1,m2)$stat
#v.2
m3 <- subset(ToothGrowth, supp=='OJ' & dose==0.5 | supp=='VC' & dose==2)
t.test(len ~ supp, m3)$stat

lekdata <- read.csv('https://stepic.org/media/attachments/lesson/11504/lekarstva.csv')
str(lekdata)

t.test(lekdata$Pressure_before, lekdata$Pressure_after, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='errorbar', width=0.1)+
  stat_summary(fun.y = mean, geom='point', size=2)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom='pointrange', size=1)


wilcox.test(Sepal.Length ~ Species, df1)
ggplot(df1, aes(Species, Sepal.Length))+ 
  geom_boxplot()



