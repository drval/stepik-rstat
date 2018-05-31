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
