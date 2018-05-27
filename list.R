age <- c(16, 18, 22, 27)
is_married <- c(F, F, T, T)
name <- c('Olga', 'Maria', 'Nastya', 'Polina')
ls <- list(age, is_married, name)
ls[[1]][1]
df <- data.frame(Name = name, Age = age, Status = is_married)
typeof(df)

#****************************
v <- c(10, 20, 30, 6, 45, 11, 54, 87, 33)
v[abs(v - mean(v)) < sd(v)]
mean(v)
sd(v)
#read.csv('https://stepik.org/media/attachments/lesson/11481/evals.csv')
#****************************
mtcars$even_gear <- as.integer(mtcars$gear %% 2 == 0)
cbind(mtcars$gear, mtcars$even_gear)

mpg_4 <- mtcars[mtcars$cyl == 4,1]
mtcars[c(3,7,10,12, nrow(mtcars)),]
#****************************
mtcars$new_var <-  ifelse(mtcars$carb >=4 | mtcars$cyl > 6, 1, 0)
mtcars
#*****************************
?AirPassengers 
vp1 <- as.vector(AirPassengers)
vp2 <- c(as.vector(AirPassengers)[2:length(vp1)],0)
t <- data.frame(vp1, vp2)
good_month <- t[t$vp2 - t$vp1 > 0, 2]


good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 
AirPassengers[-(1:0)]
cumsum(AirPassengers)[10]
cumsum(AirPassengers[-(1:30)])[10]

means <- numeric(135)
for(i in 1:(length(AirPassengers)-9))
  means[i] <- mean(AirPassengers[i:(9+i)])

