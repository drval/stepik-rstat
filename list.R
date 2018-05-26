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
