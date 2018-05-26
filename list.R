age <- c(16, 18, 22, 27)
is_married <- c(F, F, T, T)
name <- c('Olga', 'Maria', 'Nastya', 'Polina')
ls <- list(age, is_married, name)
ls[[1]][1]
df <- data.frame(Name = name, Age = age, Status = is_married)
typeof(df)
