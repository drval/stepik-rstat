df <- read.csv('https://stepic.org/media/attachments/lesson/11502/grants.csv')
df$status <- as.factor(df$status)
levels(df$status) <- c('Not funded', 'funded')
#v.2
df$status <- factor(df$status, labels = c('Not funded', 'funded'))

t1 <- table(df$status)
dim(t1)
t2 <- table(df$status, df$field)
t2
t2 <- table(status=df$status, field=df$field)
prop.table(t2)
prop.table(t2, 1)
prop.table(t2, 2)

t3 <- table(Years= df$years_in_uni, field=df$field,status=df$status )
###############
dimnames(HairEyeColor)
# доля рыжеволосых (Red) от общего числа голубоглазых мужчин.
#v1
as.numeric( prop.table(HairEyeColor[,'Blue','Male'])['Red'])
#v2.
prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']

sum(HairEyeColor[,'Green', 'Female'])
