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

#barplot
barplot(t2)
barplot(t2, beside = T, legend.text = T, args.legend = list(x='topright'))

mosaicplot(t2)


library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
fdata <- mydata[mydata$Sex=='Female',]

plot <- ggplot(data = fdata, aes(x = Hair, y=Freq, fill=Eye))
plot + geom_bar(stat='identity', position = 'dodge') + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))


s <- ggplot(mpg, aes(fl, fill = drv))
s + geom_bar(position = "dodge")

g <- ggplot(mpg, aes(class, hwy))
g + geom_bar(position = "dodge")




library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = mydata[mydata$Sex=='Female',], aes(x = Hair, y=Freq, fill=Eye)) + geom_bar(stat='identity', position = 'dodge') + scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

#binom test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)
t1
#Chi-square
chi <- chisq.test(t1)
chi$expected
chi$observed

t2
chisq.test(t2)
#fisher exact test
fisher.test(t2)

dimnames( HairEyeColor)
chisq.test(HairEyeColor['Brown',,'Female'])

str(diamonds)
d2 <- diamonds[,c('cut', 'color')]
tst <- chisq.test(d2$cut, d2$color)
as.vector(tst$statistic)
v <- as.vector( chisq.test(d2$cut, d2$color)$statistic)
