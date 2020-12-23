
# Analysis of variance (ANOVA) is a collection of statistical models and their associated 
# estimation procedures (such as the "variation" among and between groups) used to analyze the differences 
# among group means in a sample. ANOVA was developed by statistician and evolutionary biologist 
# Ronald Fisher. The ANOVA is based on the law of total variance, where the observed variance in 
# a particular variable is partitioned into components attributable to different sources of 
# variation. In its simplest form, ANOVA provides a statistical test of whether two or more 
# population means are equal, and therefore generalizes the t-test beyond two means.


#Packages

#Data
data(InsectSprays) 
attach(InsectSprays)

#visualisation 
head(InsectSprays)
tapply(count, spray, mean) 
tapply(count, spray, var)
tapply(count, spray, length)
summary(InsectSprays)
plot(spray,count,col=rainbow(6))

#Anova
# transformer la variable  X^2  because X don't have a normal distribution 
shapiro.test(count)      # p < 0.05
hist(count, xlim=c(0,30), col=rgb(0,1,0,0.5),density = 30,probability = TRUE)
lines(density(count),col=2) 

new.count <- sqrt(count)
tapply(new.count, spray, mean) 
tapply(new.count, spray, var)
plot(spray,new.count)
par(mfrow=c(1,2))
hist(count, xlim=c(0,30), col=rgb(0,1,0,0.5),density = 30,probability = TRUE)
lines(density(count),col=2) 
hist(new.count, xlim=c(0,6), col=rgb(1,0,0,0.5),density = 30, probability = TRUE)
lines(density(new.count),col=2) 
shapiro.test(new.count)      # p > 0.05


# normal Q-Q plot 
par(mfrow=c(1,2))
qqnorm(count,main="QQ plof From X")
qqline(count,col="red")

qqnorm(new.count,main="QQ plof From X^2")
qqline(new.count,col="red")

#Model avona
modele <- aov(new.count ~ spray) 
coef(modele)
anova(modele)
summary(modele)
TukeyHSD(modele) 
plot(modele)
