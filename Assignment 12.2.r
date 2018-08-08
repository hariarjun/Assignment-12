#Question 1. Use the given link Data Set.
#Answer the below questions:
#  a. What are the assumptions of ANOVA, test it out?
yeastprotien <- read_excel("D:/hary/Dataanalytics/dataset/yeastprotien.xlsx")
View(yeastprotien)
str(yeastprotien)
table(yeastprotien$class)
nrow(yeastprotien)
tail(yeastprotien)
yeastprotien <- yeastprotien[-c(1485),]
tail(yeastprotien)
nrow(yeastprotien)
table(yeastprotien$class)
sum(is.na(yeastprotien))
sum(is.na(yeastprotien))
library(dplyr)
library(sqldf)
library(ggplot2)
classnuc <- yeastprotien %>% select(nuc,class)
head(classnuc)
str(classnuc)
class(classnuc)
ggplot(yeastprotien,aes(class,nuc)) + geom_boxplot()
result <- aov(nuc~class,data=classnuc);
summary(result)
library(Hmisc)
TukeyHSD(result)
plot(result,1)
library(car)
leveneTest(nuc ~ class, data = classnuc)
oneway.test(nuc ~ class, data = classnuc)
plot(results,2)
plot(result,2)
residuals <- residuals(object = result)
shapiro.test(x = residuals)
kruskal.test(nuc~class,data=classnuc)
str(classnuc)
classnuc$class <- as.factor(classnuc$class)
kruskal.test(nuc~class,data=classnuc)

#  b. Why ANOVA test? Is there any other way to answer the above question?

# An ANOVA test is a way to find out if survey or experiment results are significant.
# In other words, they help you to figure out if you need to reject the null hypothesis or accept the alternate hypothesis. 
# Basically, you're testing groups to see if there's a difference between them

#The Other Ways Are 1.	pair wise t-test 2.One-Way Welch Test etc

