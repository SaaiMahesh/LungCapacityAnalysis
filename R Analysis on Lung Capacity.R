library(ggplot2) #Loading the library for ggplot functions
library(rmarkdown) #Loading the library for R Markdown

data<-data.frame(read.csv('LungCap.csv')) #Creating a data frame and reading the dataset in csv format

View(data) #Viewing the data in a separate view tab

summary(data) #Summarizes the dataset for statistical information

#Basic plots
boxplot(data$LungCap.cc,data$Height.inches.,col=c('blue'),main='Boxplot for height and lung capacity',ylab='Lung Capacity in cc')
hist(data$Age..years.,main='Histogram for age',xlab='Age in years')
hist(data$Height.inches.,main='Histogram for height',xlab='Height in inches')
plot(data$Age..years.,data$LungCap.cc.,main='Scatterplot for age and lung capacity',pch=20,xlab='Age in years',ylab='Lung Capacity in cc')
ggplot(data,aes(x=Age..years.,y=Height.inches.))+geom_bar(stat='identity')+ylim(0,90)+labs(title='Barplot for age and height')+xlab('Age in years')+ylab('Height in inches')

#Hypothesis testing (at 95% confidence for p)

#One sample t test (difference between actual mean and specified mean)
mean(data$LungCap.cc.)
#H0: Mean of lung capacity is not equal to 8
#H1: Mean of lung capacity is equal to 8
t.test(data$LungCap.cc.,mu=8)
#Since p > 0.05, we accept null hypothesis for mean of lung capacity is not equal to 8 and reject alternative hypothesis.

#Welch two sample t test (independent test,1 numerical and 1 categorical up-to 2 levels)
#H0: Smoking has no affect on lung capacity
#H1: Smoking has an affect on lung capacity
t.test(data$LungCap.cc.~data$Smoke)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for smoking has an affect on lung capacity.

#Welch two sample t test
#H0: Height does not relate to smoking
#H1: Height relates to smoking
t.test(data$Height.inches.~data$Smoke)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for height relates to smoking.

#Welch two sample t test
#H0: Gender does not have an impact on lung capacity
#H1: Gender does have an impact on lung capacity
t.test(data$Height.inches.~data$Gender)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for gender does have an impact on lung capacity.

#Chi Square test (for 2 categorical variables)
#H0: Gender doesn't have an impact on habit of smoking
#H1: Gender has an impact on habit of smoking
chisq.test(data$Gender,data$Smoke)
#Since p > 0.05, we accept null hypothesis for gender doesn't have an impact on smoking reject alternative hypothesis.

#Chi Square test
#H0: Being born by cesarean does not have an impact on habit of smoking
#H1: Being born by cesarean has an impact on habit of smoking
chisq.test(data$Caesarean,data$Smoke)
#Since p > 0.05, we accept null hypothesis for being born by cesarean does not have an impact on habit of smoking and reject alternative hypothesis.

#Chi Square test
#H0: Age does not impact the habit of smoking
#H1: Age does impact the the habit of smoking
chisq.test(data$Age..years,data$Smoke)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for age does impact the the habit of smoking.

#Anova test (1 numerical and 1 categorical with more than 2 levels)
#H0: Age does not affect lung capacity
#H1: Age does affect lung capacity
a<-aov(data$LungCap.cc.~data$Age..years.)
summary(a)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for age does affect lung capactiy.

#Paired t test (dependent test)
#H0: Height does not affect lung capacity
#H1: Height does affect lung capacity
t.test(data$LungCap.cc.,data$Height.inches.,paired=TRUE)
#Since p < 0.05, we reject null hypothesis and accept alternative hypothesis for height does affect lung capacity.