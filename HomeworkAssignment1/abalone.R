#Read data
abalone<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",sep=',')
head(abalone)

#Rename variables
names(abalone)[1]="Sex"
names(abalone)[2]="Length"
names(abalone)[3]="Diameter"
names(abalone)[4]="Height"
names(abalone)[5]="Whole_wt"
names(abalone)[6]="Shucked_wt"
names(abalone)[7]="Viscera_wt"
names(abalone)[8]="Shell_wt"
names(abalone)[9]="Rings"

#Explore data
plot(abalone)
plot(Rings ~ Diameter,data=abalone)
abline(lm(Rings ~ Diameter,data=abalone))

#Develop model
lm1=lm(Rings~Diameter, data=abalone)
summary(lm1)
lm2=lm(Rings ~ Diameter + Shucked_wt, data=abalone)
summary(lm2)
lm3=update(lm2,.~. + Whole_wt, data=abalone)
summary(lm3)
plot(lm3)
#While the p-value for the three variables are significant, and the F-statistic increased with addition of each variable, the R^2 value of 0.5 means that the model only explains half of the variance in the data set.
#The residuals plot appears to have structure, and the qqplot is not a straight line, both of which suggest the model is not a good fit.


#Explore ridge regression
library(MASS)
select(lm.ridge(lm3,lambda=seq(0,1,0.001),data=abalone))
abalone.ridge.reg=lm.ridge(lm3, lambda=0.116, data=abalone)
abalone.ridge.reg
#Not sure how to interpret


