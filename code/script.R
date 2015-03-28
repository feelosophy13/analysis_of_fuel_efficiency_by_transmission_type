# You work for Motor Trend, a magazine about the automobile industry. 
# Looking at a data set of a collection of cars, they are interested in exploring the 
# relationship between a set of variables and miles per gallon (MPG) (outcome). 
# They are particularly interested in the following two questions:
# 1. "Is an automatic or manual transmission better for MPG"
# 2. "Quantify the MPG difference between automatic and manual transmissions"



#### initial setup
rm(list = ls())
getwd()
setwd('/Users/hawooksong/Desktop/analysis_of_fuel_efficiency_by_transmission_type')
dir()



#### import libraries
library(ggplot2)
library(psych)  # for describeBy()
library(car)  # for leveneTest()
library(corrplot)  # for corrplot()
library(gridExtra)  # for grid.arrange()



#### quick peak at the data 
?mtcars
mtc <- mtcars
head(mtc)
dim(mtc)



#### transmission type column human-readability
mtc$am <- factor(ifelse(mtc$am==0, 'auto', 'manual'))
# am 0: automatic transmission
# am 1: manual transmission
str(mtcars)




#### quick summary statistics
summary(mtc)
describeBy(mtc, group=mtc$am)
sapply(mtc[ , -9], function(x) cor(mtc$mpg, x))

## split data by transmission type: auto and manual
auto <- subset(mtc, am=='auto')
manual <- subset(mtc, am=='manual')

## correlation with MPG by transmission type
suppressWarnings(sapply(auto[ , -9], function(x) cor(auto$mpg, x)))
suppressWarnings(sapply(manual[ , -9], function(x) cor(manual$mpg, x)))



#### exploratory analysis
## correlation between variables
corMatrix <- round(cor(mtc[ , -9]), 2)
corrplot(corMatrix, method = 'ellipse', type='lower')
# dev.copy(png, 'corrplot.png')
# dev.off()

corMatrix[upper.tri(corMatrix)] <- NA
corMatrix



#### mpg comparison between auto- and manual-transmission cars
tapply(mtc$mpg, mtc$am, mean)  
# cars with manual transmission have higher mpg (in this dataset)

## box plot of MPG by transmission type
j <- ggplot(mtc) + 
  geom_boxplot(aes(x=am, y=mpg, fill=am)) + 
  guides(fill = guide_legend(title='transmission\ntype')) + 
  ggtitle('Box Plot of MPG by Transmission Type')
# cars with manual transmission have higher mpg (in this dataset)

## density plot of MPG by transmission type
k <- ggplot(mtc, aes(x=mpg)) + 
  geom_density(aes(group=am, color=am, fill=am), alpha=0.3) + 
  ggtitle('Density Plot of MPG by Transmission Type')
# cars with manual transmission have higher mpg (in this dataset)

grid.arrange(j, k)
# dev.copy(png, 'box_plot_and_density_plot_of_mpg_by_transmission_type.png')
# dev.off()



#### is the difference in MPGs between the two samples stat. sig.?
## check for homogeneity of variance between the two samples through Levene's test

suppressWarnings(leveneTest(mtc$mpg ~ mtc$am))  
# homogeneity of variance not assumed; cannot use independent t-test; resort to Mann-Whitney U test

## Mann-Whitney U test (non-parametric)
wilcox.test(auto$mpg, manual$mpg, paired = FALSE, exact = FALSE)  
# yes, the difference in MPG between the two samples is stat. sig.



#### are there any confounders?

## average values by transmission type
mean_auto <- sapply(auto[ , -9], mean)
mean_manual <- sapply(manual[ , -9], mean)
rbind(mean_auto, mean_manual)


# on average, cars with auto transmission have:
# - higher number of cylinders
# - larger displacement
# - more horsepower
# - heavier weight
#
# all of which could be confounders

## box plots and density plots of potential confounders
a <- ggplot(mtc) + 
  geom_boxplot(aes(x=am, y=cyl, fill=am)) + 
  ggtitle('Box Plot of Number of Cylinders by Transmission Type')
b <- ggplot(mtc) + 
  geom_density(aes(x=cyl, group=am, color=am, fill=am), alpha=0.3) + 
  ggtitle('Density Plot of Number of Cylinders by Transmission Type')

c <- ggplot(mtc) + 
  geom_boxplot(aes(x=am, y=disp, fill=am)) + 
  ggtitle('Box Plot of Displacement by Transmission Type')
d <- ggplot(mtc) + 
  geom_density(aes(x=disp, group=am, color=am, fill=am), alpha=0.3) + 
  ggtitle('Density Plot of Displacement by Transmission Type')

e <- ggplot(mtc) + 
  geom_boxplot(aes(x=am, y=hp, fill=am)) + 
  ggtitle('Box Plot of Horsepower by Transmission Type')
f <- ggplot(mtc) + 
  geom_density(aes(x=hp, group=am, color=am, fill=am), alpha=0.3) + 
  ggtitle('Density Plot of Horsepower by Transmission Type')

g <- ggplot(mtc) + 
  geom_boxplot(aes(x=am, y=wt, fill=am)) + 
  ggtitle('Box Plot of Weight by Transmission Type')
h <- ggplot(mtc) + 
  geom_density(aes(x=wt, group=am, color=am, fill=am), alpha=0.3) + 
  ggtitle('Density Plot of Weight by Transmission Type')

grid.arrange(a, b, c, d, e, f, g, h, ncol=2)
# dev.copy(png, 'potential_confounders.png', width=900, height=900)
# dev.off()



#### typecasting
# mtc$cyl <- factor(mtc$cyl)
# mtc$vs <- factor(mtc$vs)
# mtc$gear <- factor(mtc$gear)
# mtc$carb <- factor(mtc$carb)



#### effect of confounders
model1 <- lm(mpg ~ am, mtc)
model2 <- lm(mpg ~ wt + cyl + disp + hp + am, mtc)

summary(model1)  # coef for mpg increase in manual transmission is stat. sig.
summary(model2)  # coef for mpg increase in manual transmission NOT stat. sig.

summary(model1)$coef  # coef for mpg increase in manual transmission is stat. sig.
summary(model2)$coef  # coef for mpg increase in manual transmission NOT stat. sig.

anova(model1, model2)
# the p-value obtained is highly significant;
# reject the null hypothesis that the confounder variables
# wt, cyl, disp and hp don’t contribute to the accuracy of the model

# interpretation: holding the vehicle weight, number of cylinders,
# displacement, and horsepower the same, the increase in MPG
# for manual transmission is NOT statistically significant



#### model's residuals
## residuals, sum of squared errors (SSE), and mean squared error (MSE)
residuals <- residuals(model2)
SSE <- sum(residuals^2); SSE
MSE <- SSE / length(residuals); MSE

## plot residuals to ensure heteroscedasticity
plot(model2)



#### centering the weight variable
head(mtc)
wt_mean <- mean(mtc$wt)

mtc$wt_centered <- mtc$wt - wt_mean
auto$wt_centered <- auto$wt - wt_mean
manual$wt_centered <- manual$wt - wt_mean



#### modeling with the centered weight variable
## centered weights
mean(auto$wt_centered)
mean(manual$wt_centered)
# manual-tranmission cars are lighter than those of auto-transmission

## build separate models for auto- and manual-transmission datasets
model_auto <- lm(mpg ~ wt_centered, auto)
model_manual <- lm(mpg ~ wt_centered, manual)

summary(model_auto)
# an average-weight auto-transmission vehicle would log about 19.24 MPG; 
# 1-sd increase in wt would bring MPG down by 3.79

summary(model_manual)
# an average-weight manual-transmission vehicle would log about 17.067 MPG;
# 1-sd increase in wt would bring MPG down by 9.08

## plot points and regression lines in ggplot2
ggplot(mtc, aes(x=wt, y=mpg, color=factor(am))) + 
  geom_point() + 
  geom_smooth(method='lm', fill=NA) +
  guides(col = guide_legend(title = 'transmission\ntype'))

## residuals for model_auto
residuals_auto <- residuals(model_auto)
SSE_auto <- sum(residuals_auto^2); SSE_auto
MSE_auto <- SSE_auto / length(residuals_auto); MSE_auto
plot(auto$wt_centered, residuals_auto)
abline(h=0, col='red', lwd=2)

## residuals for model_manual
residuals_manual <- residuals(model_manual)
SSE_manual <- sum(residuals_manual^2); SSE_manual
MSE_manual <- SSE_manual / length(residuals_manual); MSE_manual
plot(manual$wt_centered, residuals_manual)
abline(h=0, col='red', lwd=2)
