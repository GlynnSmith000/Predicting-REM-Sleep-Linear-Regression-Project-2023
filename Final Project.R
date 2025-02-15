#loading packages
library(tidyverse)
library(caret)
library(asbio)
library(olsrr)
library(xtable)
library(shiny)
library(knitr)
library(DT)
require(scatterplot3d)
require(Hmisc)
require(rgl)
require(faraway)
library(car)
library(leaps)
library(onewaytests)
library(dplyr)
library(psych)
library(ggplot2)
#load in data
setwd("~/Documents/Masters/STAT 840/Final Proj")
data <- read.csv("Sleep_Efficiency.csv", header = TRUE)
view(data)
unique(data$Caffeine.consumption)
unique(data$Alcohol.consumption)
###################
#Exploring the data
nrow(data)# 452 rows
#Checking/removing rows with N/A values
SD<-na.omit(data)
nrow(SD)#388 rows
##############################################
#Creating variable for REM sleep time in hours
SD$REM.sleep.time <- ((SD$Sleep.duration*SD$Sleep.efficiency)*SD$REM.sleep.percentage)/60
#omitting uneeded variables:
SD <- subset(SD, select = -c(ID, Bedtime, Wakeup.time,REM.sleep.percentage, 
                             Deep.sleep.percentage, Light.sleep.percentage,
                             Sleep.duration, Sleep.efficiency))
#transforming Gender: 1 for Fale, 0 for Female
SD$Gender <- ifelse(SD$Gender == "Male",1,0)
#transforming Smoking Status: 1 for Yes, 0 for No
SD$Smoking.status <- ifelse(SD$Smoking.status == 'Yes', 1,0)
#################################################
#splitting the data for model training/validation
#first 195 observations will be used for model-building/training
training <- SD[1:195,]
#last 193 observations will be used for validation
validation <- SD[196:388,]
#applying Dependent Log transformation used in training dataset
validation$LogREM.sleep.time <- log(validation$REM.sleep.time)
validation <- subset(validation, select = -c(REM.sleep.time))
###########################
#Box plots of the variables
par(mfrow = c(3,2))
#Age
boxplot(training$Age, horizontal =TRUE,
        main = '(a) Box plot of Age (years)')
#Awakenings
boxplot(training$Awakenings, horizontal = TRUE,
        main='(b) Box plot of Awakenings')
#Caffeine Consumption
boxplot(training$Caffeine.consumption, horizontal = TRUE,
        main = '(c) Box plot Caffeine Consumption (mg)')
#Alcohol Consumption
boxplot(training$Alcohol.consumption, horizontal = TRUE,
        main = '(d) Box plot of Alcohol Consumption')
#Exercise Frequency
boxplot(training$Exercise.frequency, horizontal = TRUE,
        main = '(e) Box plot of Exercise Frequency')
#REM.sleep.time
boxplot(training$REM.sleep.time, horizontal = TRUE,
        main = '(f) Box plot of REM Sleep Time')
#Barcharts for Gender and Smoking.status as they're categorical
#Gender
training %>%
  ggplot(aes(x=Gender)) +
  geom_bar() +
  ggtitle('(a) Bar chart for Gender') +
  ylab('')
#Smoking
training %>%
  ggplot(aes(x=Smoking.status)) +
  geom_bar() +
  ggtitle('(b) Bar chart for Smoking Status') +
  xlab('Smoking Status')+
  ylab('')
#############################
#preliminary model/full model
m1 <- lm(REM.sleep.time ~., data = training)
summary(m1)
#preliminary model: residuals vs. fitted values
par(mfrow = c(1,2))
plot(residuals(m1)~fitted(m1), main = '(a)', ylab = 'Residuals',
     xlab = expression(hat(Y)))
abline(h=0, col = 'red')
#issue of non-constant varaiance, consider a log transformation
qqnorm(residuals(m1), main = '(b)')
qqline(residuals(m1), col = 'red')
#Log Transformation of REM Sleep Time
training$LogREM.sleep.time <- log(training$REM.sleep.time)
training <- subset(training, select = -c(REM.sleep.time))
m1.t <- lm(LogREM.sleep.time~., data = training)
#transformed preliminary model: residuals vs. fitted values
par(mfrow = c(1,2))
plot(residuals(m1.t)~fitted(m1.t), main = '(a)', ylab = 'Residuals',
     xlab = expression(hat(Y)))
abline(h=0, col = 'red')

qqnorm(residuals(m1.t), main = '(b)')
qqline(residuals(m1.t), col = 'red')

#Partial Residual Plots of the transformed preliminary model
crPlots(m1.t, 
        pch = 16, col.lines = c('blue', 'red'))
#scatter-plot matrix and correlation matrix of the preliminary model
pairs.panels(training)
cor(training)
#VIF of the model
vif(m1.t)
vif(m.final)
##################################
#model selection using automatic selection methods: best subsets and stepwise
#force in exercise frequency as it is the main variable of interest
#method 1: best subsets
summary(m1)
best.subs <- regsubsets(LogREM.sleep.time~., force.in = 7, data = training)
(best.subs.sum <- summary(best.subs))
#Screening further to select the best model based on: adjusted R^2, CP, BIC, and RSS values
#adjusted R^2: bigger is better
#fullmodel/model 6: 0.2868291
#model 5: 0.2857763
#model 4: 0.2823637
best.subs.sum$adjr2
#Mallow's CP: low CP value and CP=P is best: 
#model 5: CP = 7.277507, p = 7, bias = 0.277507
#full model/model 6: Cp = 8, p = 8, bias = 0
best.subs.sum$cp
#BIC: smaller is better:
#model 2: --37.15807 
#model 3: -38.72200
#model 4: -38.15318
best.subs.sum$bic
#RSS: smaller is better:
#model 4: 9.965151 
#model 5: 9.865288
#fullmodel/model 6: 9.798349
best.subs.sum$rss
#Three candidate models from best subsets: model 4, model 5, and fullmodel/model 6
#Method 2: Automatic selection method - stepwise
step.subs <- regsubsets(LogREM.sleep.time~., force.in = 7, data = training, method = "seqrep")
(step.subs.sum <- summary(step.subs))
#Screening further to select the best model based on: adjusted R^2, CP, BIC, and RSS values
#adjusted R^2: bigger is better:
#full model/model 6: 0.2868291
#model 5: 0.2857763
step.subs.sum$adjr2
#Mallow's CP: low CP value and CP=P is best: 
#model 5: CP = 7.277507, p = 7, bias = 0.277507
#fullmodel/model 6: Cp = 8, p = 8, bias = 0
step.subs.sum$cp
#BIC: smaller is better:
#model 3: -38.72200
step.subs.sum$bic
#RSS: smaller is better:
#model 5: 9.865288
#fullmodel/model 6: 9.798349
step.subs.sum$rss
#Two candidate models from stepwise method: model 5 and full model/model 6

#Model 5 and  was robust to both methods. Therefore, will be moving with 
#the fullmodel/model 6, model 5, and model 4 from best subsets to the
#next step of validation via data-splitting
fullmodel <- lm(LogREM.sleep.time~., data = training)
#model 5 from best subsets
m5 <- lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
                Caffeine.consumption + Alcohol.consumption + 
                Smoking.status, data = training)
#model 4 from best subsets
m4 <- lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
            + Alcohol.consumption + Smoking.status, data = training)
####################################
#Validation of the fullmodel/model 6
anova(fullmodel)$'Sum Sq'[8] #SSE: 9.798349
anova(fullmodel)$'Mean Sq'[8]#MSE: 0.05239759
press(fullmodel)#PRESS: 10.67473

fullmodel.valid <-lm(LogREM.sleep.time~., data = validation)
summary(fullmodel)
summary(fullmodel.valid)
anova(fullmodel.valid)$'Sum Sq'[8]#SSE.v: 11.22664
anova(fullmodel.valid)$'Mean Sq'[8]#MSE.v: 0.06068455
press(fullmodel.valid)#PRESS.v: 12.21069
#calculate MSPR and compare to MSE for training
#MSPR
#fit <- predict(traininig model, newdata = testing/validation data)
#pe <- testing/validation data$dependent variable - fit
#(MSPR <- (t(pe)%*%pe)/length(pe))
fit <- predict(fullmodel, newdata = validation)
pe <- validation$LogREM.sleep.time-fit
(MSPR <- (t(pe)%*%pe)/length(pe)) #0.06276787
#MSE for training set
anova(fullmodel)$'Mean Sq'[8]#0.05239759
#MSPR/MSE difference:0.01037028
##########################
#Validation of the model 5
anova(m5)$'Sum Sq'[7]#SSE; 9.865288
anova(m5)$'Mean Sq'[7]#MSE: 0.05247494
press(m5)#PRESS: 10.64064

m5.valid <-lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
                      Caffeine.consumption + Alcohol.consumption + 
                      Smoking.status, data = validation)
summary(m5)
summary(m5.valid)
anova(m5.valid)$'Sum Sq'[7]#SSE.v: 53.82344
anova(m5.valid)$'Mean Sq'[7]#MSE.v: 0.2886186
press(m5.valid)#PRESS.v: 12.15274
#calculate MSPR and compare to MSE for training
#MSPR
fit <- predict(m5, newdata = validation)
pe <- validation$LogREM.sleep.time-fit
(MSPR <- (t(pe)%*%pe)/length(pe))#0.06274656
#MSE for training set
anova(m5)$'Mean Sq'[7]#0.05247494
#MSPR/MSE difference: 0.01027162
###################################
#Validation of the stepwise model 4 
anova(m4)$'Sum Sq'[6]#SSE: 9.965151
anova(m4)$'Mean Sq'[6]#MSE: 0.05272567
press(m4)#PRESS: 10.57748

m4.valid <- lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
           + Alcohol.consumption + Smoking.status, data = validation)
summary(m4)
summary(m4.valid)
anova(m4.valid)$'Sum Sq'[6]#SSE.v: 11.28781
anova(m4.valid)$'Mean Sq'[6]#MSE.v: 0.06036259
press(m4.valid)#PRESS.v: 12.03004
#MSPR
fit <- predict(m4, newdata = validation)
pe <- validation$LogREM.sleep.time-fit
(MSPR <- (t(pe)%*%pe)/length(pe))#0.06274979
#MSE for training set
anova(m4)$'Mean Sq'[6]#0.05272567
#MSPR/MSE difference: 0.01002412
#########################################
#fitting the final model to the full data
#adding transformed dependent variable to full data
SD$LogREM.sleep.time <- log(SD$REM.sleep.time)
SD <- subset(SD, select = -c(REM.sleep.time))

m.final <- lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
           + Alcohol.consumption + Smoking.status, data = SD)
summary(m.final)
###################################################################
#detecting outliers and influential observations in the final model
#outlying Y observations
#Bonferroni Critical Value for outlying Y-observations
n<-nrow(SD) #388 observations
p <- 6 #6betas (B0, B1, B2, B3, B4, B5)
alpha <- 0.05
#Bonferroni simultaneous test procedure: qt(1-(alpha/2n), n-p-1)
qt(1-alpha/(2*n), n-p-1)
#if |t-value| >= qt(1-(alpha/2*n), n-p-1), then the observation is an outlier,
#otherwise, not an outlier
which(abs(rstandard(m.final))>qt(1-alpha/(2*n), n-p-1))
#deleted studentized residuals vs fitted
par(mfrow = c(2,2))
plot(rstandard(m.final)~fitted(m.final), 
     ylab = 'Del. Studentized Residuals', 
     xlab = expression(hat(Y)),
     main = "(a)",
     ylim = c(-4,4)) 
abline(h=0, lty = 3, lwd = 2, col = 'red')
abline(h=3.868316, lty = 3, lwd = 2, col = 'blue') #values taken from BF test procedure
abline(h=-3.868316, lty = 3, lwd = 2, col = 'blue')
#outlying X observations using hat matrix
#leverage value greater then 2*p/n are considered large
which(hatvalues(m.final)>((2*p)/n))
hatvalues(m.final)[103]#observation 118, different number due to ommitting NA values
hatvalues(m.final)[131]#observation 155
hatvalues(m.final)[222]#observation 258
hatvalues(m.final)[263]#observation 303
hatvalues(m.final)[328]#observation 379
#4 observations w/ high leverage were identified. Need to investigate how influential these
#observations are in the fitting of the regression function

#influential cases using DFFITS,DFBETAS, Cooks distance
#DFFITS
#Rule of thumb: DFFITS > 2*sqrt(p/n) for large datasets
CT <- 2*sqrt(p/n)
plot(dffits(m.final), ylim = c(-.5,.5), 
     ylab = 'DFFITS', 
     main = '(b)')
abline(h=CT, lty = 3, lwd = 2, col = 'blue')
abline(h=-CT, lty = 3, lwd = 2, col = 'blue')
which(abs(dffits(m.final))>CT)
#16 influential cases identified (high leverage observations 258 and 303 are influential based on DFFITS and outlying in regards to its X value)

#DFBETAS
#rule of thumb: DFBETASS > 2/sqrt(n)
CV <- 2/sqrt(n)
plot(dfbetas(m.final), ylim = c(-.2,.2), 
     ylab = 'DFBETAS', 
     main = '(c)')
abline(h=CV, lty = 3, lwd = 2, col = 'blue')
abline(h=-CV, lty = 3, lwd = 2, col = 'blue')
which(abs(dfbetas(m.final))>CV)
nrow(which(abs(dfbetas(m.final))>CV))
#143 betas are considered influential

#Cooks Distance
#rule: if the percentile is less than about 10-20%, there
#is little apparent influence. If the percentile value is near
#50%, the observation is considered influential
plot(cooks.distance(m.final),
     ylab = 'Cooks Distance', 
     main = '(d)')
#compare Cook's Distance to F(p, n-p) pg.403
summary(m.final)
q <- pf(cooks.distance(m.final),6,388-6)
which(q>.1) 
#no influential observations based on cook's distance are observed

summary(m.final)
#removing observations 258 and 303
reduced.data <- SD[-c(222,263),]
nrow(reduced.data)
summary(lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
     + Alcohol.consumption + Smoking.status, data = reduced.data))
#no significant changes in the betas or the fit of the model
#removing all observations with high leverage: 118,155,258,303,379 
reduced.data <- SD[-c(103,131,222,263,328),]
nrow(reduced.data)
summary(lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
             + Alcohol.consumption + Smoking.status, data = reduced.data))
summary(m.final)
#no significant changes in the betas or the fit of the model
#removing values based on DFFITS
reduced.data <- SD[-c(1,7,50,136,179,186,216,220,222,263,271,290,297,308,357),]
nrow(reduced.data)
summary(lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
             + Alcohol.consumption + Smoking.status, data = reduced.data))
summary(m.final)
#no significant changes in the betas or the fit of the model
#removing values based on high leverage and DFFITS
reduced.data <- SD[-c(1,7,50,103,131,222,263,328,136,179,186,216,220,222,263,271,290,297,308,357),]
nrow(reduced.data)
m.r<-lm(LogREM.sleep.time~Exercise.frequency + Age + Awakenings +
     + Alcohol.consumption + Smoking.status, data = reduced.data)
summary(m.r)
summary(m.final)

#MSE/SSE of the final model w/full data
anova(m.final)$'Mean Sq'[6]#MSE: 0.05671533
anova(m.final)$'Sum Sq'[6]#SSE: 21.66526
#MSE/SSE of the final model w/reduced datta
anova(m.r)$'Mean Sq'[6]#MSE: 0.04858081
anova(m.r)$'Sum Sq'[6]#SSE: 17.68342

#Final model residual diagnostics
par(mfrow = c(1,2))
#residuals vs. fitted
plot(abs(residuals(m.final))~predict(m.final), 
     xlab = expression(hat(Y)),ylab = "Abs Residuals",
     main = '(a)')
#normality plot
qqnorm(residuals(m.final), main = '(b)')
qqline(residuals(m.final), col= 'red')

#coefficient of correlation between 
#ordered resid. vs. expected values under normality
#untransformed final model
ols_test_correlation(m.final)

#Percent of variation in the final model explained by exercise frequency
round(partial.R2(lm(LogREM.sleep.time~Age + Awakenings + Alcohol.consumption + 
                Smoking.status, data = SD),m.final)*100,2)

#Variable of interest: Exercise Frequency
partial.R2(lm(LogREM.sleep.time~Age + Awakenings + Alcohol.consumption + Smoking.status, data = SD),m.final)
#Age
round(partial.R2(lm(LogREM.sleep.time~Exercise.frequency + Awakenings + Alcohol.consumption + Smoking.status, data = SD),m.final)*100,2)
#Awakenings
round(partial.R2(lm(LogREM.sleep.time~Exercise.frequency + Age + Alcohol.consumption + Smoking.status, data = SD),m.final)*100,2)
#Alcohol consumption
round(partial.R2(lm(LogREM.sleep.time~Exercise.frequency + Age+ Awakenings + Smoking.status, data = SD),m.final)*100,2)
#Smoking Status
round(partial.R2(lm(LogREM.sleep.time~Exercise.frequency + Age+ Awakenings + Alcohol.consumption, data = SD),m.final)*100,2)


summary(m.final)
summary(m.final)$sigma^2
anova(m.final)











