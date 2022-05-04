setwd("C:/Users/danut/Desktop/Data Mining")
library(readr)
spambase <- read.csv("spambase.csv" , stringsAsFactors= TRUE)
str(spambase)
summary(spambase)

# summary statistic of mean spam/not spam against everything else
aggregate(.~ Spam, spambase, mean) # make, W_3d,our,remove,you, your,lab,hp,hpl,george,labs,cs,meeting,CAP_aveg, CAP_long,CAP_tot that vary the most

# chi squared test to determine if the predictors have a relationship with spam
 table.address<- table(spambase$address, spambase$Spam)   
table.address 
chisq.test(table.address) # pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.labs<- table(spambase$labs, spambase$Spam)
chisq.test(table.labs) # pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.telnet<- table(spambase$telnet, spambase$Spam)
chisq.test(table.telnet)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.data<- table(spambase$data, spambase$Spam)
chisq.test(table.data)# pvalue > .05 we retain the null hypothesis and reject the alternative hypothesis
table.pm<- table(spambase$pm, spambase$Spam)
chisq.test(table.pm)# pvalue > .05 we retain the null hypothesis and reject the alternative hypothesis
table.edu<- table(spambase$edu, spambase$Spam)
chisq.test(table.edu)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.re<- table(spambase$"re:", spambase$Spam)
chisq.test(table.re)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.project<- table(spambase$project, spambase$Spam)
chisq.test(table.project)# pvalue > .05 we retain the null hypothesis and reject the alternative hypothesis
table.meeting<- table(spambase$meeting, spambase$Spam)
chisq.test(table.meeting)# pvalue > .05 we retain the null hypothesis and reject the alternative hypothesis
table.C1<- table(spambase$"C!", spambase$Spam)
chisq.test(table.C1)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.Capavg<- table(spambase$CAP_avg, spambase$Spam)
chisq.test(table.Capavg)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.Caplong<- table(spambase$CAP_long, spambase$Spam)
chisq.test(table.Caplong)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship
table.Captot<- table(spambase$CAP_tot, spambase$Spam)
chisq.test(table.Captot)# pvalue< .05 reject the null hypothesis and conclude that there is a relationship

# Logistic Regression
set.seed(1)
index <- sample(nrow(spambase), nrow(spambase) * 0.6)
spam.train <- spambase[index, ]
spam.valid <- spambase[-index, ]

# Logistic Regression with all predictors using training data
spam.glm0 <- glm(Spam ~ ., family = "binomial", data = spam.train)
summary(spam.glm0)

# Metrics for Model Fitting
# residual deviance
spam.glm0$deviance

# AIC&BIC
AIC(spam.glm0)
BIC(spam.glm0)

# Prediction
library(forecast)
pred.glm0.train <- predict(spam.glm0, type = "response")
pred.glm0.valid <- predict(spam.glm0, newdata = spam.valid, type = "response")

# ROC Curve
library(pROC)
r <- roc(spam.train$Spam, pred.glm0.train)
plot.roc(r)
# Area under the curve
auc(r)

# lift chart
library(gains)
gain.train <- gains(spam.train$Spam, pred.glm0.train, groups = length(pred.glm0.train))
plot(c(0, gain.train$cume.pct.of.total * sum(spam.train$Spam)) ~ c(0, gain.train$cume.obs),
     xlab = "# of spam mail", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(spam.train$Spam)) ~ c(0, nrow(spam.train)), lty = 2)

# decile-wise lift chart
gain.train <- gains(spam.train$Spam, pred.glm0.train)
heights <- gain.train$mean.resp / mean(spam.train$Spam)
dec.lift <- barplot(heights, names.arg = gain.train$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
# confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.train >= 0.5, "1", "0")), as.factor(spam.train$Spam), 
                positive = "1")

# Using Validation data 
# ROC Curve
library(pROC)
r.valid <- roc(spam.valid$Spam, pred.glm0.valid)
plot.roc(r.valid)
auc(r.valid)

# lift chart
library(gains)
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid, groups = length(pred.glm0.valid))
plot(c(0, gain.valid$cume.pct.of.total * sum(spam.valid$Spam)) ~ c(0, gain.valid$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(spam.valid$Spam)) ~ c(0, nrow(spam.valid)), lty = 2)

# Decile wise lift chart
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid)
heights <- gain.valid$mean.resp / mean(spam.valid$Spam)
dec.lift <- barplot(heights, names.arg = gain.valid$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
# confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.valid >= 0.5, "1", "0")), as.factor(spam.valid$Spam), 
                positive = "1")


# logistic model shown with coefficients and odds
log.reg.fit<-glm(Spam ~ ., family = "binomial", data = spam.train)
data.frame(summary(log.reg.fit)$coefficient, odds=exp(coef(log.reg.fit)))

# forward Variable selection
spam.glm.null <- glm(Spam ~ 1, data = spam.train, family = "binomial")
spam.glm.full <- glm(Spam ~ ., data = spam.train, family = "binomial")
spam.glm.fwd <- step(spam.glm.null, scope = list(spam.glm.null, upper = spam.glm0), direction = "forward")

summary(spam.glm.fwd)
spam.glm.fwd$deviance
AIC(spam.glm.fwd)
BIC(spam.glm.fwd)

# stepwise Variable selection
spam.glm.null2 <- glm(Spam ~ 1, data = spam.train, family = "binomial")
spam.glm.full2 <- glm(Spam ~ ., data = spam.train, family = "binomial")
spam.glm.step <- step(spam.glm.null2, scope = list(lower = spam.glm.null2, upper = spam.glm.full2), direction = "both")

summary(spam.glm.step)
spam.glm.step$deviance
AIC(spam.glm.step)
BIC(spam.glm.step)

#prediction for forward and backward variable selection using training data

pred.glm0.train.fwd <- predict(spam.glm.fwd, type = "response")
pred.glm0.valid.fwd <- predict(spam.glm.fwd, newdata = spam.valid, type = "response")

pred.glm0.train.step <- predict(spam.glm.bck, type = "response")
pred.glm0.valid.step <- predict(spam.glm.bck, newdata = spam.valid, type = "response")

# ROC curve for forward
library(pROC)
r.fwd <- roc(spam.valid$Spam, pred.glm0.valid.fwd)
plot.roc(r.fwd,main="Forward ROC Curve")
auc(r.fwd)

# ROC curve for Stepwise
library(pROC)
r.step <- roc(spam.valid$Spam, pred.glm0.valid.step)
plot.roc(r.step,main="Stepwise ROC Curve")
auc(r.step)

# lift chart for forward
library(gains)
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid.fwd, groups = length(pred.glm0.valid.fwd))
plot(c(0, gain.valid$cume.pct.of.total * sum(spam.valid$Spam)) ~ c(0, gain.valid$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Forward Lift Chart", type = "l")
lines(c(0, sum(spam.valid$Spam)) ~ c(0, nrow(spam.valid)), lty = 2)

# lift chart for StepWise
library(gains)
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid.step, groups = length(pred.glm0.valid.bck))
plot(c(0, gain.valid$cume.pct.of.total * sum(spam.valid$Spam)) ~ c(0, gain.valid$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Stepwise Lift Chart", type = "l")
lines(c(0, sum(spam.valid$Spam)) ~ c(0, nrow(spam.valid)), lty = 2)

# Decile wise lift chart for forwards
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid.fwd)
heights <- gain.valid$mean.resp / mean(spam.valid$Spam)
dec.lift <- barplot(heights, names.arg = gain.valid$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Forward Decile-wise Lift Chart")
# Decile wise lift chart for Stepwise
gain.valid <- gains(spam.valid$Spam, pred.glm0.valid.step)
heights <- gain.valid$mean.resp / mean(spam.valid$Spam)
dec.lift <- barplot(heights, names.arg = gain.valid$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Stepwise Decile-wise Lift Chart")
# confusion matrix for forward
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.valid.fwd >= 0.5, "1", "0")), as.factor(spam.valid$Spam), 
                positive = "1")
# confusion matrix for Stepwise
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.valid.step >= 0.5, "1", "0")), as.factor(spam.valid$Spam), 
                positive = "1")
