# Import Data
credit.data <- read.csv("credit_default.csv")
# EDA
dim(credit.data)
t(t(names(credit.data)))
mean(credit.data$default.payment.next.month)

library(dplyr)

credit.data <- rename(credit.data, default = default.payment.next.month)
str(credit.data)
summary(credit.data)

## Converting variables
credit.data$SEX <- as.factor(credit.data$SEX)
credit.data$EDUCATION <- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE <- as.factor(credit.data$MARRIAGE)

## Two-Way Contingency Table and Chi-Square Test
table.edu <- table(credit.data$EDUCATION, credit.data$default)
table.edu

chisq.test(table.edu)

## Predictive Analysis of Credit Default Data Using Logistic Regression

set.seed(1)
index <- sample(nrow(credit.data), nrow(credit.data) * 0.8)
credit.train <- credit.data[index, ]
credit.valid <- credit.data[-index, ]

## Train a Logistic Regression with All Predictors
credit.glm0 <- glm(default ~ ., family = "binomial", data = credit.train)
summary(credit.glm0)

## Metrics for Model Fitting
credit.glm0$deviance
AIC(credit.glm0)
BIC(credit.glm0)

## Prediction
library(forecast)

# In-sample Predcition
pred.glm0.train <- predict(credit.glm0, type = "response")

## ROC Curve
library(pROC)

r <- roc(credit.train$default, pred.glm0.train)
plot.roc(r)
auc(r) # Area under the curve

# Out of sample predicition

pred.glm0.valid <- predict(credit.glm0, newdata = credit.valid, type = "response")

r <- roc(credit.valid$default, pred.glm0.valid)
plot.roc(r)
auc(r)

# creating lift charts and decile lift charts
library(gains)
gain <- gains(credit.valid$default, pred.glm0.valid, groups = length(pred.glm0.valid))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(credit.valid$default)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(credit.valid$default)) ~ c(0, nrow(credit.valid)), lty = 2)

# compute deciles and plot decile-wise lift chart
gain <- gains(credit.valid$default, pred.glm0.valid)
heights <- gain$mean.resp / mean(credit.valid$default)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

## Binary classification
library(caret)

confusionMatrix(as.factor(ifelse(pred.glm0.train >= 0.5, "1", "0")), as.factor(credit.train$default), 
                positive = "1")

# Naive Choice of Cutoff Probability
pcut1 <- mean(credit.train$default)
pcut1

# get binary prediction
class.glm0.train <- (pred.glm0.train > pcut1) * 1
# get confusion matrix
confusionMatrix(as.factor(class.glm0.train), as.factor(credit.train$default), 
                positive = "1")

## Determine Optimal Cutoff Probability Using Grid Search Method
# define a cost function with input "obs" being the observed response,
# "pred.p" being predicted probability, and "pcut" being the threshold
costfunc <- function(obs, pred.p, pcut){
  weight1 <- 5 # define the weight for "true=1 but pred=0" (false negative)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (false positive)
  c1 <- (obs == 1) & (pred.p < pcut) # count for "true=1 but pred=0" (false negative)
  c0 <- (obs == 0) & (pred.p >= pcut) # count for "true=0 but pred=1" (false positive)
  cost <- mean(weight1 * c1 + weight0 * c0) # misclassification with weight
  return(cost) # you have to return a value when you write R functions
} # end of the function

# define a sequence from 0.01 to 1 by 0.01
p.seq <- seq(from = 0.01, to = 1, by = 0.01)

# write a loop for all potential cutoff values to see which provides the smallest cost
# first, you need to define a 0 vector in order to save the values of cost from all cutoffs
cost <- rep(0, length(p.seq))
for (i in 1:length(p.seq)) {
  cost[i] <- costfunc(obs = credit.train$default, pred.p = pred.glm0.train, pcut = p.seq[i])
}

# draw a plot with the x-axis being all pcut and y-axis being associated cost
plot(cost ~ p.seq)

optimal.pcut.glm0 <- p.seq[which(cost == min(cost))]
optimal.pcut.glm0

# Use the Optimal Cutoff Probability

confusionMatrix(as.factor(ifelse(pred.glm0.train >= optimal.pcut.glm0, "1", "0")), as.factor(credit.train$default), 
                positive = "1")

cost <- costfunc(obs = credit.train$default, pred.p = pred.glm0.train, pcut = optimal.pcut.glm0)
cost

# Variable selection with logistic regression
credit.glm.null <- glm(default ~ 1, data = credit.train, family = "binomial")
credit.glm.fwd <- step(credit.glm.null, scope = list(credit.glm.null, upper = credit.glm0), direction = "forward")
summary(credit.glm.fwd)
credit.glm.fwd$deviance
AIC(credit.glm.fwd)
BIC(credit.glm.fwd)



