# Import Data
housing.df <- read.csv("BostonHousing.csv")
housing.df$RAD <- as.factor(housing.df$RAD) # convert to factor
# EDA
housing.df <- housing.df[, -14]
str(housing.df)
# partition the data
set.seed(5)
train.index <- sample(nrow(housing.df), nrow(housing.df) * 0.9)
valid.index <- as.numeric(setdiff(rownames(housing.df), train.index))
housing.train <- housing.df[train.index, ]
housing.valid <- housing.df[valid.index, ]
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
# Fitting Regression Tree
housing.rt <- rpart(MEDV ~ ., data = housing.train)
housing.rt # printing the tree
prp(housing.rt, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(housing.rt$frame$var == "<leaf>", 'gray', 'white'))

# Prediciton using regression tree
# In-sample predicition
housing.train.rt.pred <- predict(housing.rt)
library(caret)
RMSE(housing.train.rt.pred, housing.train$MEDV)
# Out-of-sample prediciton
housing.valid.rt.pred <- predict(housing.rt, newdata = housing.valid)
RMSE(housing.valid.rt.pred, housing.valid$MEDV)

# Comparing the Performance of Regression Tree with Linear Regression Model 
housing.lm <- 
  housing.train.lm.pred <- 
  housing.valid.lm.pred <- 
  RMSE.lm.train <- 
  RMSE.lm.valid <- 
  
  # Classification Trees
  ## Preparation
  
  credit.data <- read.csv("credit_default.csv")

# rename
library(dplyr)
credit.data <- rename(credit.data, default = default.payment.next.month)

# convert categorical data to factors
credit.data$SEX <- as.factor(credit.data$SEX)
credit.data$EDUCATION <- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE <- as.factor(credit.data$MARRIAGE)

# partitioning the data
set.seed(11)
index <- sample(nrow(credit.data),nrow(credit.data)*0.80)
credit.train = credit.data[index,]
credit.valid = credit.data[-index,]

# Fitting Classification tree
credit.ct0 <- rpart(default ~ ., data = credit.train, method = "class")
# Confusion matrix
pred0 <- predict(credit.ct0, type = "class")
confusionMatrix(pred0, as.factor(credit.train$default), positive = "1")

credit.ct <- rpart(default ~ ., data = credit.train, method = "class", 
                   parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)))
# Printing and plotting the tree
credit.ct
prp(credit.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(credit.ct$frame$var == "<leaf>", 'gray', 'white'))
# Predicition using the classification tree
# In-sample predicition
credit.train.ct.pred1 <- predict(credit.ct, credit.train, type = "class")
confusionMatrix(credit.train.ct.pred1, as.factor(credit.train$default), positive = "1")
# Out-of-sample prediction
credit.valid.ct.pred1 <- predict(credit.ct, credit.valid, type = "class")
confusionMatrix(credit.valid.ct.pred1, as.factor(credit.valid$default), positive = "1")
# Calculate the Actual Cost Using a Self-Defined Cost Function
cost <- function(r, pi){
  weight1 <- 5
  weight0 <- 1
  c1 <- (r == 1) & (pi == 0) # logical vector - true if actual 1 but predict 0
  c0 <- (r == 0) & (pi == 1) # logical vector - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
cost(credit.train$default, credit.train.ct.pred1)

# Comparing the Performance of Classification Tree with Logistic Regression Model
# fit logistic regression model
credit.glm <- glm(default ~ ., data = credit.train, family = "binomial")

# get binary prediction
credit.valid.glm.pred <- as.numeric(predict(credit.glm, credit.valid, type = "response") > 0.23)

# calculate cost using validation set
cost(credit.valid$default, credit.valid.glm.pred)
# confusion matrix
confusionMatrix(as.factor(credit.valid.glm.pred), as.factor(credit.valid$default), positive = "1")
#ROC Curve for Classification Tree
credit.valid.ct.prob <- predict(credit.ct, credit.valid, type = "prob")
library(pROC)
r <- roc(credit.valid$default, credit.valid.ct.prob[, 2])
plot.roc(r)
auc(r) # Area under the curve
# Pruning
housing.largetree <- rpart(MEDV ~ ., data = housing.train, cp = 0.001)
# Plotting
prp(housing.largetree, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(housing.largetree$frame$var == "<leaf>", 'gray', 'white'))
plotcp(housing.largetree) # relationship between 10-fold cross-validation error in the training set and the size of the tree.

printcp(housing.largetree)
sum((housing.train$MEDV - mean(housing.train$MEDV))^2) / nrow(housing.train)
mean((predict(housing.largetree) - housing.train$MEDV)^2)
0.12282 * 86.582

housing.pruned <- prune(housing.largetree, cp = 0.0090412)
housing.pruned

prp(housing.pruned, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(housing.pruned$frame$var == "<leaf>", 'gray', 'white'))


