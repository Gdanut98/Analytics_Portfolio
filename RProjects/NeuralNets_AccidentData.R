library(neuralnet)
library(nnet)
library(caret)

# load the data
accidents.df <- read.csv("accidentsnn.csv")
summary(accidents.df)

# Convert ALCHL_I to a binary dummy variable using an ifelse() function
accidents.df$ALCHL_I <- ifelse(accidents.df$ALCHL_I == 1, 1, 0)
summary(accidents.df$ALCHL_I)

# Convert SUR_COND to a factor and use the model.matrix() function to create dummy variables for each level of SUR_COND leaving out one dummy.
accidents.df$SUR_COND <- as.factor(accidents.df$SUR_COND)
dummies <- as.data.frame(model.matrix(~ 0 + SUR_COND, data = accidents.df))
t(t(names(dummies)))

t(t(names(accidents.df)))

# drop original SUR_COND variable and one dummy variable
accidents.df <- cbind(accidents.df[, -3], dummies[, -5])
t(t(names(accidents.df)))

# standardize VEH_INVL to a [0,1] scale by subtracting by its minimum value and dividing by its range for every record.
accidents.df$VEH_INVL <- (accidents.df$VEH_INVL - min(accidents.df$VEH_INVL)) /
  (max(accidents.df$VEH_INVL) - min(accidents.df$VEH_INVL))
summary(accidents.df$VEH_INVL)

## convert MAX_SEV_IR to a factor and partition the data, using 60% of the records for the training sample and 40% for the validation.
# convert MAX_SEV_IR to a factor
accidents.df$MAX_SEV_IR <- as.factor(accidents.df$MAX_SEV_IR)

# partition the data
set.seed(2)
train.index <- sample(nrow(accidents.df), nrow(accidents.df)*0.6)
valid.index <- setdiff(row.names(accidents.df), train.index)
acc.train <- accidents.df[train.index, ]
acc.valid <- accidents.df[valid.index, ]

summary(acc.train)

## Training the model
# run nn with 2 hidden nodes
# use hidden= with a vector of integers specifying number of nodes in each hidden layer
nn <- neuralnet(MAX_SEV_IR ~ ., data = acc.train, hidden = 2)

# plot the network
plot(nn, rep = "best")

# Assessing the classification performance
# In-Sample
training.prediction <- compute(nn, acc.train[, -4])
training.class <- apply(training.prediction$net.result, 1, which.max) - 1
confusionMatrix(as.factor(training.class), as.factor(accidents.df[train.index, ]$MAX_SEV_IR))

# Out-of-sample
validation.prediction <- compute(nn, acc.valid[, -4])
validation.class <- apply(validation.prediction$net.result, 1, which.max) - 1
confusionMatrix(as.factor(validation.class), as.factor(accidents.df[valid.index, ]$MAX_SEV_IR))

## Tweaking the model
# Trying the neural net with 2 hidden layers and 2 nodes each
nn.2.2 <- neuralnet(MAX_SEV_IR ~ ., data = acc.train, hidden = c(2, 2), stepmax = 1e6)

# plot the network
plot(nn.2.2, rep = "best")

# Confusion matrix
training.prediction2.2 <- compute(nn.2.2, acc.train[, -4])
training.class2.2 <- apply(training.prediction2.2$net.result, 1, which.max) - 1
confusionMatrix(as.factor(training.class2.2), as.factor(accidents.df[train.index, ]$MAX_SEV_IR))

## Neural net for Predicition
# Load data
toyota.df <- read.csv("ToyotaCorolla.csv")
t(t(names(toyota.df)))

toyota.sub <- toyota.df[, c(3:4, 8:9, 12, 14, 17, 19, 21, 25:26, 28, 30, 34, 39)]
str(toyota.sub)
summary(toyota.sub)

# Looking at all numerical predictors
library(e1071)
num.cols <- colnames(toyota.sub[, c(2,4,6:7,9)])
for (i in num.cols) {
  boxplot(toyota.sub[[i]],
          ylab = i)
  print(skewness(toyota.sub[[i]]))
}

##Quarterly_Tax and Guarantee_Period are highly skewed,so apply a log transformation.

toyota.sub$Quarterly_Tax <- log(toyota.sub$Quarterly_Tax + 1)
toyota.sub$Guarantee_Period <- log(toyota.sub$Guarantee_Period + 1)

# Fuel_Type must be converted to binary dummy variables, leaving one out in the process
dummies <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = toyota.sub))
t(t(names(dummies)))
t(t(names(toyota.sub)))

toyota.sub <- cbind(toyota.sub[, -3], dummies[, -3])
summary(toyota.sub)

# rescale all variables to a [0,1] scale using the preProcess() function with method="range".

norm.values <- preProcess(toyota.sub, method = "range")
toyota.norm <- predict(norm.values, toyota.sub)
summary(toyota.norm)

## Partition the data
set.seed(41)
train.index <- sample(nrow(toyota.norm), nrow(toyota.norm)*0.6)
toyota.train <- toyota.norm[train.index, ]
toyota.valid <- toyota.norm[-train.index, ]

# Training the Model with a single hidden layer containing 2 nodes
nn1 <- neuralnet(Price ~ ., data = toyota.train, linear.output = FALSE, hidden = 2)

# looking at the predicted proce for the first record
nn1$response[1, ]

# Rescaling the output
nn1$response[1, ]*(max(toyota.df$Price) - min(toyota.df$Price)) + min(toyota.df$Price)

# Plotting the network
plot(nn1, rep = "best")

# Assessing the predictive performance
# In-sample
predict1.train <- compute(nn1, toyota.train[, c(2:16)])
RMSE(predict1.train$net.result, toyota.train$Price)
# Out-of-sample
predict1.valid <- compute(nn1, toyota.valid[, c(2:16)])
RMSE(predict1.valid$net.result, toyota.valid$Price)

