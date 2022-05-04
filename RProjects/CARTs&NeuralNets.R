library(rpart)
library(rpart.plot)
## CART
#Partition the data
library(readr)
spambase <- read.csv("spambase.csv")
View(spambase)

set.seed(1)
train.index <- sample(nrow(spambase), nrow(spambase) * 0.6)
valid.index <- as.numeric(setdiff(rownames(spambase), train.index))
spam.train <- spambase[train.index, ]
spam.valid <- spambase[valid.index, ]
# Classification Tree
class.tree <- rpart(Spam ~ ., data = spambase, method = "class", cp = 0, minsplit = 1)
# Plot tree
prp(class.tree, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(class.tree$frame$var == "<leaf>", 'gray', 'white'))
# Confusion Matrix for validation data
default.ct.pred.valid <- predict(class.tree, spam.valid, type = "class")
confusionMatrix(default.ct.pred.valid, 
                as.factor(spam.valid$Spam), 
                positive = "1")
# Prune the tree
cv.ct <- rpart(Spam ~ ., data = spambase, method = "class",
               cp = 0.00001, minsplit = 5, xval = 10)
printcp(cv.ct)
# plot tree with lowest xerror
pruned.ct <- prune(cv.ct, cp = 0.00055157)
prp(pruned.ct, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))
# COnfusion Matrix
prune.ct.pred.valid <- predict(pruned.ct, spam.valid, type = "class")
confusionMatrix(prune.ct.pred.valid, 
                as.factor(spam.valid$Spam), 
                positive = "1")
# Classifying with new predictors
## Random Forest
rf <- randomForest(as.factor(Spam) ~ ., data = spambase, ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)
# plot
varImpPlot(rf, type = 1)
# Confusion matrix
rf.pred <- predict(rf, spam.valid)
confusionMatrix(rf.pred, as.factor(spam.valid$Spam), positive = "1")




## Neural Nets
# See skewness of all variables (if less then -1 or greater than +1 it is highly skewed)
skewness(spambase$make)
skewness(spambase$address)
skewness(spambase$all)
skewness(spambase$W_3d)
skewness(spambase$our)
skewness(spambase$over)
skewness(spambase$remove)
skewness(spambase$internet)
skewness(spambase$order)
skewness(spambase$mail)
skewness(spambase$receive)
skewness(spambase$will)
skewness(spambase$people)
skewness(spambase$report)
skewness(spambase$addresses)
skewness(spambase$free)
skewness(spambase$business)
skewness(spambase$email)
skewness(spambase$you)
skewness(spambase$credit)
skewness(spambase$your)
skewness(spambase$font)
skewness(spambase$W_000)
skewness(spambase$money)
skewness(spambase$hp)
skewness(spambase$hpl)
skewness(spambase$george)
skewness(spambase$W_650)
skewness(spambase$lab)
skewness(spambase$labs)
skewness(spambase$telnet)
skewness(spambase$W_857)
skewness(spambase$CAP_tot)
skewness(spambase$CAP_long)
skewness(spambase$CAP_avg)
skewness(spambase$C1)
skewness(spambase$C2)
skewness(spambase$C3)
skewness(spambase$C4)
skewness(spambase$C6)
skewness(spambase$conference)
skewness(spambase$table)
skewness(spambase$edu)
skewness(spambase$re)
skewness(spambase$project)
skewness(spambase$original)
skewness(spambase$meeting)
skewness(spambase$cs)
skewness(spambase$direct)
skewness(spambase$pm)
skewness(spambase$parts)
skewness(spambase$W_1999)
skewness(spambase$technology)
skewness(spambase$W_85)
skewness(spambase$W_415)
skewness(spambase$data)
# Perform Log transition for all predictors
cols <- colnames(spambase[ , c(1:57)])

for (i in cols) {
  spambase[[i]] <- log(spambase[[i]] + 1)
}
# Standarize all predictors
str(spambase)
# set all variables to 0
spam.new <- spambase[1, -57]
for (i in cols) {
  spam.new[[i]] <- 0
}                              
cols <- colnames(spambase[,-57])
for (i in cols) {
  spam.new[[i]] <- (spam.new[[i]] - min(spambase[[i]])) / (max(spambase[[i]]) - min(spambase[[i]]))
  spambase[[i]] <- (spambase[[i]] - min(spambase[[i]])) / (max(spambase[[i]]) - min(spambase[[i]]))
}
summary(spambase)                           
# partition the data
set.seed(5)
train.rows <- sample(nrow(spambase), nrow(spambase)*0.6)
train.data <- spambase[train.rows, ]
valid.data <- spambase[-train.rows, ]
valid.rows <- as.numeric(row.names(valid.data))
#Neural net plot 1, 1 hidden layer with 3 nodes
nn.1 <- neuralnet(Spam ~ ., data = train.data , hidden = c(3,1))
# display weights
nn.1$weights
# display predictions
prediction(nn.1)
# plot network1
plot(nn.1, rep = "best")
# confusion matrix 1
training.prediction <- compute(nn.1, train.data[, -57])
training.class <- apply(training.prediction$net.result, 1, which.max) - 1
confusionMatrix(as.factor(training.class), as.factor(spambase[train.index, ]$Spam))
#Neural net plot 2, 1 hidden layer with 28 nodes
nn.2 <- neuralnet(Spam ~ ., data = train.data , hidden = c(28,1))
# display weights
nn.2$weights
# display predictions
prediction(nn.2)
# Plot network 2
plot(nn.2, rep = "best")
# Confusion matrix 2
training.prediction1 <- compute(nn.2, train.data[, -57])
training.class1 <- apply(training.prediction1$net.result, 1, which.max) - 1
confusionMatrix(as.factor(training.class1), as.factor(spambase[train.index, ]$Spam))
# Neural net plot 3, 2 hidden layers with 12 nodes
nn.3 <- neuralnet(Spam ~ ., data = train.data , hidden = c(12,2))
# display weights
nn.3$weights
# display predictions
prediction(nn.3)
# Plot network 2
plot(nn.3, rep = "best")
# Confusion matrix 2
training.prediction2 <- compute(nn.3, train.data[, -57])
training.class2 <- apply(training.prediction2$net.result, 1, which.max) - 1
confusionMatrix(as.factor(training.class2), as.factor(spambase[train.index, ]$Spam))