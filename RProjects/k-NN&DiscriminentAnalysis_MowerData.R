## k-Nearest Neighbors
# Import Lawn Mower Data
mower.df <- read.csv("RidingMowers.csv")

# partition the data
set.seed(111)
train.index <- sample(nrow(mower.df), nrow(mower.df) * 0.6)
mower.train <- mower.df[train.index, ]
mower.valid <- mower.df[-train.index, ]

# new household - predict ownership for househole with $60,000 income and 20,000 sq. ft. lot
new.df <- data.frame(Income = 60, Lot_Size = 20)

## scatter plot
plot(Lot_Size ~ Income, data = mower.train, pch = ifelse(mower.train$Ownership == "Owner", 1, 3))
text(mower.train$Income, mower.train$Lot_Size, rownames(mower.train), pos = 4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

## normalize data
# initialize normalized training, validation, and complete data frames to originals
mower.train.norm <- mower.train
mower.valid.norm <- mower.valid
mower.df.norm <- mower.df

# use preProcess() from the caret package to normalize Income and Lot_Size
library(caret)
norm.values <- preProcess(mower.train[, 1:2], method = c("center", "scale"))
mower.train.norm[, 1:2] <- predict(norm.values, mower.train[, 1:2])
mower.valid.norm[, 1:2] <- predict(norm.values, mower.valid[, 1:2])
mower.df.norm[, 1:2] <- predict(norm.values, mower.df[, 1:2])
new.df.norm <- predict(norm.values, new.df)

# use knn() to compute knn
# and library class (allows a numerical output variable)
install.packages("FNN") 
library(FNN)
nn <- knn(train = mower.train.norm[, 1:2], test = new.df.norm, cl = mower.train.norm[, 3], k = 3)
row.names(mower.train)[attr(nn, "nn.index")]
nn

## measuring accuracy of different k-values
library(caret)
# initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation set
for (i in 1:14) {
  knn.pred <- knn(mower.train.norm[, 1:2], mower.valid.norm[, 1:2], cl = mower.train.norm[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, mower.valid.norm[, 3])$overall[1]
}
accuracy.df

# classifying a new household using the "best k" = 8
knn.pred.new <- knn(mower.df.norm[, 1:2], new.df.norm, cl = mower.df.norm[, 3], k = 8)
row.names(mower.df)[attr(knn.pred.new, "nn.index")]
knn.pred.new

# confusion matrix using best k on validation data
knn.pred.best <- knn(mower.train.norm[, 1:2], mower.valid.norm[, 1:2], 
                     cl = mower.train.norm[, 3], k = 8, prob = TRUE)
confusionMatrix(knn.pred.best, mower.valid.norm[, 3], positive = "Owner")

## discriminant analysis
bank.df <- read.csv("UniversalBank.csv")

str(bank.df)

# drop ID and ZIP.Code
bank.df <- bank.df[, -c(1, 5)]
str(bank.df)

# scatter plot
plot(CCAvg ~ Income, data = bank.df, col = ifelse(bank.df$Personal.Loan == 1, "red", "blue"))
legend("topleft", c("acceptor", "nonacceptor"), col = c("red", "blue"), pch = 1)

# partition the data
set.seed(1)
train.index <- sample(nrow(bank.df), nrow(bank.df) * 0.6)
bank.train <- bank.df[train.index, ]
bank.valid <- bank.df[-train.index, ]
valid.index <- as.numeric(row.names(bank.valid))

# code for discriminant analysis
install.packages("DiscriMiner") # this only needs done once
library(DiscriMiner)
bank.da <- linDA(bank.df[, c(3, 5)], bank.df$Personal.Loan, validation = "learntest", 
                 learn = train.index, 
                 test = valid.index)
bank.da$functions

# classification scores, predicted classes, and probabilities
# compute probabilities manually
propensity.acceptor <- exp(bank.da$scores[, 2]) / (exp(bank.da$scores[, 1]) + exp(bank.da$scores[, 2]))
da.results <- data.frame(Actual = bank.valid$Personal.Loan, bank.da$classification, bank.da$scores, 
                         propensity.acceptor = propensity.acceptor)
head(da.results, 20)

# confusion matrix
confusionMatrix(bank.da$classification, 
                as.factor(bank.valid$Personal.Loan), 
                positive = "1")

# lift chart
library(gains)
gain <- gains(bank.valid$Personal.Loan, exp(bank.da$scores[, 2]) / (exp(bank.da$scores[, 1]) + exp(bank.da$scores[, 2])), 
              groups = length(bank.valid))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(bank.valid$Personal.Loan))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(bank.valid$Personal.Loan))) ~ c(0, nrow(bank.valid)), lty = 2)

# compute deciles and plot decile-wise lift chart
gain <- gains(as.numeric(bank.valid$Personal.Loan), 
              exp(bank.da$scores[, 2]) / (exp(bank.da$scores[, 1]) + exp(bank.da$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(bank.valid$Personal.Loan))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 6),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")


# prior probabilities
bank.da.prior <- linDA(bank.df[, c(3, 5)], bank.df$Personal.Loan, prior = c(0.95, 0.05), validation = "learntest", 
                 learn = train.index, 
                 test = valid.index)
bank.da.prior$functions

# DA for mower
mower.da <- linDA(mower.df[, 1:2], mower.df[, 3])
mower.da$functions

# prior probabilities for mower
mower.da.prior <- linDA(mower.df[, 1:2], mower.df[, 3], prior = c(0.85, 0.15))
mower.da.prior$functions

