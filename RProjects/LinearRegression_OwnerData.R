owner.df <- read.csv("ownerExample.csv")

install.packages("caret") 
install.packages("e1071") 
library(caret)
library(e1071)

## cutoff = 0.5
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.5, 'owner', 'nonowner')), owner.df$Class,
                positive = "owner")

## cutoff = 0.25
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.25, 'owner', 'nonowner')), owner.df$Class,
                positive = "owner")

## cutoff = 0.75
confusionMatrix(as.factor(ifelse(owner.df$Probability > 0.75, 'owner', 'nonowner')), owner.df$Class,
                positive = "owner")

# create empty accuracy table
accT <- c()

# compute accuracy per cutoff
owner.df$dumClass <- as.factor(ifelse(owner.df$Class == "owner", 1, 0))
for (cut in seq(0, 1, 0.01)){
  cm <- confusionMatrix(as.factor(1 * (owner.df$Probability > cut)), owner.df$dumClass)
  accT <- c(accT, cm$overall[1])
}

# plot accuracy
plot(accT ~ seq(0, 1, 0.01), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1 - accT ~ seq(0, 1, 0.01), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

# ROC curve
install.packages("pROC")
library(pROC)
r <- roc(owner.df$dumClass, owner.df$Probability)
plot.roc(r)

# compute AUC
options(digits = 3)
auc(r)

## lift charts
install.packages("gains")
library(gains)
df <- read.csv("liftexample.csv")
gain <- gains(df$actual, df$prob, groups = dim(df)[1])
plot(c(0, gain$cume.pct.of.total * sum(df$actual)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(df$actual)) ~ c(0, dim(df)[1]), col = "gray", lty = 2)

## decile lift chart
# use gains() to compute deciles
# when using the caret package, deciles must be computed manually
gain <- gains(df$actual, df$prob) # default for groups is 10
barplot(gain$mean.resp / mean(df$actual), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise Lift Chart")

