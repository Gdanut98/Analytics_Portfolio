# Import Toyota Data
toyota.df <- read.csv("ToyotaCorolla.csv")

t(t(names(toyota.df)))
summary(toyota.df[, c(3:7, 9, 13:18, 21)])

# bar chart showing distribution of fuel type
data.for.plot <- aggregate(toyota.df$Id, by = list(toyota.df$Fuel_Type), FUN = length)
names(data.for.plot) <- c("FuelType", "CountOfCars")
barplot(height = data.for.plot$CountOfCars, names.arg = data.for.plot$FuelType, 
        xlab = "Fuel Type", ylab = "# of Cars", main = "Distribution of Fuel Type")

# correlation matrix with heatmap
install.packages('gplots')
library(gplots)
colfun <- colorRampPalette(c("red", "white", "green"))
heatmap.2(round(cor(toyota.df[, c(3:7, 9, 13:14, 16:18, 21)]), 2), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", col = colfun(15), lwid=c(0.1,4), lhei=c(0.1,4),
          cellnote = round(cor(toyota.df[, c(3:7, 9, 13:14, 16:18, 21)]), 2), 
          notecol = "black", key = FALSE, trace = "none", margins = c(10, 10))

# boxplot of CC by fuel type
boxplot(toyota.df$CC ~ toyota.df$Fuel_Type, color = "blue", xlab = "Fuel Type", 
        ylab = "CC", main = "Distribution of CC by Fuel Type")

# drop the record(s) containing the outlier for CC
toyota.df <- toyota.df[toyota.df$CC != 16000, ]
# recreate boxplot without the outlier
boxplot(toyota.df$CC ~ toyota.df$Fuel_Type, color = "blue", xlab = "Fuel Type", 
        ylab = "CC", main = "Distribution of CC by Fuel Type")

# scatterplot matrix
plot(toyota.df[, c(3:5, 7, 9, 13:14, 16:18, 21)])
# scatterplot of price by age
plot(toyota.df$Price ~ toyota.df$Age_08_04, xlab = "Age (in months as of August 2004)", 
     ylab = "Price")

# create new data frame containing only the variables to be used for analysis
toyotasub.df <- toyota.df[, c(3, 4, 7:10, 12:14, 17:18)]
head(toyotasub.df)


# partitioning the data
# use set.seed() to get the same partitions 
set.seed(1)

## partitioning into training (60%) and validation (40%) 
train.rows <- sample(rownames(toyotasub.df), nrow(toyotasub.df)*0.6)
# collect all the columns with training row ID into training set:
train.data <- toyotasub.df[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(toyotasub.df), train.rows)
valid.data <- toyotasub.df[valid.rows, ]

head(train.data)
head(valid.data)

# use lm() to run a linear regression of Price on all 10 predictors in the
# training set
# use . after ~ to include all the remaining columns in train.df as predictors.
toyota.lm <- lm(Price ~ ., data = train.data)
# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(toyota.lm)

install.packages("forecast")  
library(forecast)
# use predict() to make predictions on a new set
valid.lm.pred <- predict(toyota.lm, valid.data)
options(scipen = 999, digits = 1)
# calculate the residuals
valid.resid <- valid.data$Price - valid.lm.pred
# look at the first 20 residuals
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$Price[1:20],
           "Residual" = valid.resid[1:20])

# use accuracy() to compute common accuracy measures
accuracy(valid.lm.pred, valid.data$Price)

# view the distribution of residuals
install.packages('tidyverse')
library(tidyverse)
ggplot(data = as.data.frame(valid.resid), aes(y = valid.resid)) + geom_boxplot()

# exhaustive search
install.packages("leaps")
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = train.data))
train.data <- cbind(train.data[, -4], Fuel_Type[ , -1])
head(train.data)
search <- regsubsets(Price ~ ., data = train.data, nbest = 1, 
                     nvmax = dim(train.data)[2], method = "exhaustive")
sum <- summary(search)
sum$which # shows the models
options(scipen = 999, digits = 8)
t(t(sum$rsq)) # shows r-squared values
t(t(sum$adjr2)) # shows adjusted r-squared values
t(t(sum$cp)) # shows Mallow's Cp values


# create model with no predictors for bottom of search range
toyota.lm <- lm(Price ~ ., data = train.data)
toyota.lm.null <- lm(Price ~ 1, data = train.data)

# use step() to run forward selection
toyota.lm.fwd <- step(toyota.lm.null, scope = list(toyota.lm.null, upper = toyota.lm), 
                      direction = "forward")

summary(toyota.lm.fwd)


toyota.lm.back <- step(toyota.lm, direction = "backward")

summary(toyota.lm.back)


toyota.lm.step <- step(toyota.lm.null, scope = list(toyota.lm.null, upper = toyota.lm), 
                       direction = "both")

summary(toyota.lm.step)


library(forecast)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = valid.data))
valid.data <- cbind(valid.data[, -4], Fuel_Type[ , -1])
valid.fwd.pred <- predict(toyota.lm.fwd, valid.data)
options(scipen = 999)
accuracy(valid.fwd.pred, valid.data$Price)

# compare to the original model
accuracy(valid.lm.pred, valid.data$Price)

