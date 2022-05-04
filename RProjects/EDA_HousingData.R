## Import Boston housing data
housing.df <- read.csv("BostonHousing.csv")

# summary statistics for housing data
summary(housing.df)

# summary statistics for columns 27 & 28 in IMDB data
IMDB.df <- read.csv("IMDB Movie Dataset.csv")
summary(IMDB.df[27:28])

## bar chart showing count of CHAS
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = length)
names(data.for.plot) <- c("CHAS","CountOfTracts")
barplot(height = data.for.plot$CountOfTracts, names.arg = data.for.plot$CHAS, xlab = "CHAS", ylab = "# of tracts")

## scatterplot with axis names
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "LSTAT", ylab = "MEDV")

## histogram of MEDV
hist(housing.df$MEDV, xlab = "MEDV")

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")

# correlation matrix for Boston Housing data
round(cor(housing.df), 2)

# install package containing heatmaps
install.packages("gplots")

# correlation matrix with heat map
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",col = colfunc(15),
          cellnote = round(cor(housing.df), 2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

## color plot
par(xpd = TRUE) #allows legend to be displayed outside of the plot area
plot(housing.df$NOX ~ housing.df$LSTAT, ylab = "NOX", xlab = "LSTAT",
     col = ifelse(housing.df$CAT..MEDV == 1, "black", "gray"))
# add legend outside of plotting area
legend("topleft", inset = c(0, -0.2),
       legend = c("CAT.MEDV = 1", "CAT.MEDV = 0"), col = c("black","gray"),
       pch = 1, cex = 0.5)


## panel plots
# compute the count of census tracts by RAD and CHAS
# In aggregate(), used "drop = FALSE" to include all combinations of RAD and CHAS
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$RAD, housing.df$CHAS),
                           FUN = length, drop = FALSE)
names(data.for.plot) <- c("RAD", "CHAS", "CountOfTracts")
# plot the data
par(mfcol = c(2,1))
barplot(height = data.for.plot$CountOfTracts[data.for.plot$CHAS == 0],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 0],
        xlab = "RAD", ylab = "# of tracts", main = "CHAS = 0")
barplot(height = data.for.plot$CountOfTracts[data.for.plot$CHAS == 1],
        names.arg = data.for.plot$RAD[data.for.plot$CHAS == 1],
        xlab = "RAD", ylab = "# of tracts", main = "CHAS = 1")

## scatterplot matrix
plot(housing.df)

# scatterplot matrix for a subset of variables
plot(housing.df[, c(1, 3, 12, 13)])

# alternate scatterplot matrix
install.packages("GGally") 
ggpairs(housing.df[, c(1, 3, 12, 13)])

## parallel coordinates plot
par(mfcol = c(1,1))
install.packages("MASS")  
library(MASS)
cols <-  c("skyblue","red")
parcoord(housing.df[, -14], col = cols[as.factor(housing.df$CAT..MEDV)], var.label = TRUE)

## creating binary dummy variables
WestRox.df <- read.csv("WestRoxbury.csv", header = TRUE)
# Use model.matrix() to convert all categorical variables in the data frame into
# a set of dummy variables. We must then turn the resulting data matrix back into
# a data frame for further work.
dummies <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = WestRox.df)
dummies <- as.data.frame(dummies)
t(t(names(dummies))) # checks the names of the dummy variables
head(dummies)
dummies <- dummies[, -4] # drop one of the dummy variables
# Now, put it all together again, drop original REMODEL from the data
WestRox.df <- cbind(WestRox.df[, -c(9, 14)], dummies)
t(t(names(WestRox.df)))
head(WestRox.df)

## deleting rows with missing values
IMDB.df <- read.csv("IMDB Movie Dataset.csv")
summary(IMDB.df$duration)  
IMDB.df <- IMDB.df[!is.na(IMDB.df$duration), ]
dim(IMDB.df)

## replacing missing data with median
summary(IMDB.df$budget) 


# Replace the missing values using the median of the remaining values.
# Use median() with na.rm = TRUE to ignore missing values when computing the median.
IMDB.df$budget[is.na(IMDB.df$budget)] <- median(IMDB.df$budget, na.rm = TRUE)
summary(IMDB.df$budget)

## rescaling to log scale
hist(housing.df$CRIM) # note that CRIM is highly postively skewed
plot(housing.df$MEDV ~ housing.df$CRIM) # as a result, this scatterplot is difficult
# to interpret

# rescaling to log scale makes the relationship easier to interpret
plot(housing.df$MEDV ~ housing.df$CRIM, log = 'xy')