# Load data
data("iris")

# data type
class(iris)

# dimensions of the dataset
dim(iris)

# number of records
nrow(iris)

# number of variables
ncol(iris)

# variable names
names(iris)

# structure
# will show you the data type of each variable and how many levels there are for
# each factor (categorical variable)
str(iris)

# view the first six records
head(iris)

# data type of a specific variable
class(iris[, 1])
class(iris[, 5])

# Summary Statistics
summary(iris)
summary(Filter(is.numeric, iris))
# standard deviation
sd(iris$Sepal.Length)

# quantiles
quantile(iris$Sepal.Length)

apply(iris[, 1:4], 2, sd) # "2" means by "column"

# group mean
aggregate(. ~ Species, iris, mean)

# group standard deviation
aggregate(. ~ Species, iris, sd)

# Use the "cut" function for categorization based on quantiles
Cat.SepalLength <- cut(iris$Sepal.Length, breaks = quantile(iris$Sepal.Length), include.lowest = TRUE)
# Add the created categorical variable to the data
iris1 <- iris
iris1$Sepal.Length.Cat <- Cat.SepalLength

aggregate(. ~ Species + Sepal.Length.Cat, iris1, mean)

# one-way count table
table(iris1$Species)
# two-way count table
table(iris1$Species, iris1$Sepal.Length.Cat)

## Visualization
# Create Histogram
hist(iris$Sepal.Length, col = "green", breaks = 20)
# Density plot
plot(density(iris$Sepal.Length))
# Combine both plots
hist(iris$Sepal.Length, probability = TRUE, col = "green", breaks = 20, main = "Histogram and Density of Sepal Length", xlim = c(3, 9), xlab = "Sepal Length")
lines(density(iris$Sepal.Length), col = "red", lwd = 2)

# Add a vertical line that indicates the average of SepalLength
abline(v = mean(iris$Sepal.Length), col = "blue", lty = 2, lwd = 1.5)

# bar chart for count of records by job category
CustomerData <- read.csv("CustomerData.csv")
countjob <- aggregate(CustomerData$CustomerID, by = list(CustomerData$JobCategory), 
                      FUN = length)
names(countjob) <- c("Job", "CountOfCustomers")
barplot(height = countjob$CountOfCustomers, names.arg = countjob$Job, 
        xlab = "Job", ylab = "Count")

# bar chart for average of the 4 quantitative variables in the iris data
aveg <- apply(iris[, 1:4], 2, mean)
barplot(aveg, ylab = "Average")

# Pie chart
pie(table(CustomerData$JobCategory), col = rainbow(7))
# box plot of Sepal.Length
boxplot(iris$Sepal.Length)

# Box plot with multiple variables
boxplot(iris[, 1:4], notch = TRUE, col = c("red", "blue", "yellow", "gray"))

# Box plot by groups
boxplot(iris[, 1] ~ iris[, 5], notch = TRUE, ylab = "Sepal Length", col = "blue")

# Scatter plot
plot(iris$Sepal.Length ~ iris$Sepal.Width, xlab = "Width", ylab = "Length",
     main = "Sepal")
par(xpd = TRUE)
plot(iris$Sepal.Length ~ iris$Sepal.Width, ylab = "Sepal Length", xlab = "Sepal Width",
     col = ifelse(iris$Species == "setosa","blue",
                  ifelse(iris$Species == "versicolor","red","green")))
legend("topleft", inset = c(0, -0.2), legend = c("setosa", "versicolor", "virginica"),
       col = c("blue", "red", "green"), pch = 1, cex = 0.9)

# Scatter plot matrix
plot(iris[, 1:4])
# Parallel coordinate plot
library(MASS)
parcoord(iris[, 1:4], col = iris$Species)

# Correlation matrix with heatmap
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, CustomerData)), Rowv = FALSE, 
          Colv = FALSE, dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, CustomerData)), 2),
          notecol = "black", key = FALSE, trace = 'none')

library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, CustomerData), use = "complete.obs"), Rowv = FALSE, 
          Colv = FALSE, dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), 
          col = colfunc(15), 
          cellnote = round(cor(Filter(is.numeric, CustomerData), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none')

# Displaying multiple plots in one window
# set arrangement of multiple plots
par(mfrow = c(2, 2)) # mfrow() and mfcol() serve the same purpose
# set margins
par(mar = c(4.5, 4.2, 3, 1.5))
hist(iris$Sepal.Length, xlab = "Sepal Length", cex.lab = 1.5)
hist(iris$Sepal.Width, xlab = "Sepal Width", col = "red")
plot(iris$Sepal.Width ~ iris$Sepal.Length, xlab = "Length", ylab = "Width", 
     main = "Sepal", pch = 17)
boxplot(iris[, 1:4], notch = TRUE, col = c("red", "blue", "yellow", "gray"))

## Handling missing data
# Deleting rows
summary(Filter(is.numeric, CustomerData))

CustomerData <- CustomerData[!(is.na(CustomerData$HouseholdSize) |
                                 is.na(CustomerData$NumberPets) |
                                 is.na(CustomerData$NumberCats)), ]

# Imputing the median for missing values
CustomerData$NumberDogs[is.na(CustomerData$NumberDogs)] <- median(CustomerData$NumberDogs,
                                                                  na.rm = TRUE)
CustomerData$NumberBirds[is.na(CustomerData$NumberBirds)] <- 
  median(CustomerData$NumberBirds, na.rm = TRUE)
CustomerData$HomeOwner[is.na(CustomerData$HomeOwner)] <- median(CustomerData$HomeOwner,
                                                                na.rm = TRUE)
# view the different levels of the categorical variable *TownSize*
levels(CustomerData$TownSize) 

# add a new level called 'N/A'
levels(CustomerData$TownSize) <- c(levels(CustomerData$TownSize), 'N/A')
levels(CustomerData$TownSize)

# replace '#NULL!' values with 'N/A'
CustomerData$TownSize[CustomerData$TownSize == '#NULL!'] <- 'N/A'
summary(CustomerData$TownSize)

levels(CustomerData$Gender)

levels(CustomerData$Gender) <- c(levels(CustomerData$Gender), 'N/A')
levels(CustomerData$Gender)

CustomerData$Gender[CustomerData$Gender == ''] <- 'N/A'
summary(CustomerData$Gender)

# Creating dummy variables

dummies <- model.matrix(~ 0 + TownSize + Gender, 
                        data = CustomerData)
dummies <- as.data.frame(dummies)

# look at the names of the dummy variables
t(t(names(dummies)))

# drop any now-unused levels (*TownSize#NULL!* in this case) in addition to
# one remaining dummy for each categorical variable
dummies <- dummies[, -c(1, 7, 10)]
t(t(names(dummies)))

# now combine the new dummy variables with the original dataset, dropping the
# original categorical variables
CustomerData <- cbind(CustomerData[, -c(3:4)], dummies)
t(t(names(CustomerData)))