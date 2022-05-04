# import Data
setwd("C:/Users/danut/Desktop/Data Mining")
library(readr)
WeatherSample <- read.csv("WeatherSample.csv", stringsAsFactors = TRUE)
## EDA
dim(WeatherSample)
str(WeatherSample)
# box plots 
boxplot(Pressure3pm~RainTomorrow, data= WeatherSample)
boxplot(Temp3pm~RainTomorrow, data= WeatherSample)
boxplot(Humidity3pm~RainTomorrow, data= WeatherSample)
boxplot(WindSpeed3pm~RainTomorrow, data= WeatherSample)
# two-way contingency table
table.weather <- table(WeatherSample$RainToday, WeatherSample$RainTomorrow)
table.weather
# chi sqaured test
chisq.test(table.weather)
#summary statistic
summary(WeatherSample)
# removing not available data
WeatherSample<- data.frame(WeatherSample)
WeatherSample<-WeatherSample[!(WeatherSample$WindDir9am=="Not Available"),]
WeatherSample<-WeatherSample[!(WeatherSample$WindDir3pm=="Not Available"),]

# dummy variables (turn yes= 1 and no =0)
WeatherSample$RainToday <- ifelse(grepl("Yes", WeatherSample$RainToday), 1,0)
WeatherSample$RainTomorrow <- ifelse(grepl("Yes", WeatherSample$RainTomorrow), 1,0)

# winddir3pm
WeatherSample$key_pmDirNE <- ifelse(grepl("NE", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirE <- ifelse(grepl("E", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirENE <- ifelse(grepl("ENE", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirESE <- ifelse(grepl("ESE", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirN <- ifelse(grepl("N", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirNNE <- ifelse(grepl("NNE", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirNNW <- ifelse(grepl("NNW", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirNW <- ifelse(grepl("NW", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirS <- ifelse(grepl("S", WeatherSample$WindDir3pm), 1,0)
WeatherSample$key_pmDirSE <- ifelse(grepl("SE", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirSSE <- ifelse(grepl("SSE", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirSSW <- ifelse(grepl("SSW", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirSW <- ifelse(grepl("SW", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirW <- ifelse(grepl("W", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirWNW <- ifelse(grepl("WNW", WeatherSample$WindDir3pm), 1,0) 
WeatherSample$key_pmDirWSW <- ifelse(grepl("WSW", WeatherSample$WindDir3pm), 1,0)

#WindGustDir
WeatherSample$key_gustDirE <- ifelse(grepl("E", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirENE <- ifelse(grepl("ENE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirESE <- ifelse(grepl("ESE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirN <- ifelse(grepl("N", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirNE <- ifelse(grepl("NE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirNNE <- ifelse(grepl("NNE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirNNW <- ifelse(grepl("NNW", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirNW <- ifelse(grepl("NW", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirS <- ifelse(grepl("S", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirSE <- ifelse(grepl("SE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirSSE <- ifelse(grepl("SSE", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirSSW <- ifelse(grepl("SSW", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirSW <- ifelse(grepl("SW", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirW <- ifelse(grepl("W", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirWNW <- ifelse(grepl("WNW", WeatherSample$WindGustDir), 1,0)
WeatherSample$key_gustDirWSW <- ifelse(grepl("WSW", WeatherSample$WindGustDir), 1,0)

#windDir9am
WeatherSample$key_amDirENE <- ifelse(grepl("ENE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirESE <- ifelse(grepl("ESE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirN <- ifelse(grepl("N", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirNE <- ifelse(grepl("NE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirNNE <- ifelse(grepl("NNE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirNNW <- ifelse(grepl("NNW", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirNW <- ifelse(grepl("NW", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirS <- ifelse(grepl("S", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirSE <- ifelse(grepl("SE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirSSE <- ifelse(grepl("SSE", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirSSW <- ifelse(grepl("SSW", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirSW <- ifelse(grepl("SW", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirW <- ifelse(grepl("W", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirWNW <- ifelse(grepl("WNW", WeatherSample$WindDir9am), 1,0)
WeatherSample$key_amDirWSW <- ifelse(grepl("WSW", WeatherSample$WindDir9am), 1,0)

#Convert Humidity to factor
WeatherSample$Humidity3pm <- as.factor(WeatherSample$Humidity3pm) # convert to factor
dummies <- as.data.frame(model.matrix(~ 0 + Humidity3pm, data = WeatherSample))
WeatherSample <- cbind(WeatherSample[, -9], dummies)
str(WeatherSample)
# Nomalize
WeatherSample.norm<- WeatherSample

install.packages("scales")
library(scales)
WeatherSample.norm$MinTemp <- (WeatherSample.norm$MinTemp - min(WeatherSample.norm$MinTemp)) / (max(WeatherSample.norm$MinTemp) - min(WeatherSample.norm$MinTemp))
WeatherSample.norm$MaxTemp <- (WeatherSample.norm$MaxTemp - min(WeatherSample.norm$MaxTemp)) / (max(WeatherSample.norm$MaxTemp) - min(WeatherSample.norm$MaxTemp))
WeatherSample.norm$Rainfall <- (WeatherSample.norm$Rainfall - min(WeatherSample.norm$Rainfall)) / (max(WeatherSample.norm$Rainfall) - min(WeatherSample.norm$Rainfall))
WeatherSample.norm$Evaporation <- (WeatherSample.norm$Evaporation - min(WeatherSample.norm$Evaporation)) / (max(WeatherSample.norm$Evaporation) - min(WeatherSample.norm$Evaporation))
WeatherSample.norm$Sunshine <- (WeatherSample.norm$Sunshine - min(WeatherSample.norm$Sunshine)) / (max(WeatherSample.norm$Sunshine) - min(WeatherSample.norm$Sunshine))
WeatherSample.norm$WindSpeed9am <- (WeatherSample.norm$WindSpeed9am - min(WeatherSample.norm$WindSpeed9am)) / (max(WeatherSample.norm$WindSpeed9am) - min(WeatherSample.norm$WindSpeed9am))
WeatherSample.norm$WindSpeed3pm <- (WeatherSample.norm$WindSpeed3pm - min(WeatherSample.norm$WindSpeed3pm)) / (max(WeatherSample.norm$WindSpeed3pm) - min(WeatherSample.norm$WindSpeed3pm))
WeatherSample.norm$Humidity9am <- (WeatherSample.norm$Humidity9am - min(WeatherSample.norm$Humidity9am)) / (max(WeatherSample.norm$Humidity9am) - min(WeatherSample.norm$Humidity9am))
WeatherSample.norm$Humidity9am <- (WeatherSample.norm$Humidity9am - min(WeatherSample.norm$Humidity9am)) / (max(WeatherSample.norm$Humidity9am) - min(WeatherSample.norm$Humidity9am))
WeatherSample.norm$Pressure9am <- (WeatherSample.norm$Pressure9am - min(WeatherSample.norm$Pressure9am)) / (max(WeatherSample.norm$Pressure9am) - min(WeatherSample.norm$Pressure9am))
WeatherSample.norm$Pressure3pm <- (WeatherSample.norm$Pressure3pm - min(WeatherSample.norm$Pressure3pm)) / (max(WeatherSample.norm$Pressure3pm) - min(WeatherSample.norm$Pressure3pm))
WeatherSample.norm$Cloud9am <- (WeatherSample.norm$Cloud9am- min(WeatherSample.norm$Cloud9am)) / (max(WeatherSample.norm$Cloud9am) - min(WeatherSample.norm$Cloud9am))
WeatherSample.norm$Cloud3pm <- (WeatherSample.norm$Cloud3pm- min(WeatherSample.norm$Cloud3pm)) / (max(WeatherSample.norm$Cloud3pm) - min(WeatherSample.norm$Cloud3pm))
WeatherSample.norm$Temp9am <- (WeatherSample.norm$Temp9am- min(WeatherSample.norm$Temp9am)) / (max(WeatherSample.norm$Temp9am) - min(WeatherSample.norm$Temp9am))
WeatherSample.norm$Temp3pm <- (WeatherSample.norm$Temp3pm- min(WeatherSample.norm$Temp3pm)) / (max(WeatherSample.norm$Temp3pm) - min(WeatherSample.norm$Temp3pm))
summary(WeatherSample.norm)

# rescaling
WeatherSample.rescaled <- WeatherSample.norm
WeatherSample$MinTemp.rescaled <- rescale(WeatherSample$MinTemp)
WeatherSample$MaxTemp.rescaled <- rescale(WeatherSample$MaxTemp)
WeatherSample$Rainfall.rescaled <- rescale(WeatherSample$Rainfall)
WeatherSample$Evaporation.rescaled <- rescale(WeatherSample$Evaporation)
WeatherSample$Sunshine.rescaled <- rescale(WeatherSample$Sunshine)
WeatherSample$WindSpeed9am.rescaled <- rescale(WeatherSample$WindSpeed9am)
WeatherSample$WindSpeed3pm.rescaled <- rescale(WeatherSample$WindSpeed3pm)
WeatherSample$Humidity9am.rescaled <- rescale(WeatherSample$Humidity9am)
WeatherSample$Humidity3pm.rescaled <- rescale(WeatherSample$Humidity3pm)
WeatherSample$Pressure9am.rescaled <- rescale(WeatherSample$Pressure9am)
WeatherSample$Pressure3pm.rescaled <- rescale(WeatherSample$Pressure3pm)
WeatherSample$Cloud9am.rescaled <- rescale(WeatherSample$Cloud9am)
WeatherSample$Cloud3pm.rescaled <- rescale(WeatherSample$Cloud3pm)
WeatherSample$Temp9am.rescaled <- rescale(WeatherSample$Temp9am)
WeatherSample$Temp3pm.rescaled <- rescale(WeatherSample$Temp3pm)
summary(WeatherSample.rescaled)



# Partitioning the data
set.seed(1)
train.index <- sample(nrow(WeatherSample.rescaled), nrow(WeatherSample.rescaled) * 0.6)
valid.index <- as.numeric(setdiff(rownames(WeatherSample.rescaled), train.index))
weather.train <- WeatherSample.rescaled[train.index, ]
weather.valid <- WeatherSample.rescaled[-train.index, ]


# knn, can't use humidity3pm because factor data types don't work in knn
install.packages("FNN") # this only needs done once
library(FNN)
t(t(names(WeatherSample.rescaled)))
nn <- knn(train = weather.train[, c(2:6,8, 10, 14:20, 22:159)], test = weather.valid[,c(2:6,8, 10, 14:20, 22:159)], cl = weather.train$RainTomorrow, k = 1)
row.names(WeatherSample.rescaled)[attr(nn, "nn.index")]
nn
# Confusion Matrix
library(caret)
confusionMatrix(nn, as.factor(weather.valid$RainTomorrow), positive = "1")
#Determine best K
accuracy <- data.frame(k = seq(1, 35, 1), accuracy = rep(0, 35))

for (i in 1:35) {
  knn.pred <- knn(train = weather.train[, c(2:6,8, 10, 14:20, 22:159)], test = weather.valid[, c(2:6,8, 10, 14:20, 22:159)], 
                  cl = weather.train[, 21], k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, as.factor(weather.valid$RainTomorrow), positive = "1")$overall[1]
}
accuracy

# k = 4 for a parsimonious model

nn2 <- knn.reg(train = weather.train[, c(2:6,8, 10, 14:20, 22:159)], test = weather.valid[, c(2:6,8, 10, 14:20, 22:159)], 
               y = weather.train$RainTomorrow, k = 4)

nn2.results <- data.frame(cbind(pred = nn2$pred, actual = weather.valid$RainTomorrow))
head(nn2.results, 20)
