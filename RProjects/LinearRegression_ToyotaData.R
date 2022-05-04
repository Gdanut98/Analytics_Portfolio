# Load Data
toyota <- read.csv("ToyotaCorolla.csv")
# Exploratory Data Analysis
dim(toyota)
names(toyota)
str(toyota)
summary(toyota)
## Prepartion
# Splitting Data into Training and Validation Sets
toyota <- toyota[toyota$CC != 16000, -c(1:2, 6, 15)]
set.seed(1)
train.rows <- sample(nrow(toyota), nrow(toyota) * 0.8)
toyota.train <- toyota[train.rows, ]
toyota.valid <- toyota[-train.rows, ]

# Build Regression model

# draw a scatter plot
plot(Price ~ Age_08_04, data = toyota.train)

# fit the simple linear regression
model0 <- lm(Price ~ Age_08_04, data = toyota.train)
model0

# add the resulting line to the scatter plot
abline(model0, col = "red", lty = 2, lwd = 2)

sum.model0 <- summary(model0)
sum.model0

model0$coefficients
sum.model0$adj.r.squared
confint(model0)

# Prediction
pred.model0 <- predict(model0, newdata = toyota.valid)

# Predict Price for Age_08_04 values of 10, 20, and 30
predict(model0, newdata = data.frame(Age_08_04 = c(10, 20, 30)), interval = "confidence")

model1 <- lm(Price ~ Age_08_04 + Mfg_Month + KM + Fuel_Type + HP + Met_Color + Color + Automatic +
               CC + Doors + Gears + Quarterly_Tax + Weight + Mfr_Guarantee + BOVAG_Guarantee + 
               Guarantee_Period + ABS + Airbag_1 + Airbag_2 + Airco + Automatic_airco + 
               Boardcomputer + CD_Player + Central_Lock + Powered_Windows + Power_Steering + 
               Radio + Mistlamps + Sport_Model + Backseat_Divider + Metallic_Rim + 
               Radio_cassette + Parking_Assistant + Tow_Bar, data = toyota.train)

# more efficient equivalent
model1 <- lm(Price ~ ., data = toyota.train)

# summary
summary(model1)

# here we manually eliminate some of the insignificant predictors
model2 <- lm(Price ~ . - Power_Steering - Airbag_1 - Airbag_2 - Mistlamps, data = toyota.train)
summary(model2)

# Model assessment

sum.model1 <- summary(model1)

# MSE
(sum.model1$sigma)^2

# R-squared
sum.model1$r.squared

# adjusted R-squared
sum.model1$adj.r.squared

AIC(model1)
BIC(model1)

## Out-of-sample Prediction

library(forecast)

pred.model0 <- predict(model0, newdata = toyota.valid)
pred.model1 <- predict(model1, newdata = toyota.valid)
pred.model2 <- predict(model2, newdata = toyota.valid)

accuracy(pred.model0, toyota.valid$Price)

accuracy(pred.model1, toyota.valid$Price)
accuracy(pred.model2, toyota.valid$Price)

# Variable Selection

sum0 <- summary(model0)
sum1 <- summary(model1)
sum2 <- summary(model2)

AIC(model0); BIC(model0); sum0$adj.r.squared
AIC(model1); BIC(model1); sum1$adj.r.squared
AIC(model2); BIC(model2); sum2$adj.r.squared

# Exhaustive Search
install.packages("leaps") 
library(leaps)

# Create Binary dummy variables
dummies <- as.data.frame(model.matrix(~ 0 + Fuel_Type + Color, data = toyota))
t(t(names(dummies)))

toyota <- cbind(toyota[, -c(5,8)], dummies[ , -c(1,4)])
t(t(names(toyota)))

## Redefine training and validation set with dummy variables
set.seed(1)
train.rows <- sample(nrow(toyota), nrow(toyota) * 0.8)
toyota.train <- toyota[train.rows, ]
toyota.valid <- toyota[-train.rows, ]
# regsubsets will only accept a data frame as an input
toyota.search <- regsubsets(Price ~ ., data = toyota.train, nbest = 1, nvmax = ncol(toyota.train))
sum <- summary(toyota.search)
sum$which 


# a useful visualization of the models returned by exhaustive search
par(mfrow = c(1, 1))
plot(toyota.search, scale = "Cp")

t(t(sum$cp))

## Forward/Backward/Stepwise Regression Using AIC
toyota.null <- lm(Price ~ 1, data = toyota.train)
toyota.full <- lm(Price ~ ., data = toyota.train)

# Forward Selection
toyota.fwd <- step(toyota.null, scope = list(lower = toyota.null, upper = toyota.full), 
                   direction = "forward")

summary(toyota.fwd)


# Backwards
toyota.back <- step(toyota.full, direction = "backward")
summary(toyota.back)

## Stepwise Regression
toyota.step <- step(toyota.null, scope = list(lower = toyota.null, upper = toyota.full),
                    direction = "both")
summary(toyota.step)
