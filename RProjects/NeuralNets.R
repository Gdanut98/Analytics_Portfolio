# Import Data

df <- read.csv("tinydata.csv")

## neural network with a single hidden layer of three nodes
install.packages("neuralnet")
library(neuralnet)

nn <- neuralnet(Acceptance ~ Salt + Fat, data = df, 
                linear.output = FALSE, hidden = 3)

# display weights
nn$weights
# display predictions
prediction(nn)
# plot network
plot(nn, rep = "best")

## confusion matrix
library(caret)
predict <- compute(nn, data.frame(df$Salt, df$Fat))
predicted.class <- apply(predict$net.result, 1, which.max) - 1
confusionMatrix(as.factor(ifelse(predicted.class == 1, "like", "dislike")), 
                df$Acceptance, positive = "like")
