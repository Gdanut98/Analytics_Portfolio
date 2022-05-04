#read file into R
IMDB.df <- read.csv("IMDB Movie Dataset.csv")
#explore some early facts about the dataset
summary(Filter(is.numeric,IMDB.df)) 
#budget has huge outliers, and there's a lot of missing data

## deleting rows with missing values
IMDB.df <- IMDB.df[!(is.na(IMDB.df$num_critic_for_reviews) |
                       is.na(IMDB.df$director_facebook_likes) |
                       is.na(IMDB.df$actor_3_facebook_likes) |
                       is.na(IMDB.df$actor_1_facebook_likes) |
                       is.na(IMDB.df$facenumber_in_poster) |
                       is.na(IMDB.df$num_user_for_reviews) |
                       is.na(IMDB.df$title_year) |
                       is.na(IMDB.df$duration) |
                       is.na(IMDB.df$actor_2_facebook_likes)), ]

#imputing median values into numerical data                 
IMDB.df$gross[is.na(IMDB.df$gross)] <- median(IMDB.df$gross, na.rm = TRUE)
IMDB.df$budget[is.na(IMDB.df$budget)] <- median(IMDB.df$budget, na.rm = TRUE)
IMDB.df$aspect_ratio[is.na(IMDB.df$aspect_ratio)] <- median(IMDB.df$aspect_ratio, na.rm = TRUE)

# summary statistics for IMDB data
summary(Filter(is.numeric,IMDB.df))

#doing some exploratory analysis
#Boxplot for imdb score by country
boxplot(IMDB.df$imdb_score ~ IMDB.df$country, color = "blue", xlab = "Country", ylab = "IMDB Score", main = "Distribution of IMDB Scores by Country")
#Boxplot for imdb score by color
boxplot(IMDB.df$imdb_score ~ IMDB.df$ï..color, color = "blue", xlab = "Color", ylab = "IMDB Score", main = "Distribution of IMDB Score by Color")
#Boxplot for number of voted users by color
boxplot(IMDB.df$num_voted_users ~ IMDB.df$ï..color, color = "blue", xlab = "Color", ylab = "# Voted Users", main = "Distribution of Number of Voted Users by Color")
#Boxplot for imdb score by content rating
boxplot(IMDB.df$imdb_score ~ IMDB.df$content_rating, color = "blue", xlab = "Content Rating", ylab = "IMDB Score", main = "Distribution of IMDB Score by Content Rating")
#Boxplot for budget
boxplot(IMDB.df$budget, color = "blue", ylab = "Budget", main = "Distribution of Budget")
#this boxplot displays the large amount of outliers in the dataset for budget

# correlation matrix for IMDB Movie Dataset
round(cor(Filter(is.numeric,IMDB.df)), 2)

#create a correlation matrix with a heat map to explore relationships within the database
library(gplots)
colfunc <- colorRampPalette(c("red", "white", "green"))
heatmap.2(round(cor(Filter(is.numeric, IMDB.df), use = "complete.obs"), 2), Rowv = FALSE, 
          Colv = FALSE, dendrogram = "none", col = colfunc(15), 
          lwid=c(0.1,4), lhei=c(0.1,4), cellnote = round(cor(Filter(is.numeric, IMDB.df), use = "complete.obs"), 2), 
          notecol = "black", key = FALSE, trace = "none", margins = c(10, 10))
          round(cor(Filter(is.numeric,IMDB.df)), 2)

#Creating dummy variables
#keywords
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$plot_keywords),split = "|", fixed = TRUE))))
View(parse_key)
#list the 20 most frequent keywords
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 20)

#top 5 most frequent keywords:
IMDB.df$key_love <- ifelse(grepl("love", IMDB.df$plot_keywords), 1,0)
IMDB.df$key_friend <- ifelse(grepl("friend", IMDB.df$plot_keywords), 1,0)
IMDB.df$key_murder <- ifelse(grepl("murder", IMDB.df$plot_keywords), 1,0)
IMDB.df$key_death <- ifelse(grepl("death", IMDB.df$plot_keywords), 1,0)
IMDB.df$key_police <- ifelse(grepl("police", IMDB.df$plot_keywords), 1,0)


#Color
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$color),split = "|", fixed = TRUE))))
#fequency of black or white
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 2)
IMDB.df$key_color <- ifelse(grepl("color", IMDB.df$plot_keywords), 1,0)

#variable names
View(parse_key)
t(t(names(IMDB.df)))

#content rating
parse_key <- data.frame(table(unlist(strsplit(as.character(IMDB.df$content_rating),split = "|", fixed = TRUE))))
View(parse_key)
head(parse_key[order(parse_key$Freq, decreasing = TRUE), ], 15)

IMDB.df$key_R <- ifelse(grepl("R", IMDB.df$content_rating), 1,0)
IMDB.df$key_13 <- ifelse(grepl("PG-13", IMDB.df$content_rating), 1,0)
IMDB.df$key_PG <- ifelse(grepl("PG", IMDB.df$content_rating), 1,0)
IMDB.df$key_NR <- ifelse(grepl("Not Rated", IMDB.df$content_rating), 1,0)
IMDB.df$key_G <- ifelse(grepl("G", IMDB.df$content_rating), 1,0)
IMDB.df$key_Unrated <- ifelse(grepl("Unrated", IMDB.df$content_rating), 1,0)
IMDB.df$key_Approved <- ifelse(grepl("Approved", IMDB.df$content_rating), 1,0)

#Create a new dataset with these variables          
IMDBnew.df <- IMDB.df[, c(3:6, 8:9, 13:14, 16, 19, 24:41)]
View(IMDBnew.df)


# partitioning the data
set.seed(1)
train.rows <- sample(rownames(IMDBnew.df), nrow(IMDBnew.df)*0.6)
train.data <- IMDBnew.df[train.rows, ]
valid.rows <- setdiff(rownames(IMDBnew.df), train.rows)
valid.data <- IMDBnew.df[valid.rows, ]

head(train.data)
head(valid.data)

#linear model
IMDB.lm <- lm(imdb_score ~ ., data = train.data)
options(scipen = 999)
summary(IMDB.lm)

#predictions
library(forecast)
valid.lm.pred <- predict(IMDB.lm, valid.data)
options(scipen = 999, digits = 2)
# calculate the residuals
valid.resid <- valid.data$imdb_score - valid.lm.pred
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$imdb_score[1:20],
           "Residual" = valid.resid[1:20])
#accuracy measures
accuracy(valid.lm.pred, valid.data$imdb_score)

#distribution of residuals
library(tidyverse)
ggplot(data = as.data.frame(valid.resid), aes(y = valid.resid)) + geom_boxplot()


#exhaustive search
library(leaps)
search <- regsubsets(imdb_score ~ ., data = train.data, nbest = 1, 
                     nvmax = dim(train.data)[2], method = "exhaustive")
sum <- summary(search)
sum$which
options(scipen = 999, digits = 2)
t(t(sum$rsq))
t(t(sum$adjr2))
t(t(sum$cp))

IMDB.lm.null <- lm(imdb_score ~ 1, data = train.data)

#Forward
IMDB.lm.fwd <- step(IMDB.lm.null, scope = list(IMDB.lm.null, upper = IMDB.lm), 
                    direction = "forward")
summary(IMDB.lm.fwd)
#Prediction
pred.IMDB.lm.fwd <- predict(IMDB.lm.fwd, newdata = valid.data)
options(scipen = 999, digits = 2)
# calculate the residuals
valid.resid.fwd <- valid.data$imdb_score - pred.IMDB.lm.fwd
# look at the first 20 residuals
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$imdb_score[1:20],
           "Residual" = valid.resid.fwd[1:20])
#accuracy
accuracy(pred.IMDB.lm.fwd, valid.data$imdb_score)

#Backward
IMDB.lm.back <- step(IMDB.lm, direction = "backward")
summary(IMDB.lm.back)
#Prediction
pred.IMDB.lm.back <- predict(IMDB.lm.back, newdata = valid.data)
options(scipen = 999, digits = 2)
# calculate the residuals
valid.resid.back <- valid.data$imdb_score - pred.IMDB.lm.back
# look at the first 20 residuals
data.frame("Predicted" = pred.IMDB.lm.back[1:20], "Actual" = valid.data$imdb_score[1:20],
           "Residual" = valid.resid.back[1:20])
#accuracy
accuracy(pred.IMDB.lm.back, valid.data$imdb_score)

#Stepwise
IMDB.lm.step <- step(IMDB.lm.null, scope = list(IMDB.lm.null, upper = IMDB.lm), 
                     direction = "both")
summary(IMDB.lm.step)
#Prediction
pred.IMDB.lm.step <- predict(IMDB.lm.step, newdata = valid.data)
options(scipen = 999, digits = 2)
# calculate the residuals
valid.resid.step <- valid.data$imdb_score - pred.IMDB.lm.step
# look at the first 20 residuals
data.frame("Predicted" = pred.IMDB.lm.step[1:20], "Actual" = valid.data$imdb_score[1:20],
           "Residual" = valid.resid.step[1:20])
#accuracy
accuracy(pred.IMDB.lm.step, valid.data$imdb_score)
