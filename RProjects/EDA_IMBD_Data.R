# summary statistics for IMDB Data
summary(IMDB_Movie_Dataset)
# remove categorical data
IMDB_Movie_Dataset$color <- NULL
IMDB_Movie_Dataset$director_name <- IMDB_Movie_Dataset$actor_2_name <- IMDB_Movie_Dataset$genres <- IMDB_Movie_Dataset$actor_1_name <- IMDB_Movie_Dataset$movie_title <- IMDB_Movie_Dataset$actor_3_name <- IMDB_Movie_Dataset$plot_keywords <- IMDB_Movie_Dataset$language <- IMDB_Movie_Dataset$country <- IMDB_Movie_Dataset$content_rating <- NULL
IMDB_Movie_Dataset$movie_imdb_link <- NULL # forgot one

# removing missing data in variableb that 25 or less missing points
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$duration), ]
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$actor_3_facebook_likes), ]
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$actor_2_facebook_likes, ]
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$actor_1_facebook_likes), ]
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$num_user_for_reviews), ]
IMDB_Movie_Dataset <- IMDB_Movie_Dataset[!is.na(IMDB_Movie_Dataset$facenumber_in_poster), ]
# set the median for data of variables that have great than 25 missing points
IMDB_Movie_Dataset$num_critic_for_reviews[is.na(IMDB_Movie_Dataset$num_critic_for_reviews)] <- median(IMDB_Movie_Dataset$num_critic_for_reviews , na.rm= TRUE) 
IMDB_Movie_Dataset$director_facebook_likes[is.na(IMDB_Movie_Dataset$director_facebook_likes)] <- median(IMDB_Movie_Dataset$director_facebook_likes , na.rm= TRUE) 
IMDB_Movie_Dataset$num_critic_for_reviews[is.na(IMDB_Movie_Dataset$num_critic_for_reviews)] <- median(IMDB_Movie_Dataset$num_critic_for_reviews , na.rm= TRUE) 
IMDB_Movie_Dataset$aspect_ratio[is.na(IMDB_Movie_Dataset$aspect_ratio)] <- median(IMDB_Movie_Dataset$aspect_ratio , na.rm= TRUE) 
IMDB_Movie_Dataset$title_year[is.na(IMDB_Movie_Dataset$title_year)] <- median(IMDB_Movie_Dataset$title_year , na.rm= TRUE) 
IMDB_Movie_Dataset$budget[is.na(IMDB_Movie_Dataset$budget)] <- median(IMDB_Movie_Dataset$budget , na.rm= TRUE) 
IMDB_Movie_Dataset$gross[is.na(IMDB_Movie_Dataset$gross)] <- median(IMDB_Movie_Dataset$gross , na.rm= TRUE) 
# scatterplot matrix with correlation
install.packages("GGally") # do this only once
library(GGally)
ggpairs(IMDB_Movie_Dataset)
# correlation heatmap 
install.packages("gplots") # only needed once
library(gplots)
colfunc <- colorRampPalette(c("green", "white","red"))
heatmap.2(cor(IMDB_Movie_Dataset), Rowv= FALSE, Colv= FALSE, dendrogram= "none", col=colfunc(15),cellnote= round(cor(IMDB_Movie_Dataset), 2), notecol="black", key= FALSE, trace= "none", margins=c(10,10))
# histogram for Duration
hist(IMDB_Movie_Dataset$duration, xlab= "Duration in minutes")
# boxplot for IMDB scores based on critic reviews
boxplot(IMDB_Movie_Dataset$num_critic_for_reviews ~ IMDB_Movie_Dataset$imdb_score, xlab="Critic reviews" , ylab="IMDB scored")

# scatter plotting number of voted users with IMDB Score
plot(IMDB_Movie_Dataset[, c(13, 26)])

# scatter plotting duration of movie with IMDB Score
plot(IMDB_Movie_Dataset[, c(4, 26)])

# creating a box plot of IMDB ratings by release date
boxplot(IMDB_Movie_Dataset$title_year ~ IMDB_Movie_Dataset$imdb_score, xlab = "imdb_score", ylab = "title_year")

# creating a bar chart of average IMDB ratings by Genre
data.for.plot <- aggregate(IMDB_Movie_Dataset$imdb_score, by = listIMDB_Movie_Datasetb$genres), FUN = mean)
names(data.for.plot) <- c("genres", "MeanImdb_score")
barplot(data.for.plot$MeanImdb_score, names.arg = data.for.plot$genres, xlab= "genres", ylab = "MeanImdb_score")

#creating a more specific bar chart with average IMDB ratings for the first 6 genres of filmss
barplot(head(data.for.plot$MeanImdb_score, names.arg = data.for.plot$genres, ), names.arg=c("Action", "Action|Adventure", "Act|Adv|An|Com|Crime|Fam|Fan", "Act|Adv|An|Com|Dra|Fam|Fan|Thril", "Act|Adv|An|Com|Dra|Fam|Sci-Fi", "Act|Adv|An|Com|Fam"), xlab= "genres", ylab = "MeanImdb_score")
        