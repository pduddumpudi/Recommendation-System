library(psych)	
library(dbplyr)	
library(dtplyr)	
library(data.table)	
library(ggplot2)	
library(Hmisc)	
library(tidyverse)	
library(gmodels)	
library(dplyr)	
library(data.table)	
library("matrixStats")	
library('conflicted')	
library(pwr)	
library('car')	
conflict_prefer("count", "dplyr")	
conflict_prefer("filter", "dplyr")	
library("readxl")	
library("PMCMRplus")
library(PASWR2)
library("jtools")
library("huxtable")
library("knitr")
library("recommenderlab")
conflict_prefer("as.matrix", "proxy")


##########################################################################
############################# PART 1 #####################################
##########################################################################
rm(list = ls())

setwd("C:\\Users\\mahen\\OneDrive - University of California, Davis\\Desktop\\School\\BAX 401 Fall - Information, Insight and Impact\\Class 8\\HW4\\")


HW4_Data_2021_Modified <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 1")

summary(HW4_Data_2021_Modified$Count)

#On an average everyone rated approximately 16 movies.



Onlyratings<- HW4_Data_2021_Modified[,5:ncol(HW4_Data_2021_Modified)]

user_average_ratings <- rowMeans(Onlyratings, na.rm = TRUE)

hist(user_average_ratings, main = "Distribution of the average rating per user",
     col = "blue")


matrix_rating <- as.matrix(Onlyratings)


matrix_rating <- as(matrix_rating,"realRatingMatrix")

evaluation <- evaluationScheme(matrix_rating, method="split", train=0.8, given=3, goodRating=3)



#COSINE - User-User
set.seed(5)
User_User_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                         param=list(normalize = NULL, method="Cosine"))

User_User_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                             param=list(normalize = "center",method="Cosine"))

User_User_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_1 <- predict(User_User_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_2 <- predict(User_User_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_3 <- predict(User_User_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods <- rbind(
  User_User_Nonormal_Cosine = calcPredictionAccuracy(predict_1, getData(evaluation, "unknown")),
  User_User_meancentered_Cosine = calcPredictionAccuracy(predict_2, getData(evaluation, "unknown")),
  User_User_Zscored_Cosine  = calcPredictionAccuracy(predict_3, getData(evaluation, "unknown"))
)
kable(error_User_methods)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.


#Euclidean - User-User
set.seed(5)
User_User_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))
set.seed(5)
User_User_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                                param=list(normalize = "center",method="Euclidean"))
set.seed(5)
User_User_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_4 <- predict(User_User_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_5 <- predict(User_User_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_6 <- predict(User_User_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean <- rbind(
  User_User_Nonormal_Euclidean = calcPredictionAccuracy(predict_4, getData(evaluation, "unknown")),
  User_User_meancentered_Euclidean = calcPredictionAccuracy(predict_5, getData(evaluation, "unknown")),
  User_User_Zscored_Euclidean  = calcPredictionAccuracy(predict_6, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.



#COSINE - Item-Item

Item_Item_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                         param=list(normalize = NULL, method="Cosine"))

Item_Item_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                             param=list(normalize = "center",method="Cosine"))

Item_Item_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_7 <- predict(Item_Item_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods_Item <- rbind(
  Item_Item_Nonormal_Cosine = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Cosine= calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Cosine  = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Item)

#Based on root mean squared characteristics Item-Item non normalization performed better compared to other two methods.



#Euclidean - Item-Item

Item_Item_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))

Item_Item_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                                param=list(normalize = "center",method="Euclidean"))

Item_Item_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_7 <- predict(Item_Item_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean_Item <- rbind(
  Item_Item_Nonormal_Euclidean = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Euclidean = calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Euclidean = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean_Item)

#Based on root mean squared characteristics Item-Item Z-score performed better compared to other two methods.


conclusion_results <- data.frame(rbind(error_User_methods,error_User_methods_Euclidean,error_User_methods_Item,error_User_methods_Euclidean_Item
))

conclusion_results <- conclusion_results[order(conclusion_results$RMSE ),]

conclusion_results <- conclusion_results[1:(nrow(conclusion_results)-2),]

kable(conclusion_results)

barplot(conclusion_results$RMSE, col = "white", main = "Barplot of Model RMSE's", las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(conclusion_results),cex.names=.6)


###########The best model is User-User Non normalized using Euclidean distance


#####Predicting using best model

predict_ratings<- predict(User_User_Nonormal_Cosine,matrix_rating , type="ratings")

predict_ratings_output <- as(predict_ratings, "matrix")
predict_ratings_output <- as.data.frame(predict_ratings_output)
write.csv(conclusion_results,"part1_RMSE.csv")
write.csv(predict_ratings_output,"Part1_Ratings.csv")

##########################################################################
############################# PART 2 #####################################
##########################################################################

rm(list = ls())

setwd("C:\\Users\\mahen\\OneDrive - University of California, Davis\\Desktop\\School\\BAX 401 Fall - Information, Insight and Impact\\Class 8\\HW4\\")


HW4_Data_2021_Modified <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 2")

summary(HW4_Data_2021_Modified$Count)

#On an average everyone rated approximately 16 movies.



Onlyratings<- HW4_Data_2021_Modified[,5:ncol(HW4_Data_2021_Modified)]

user_average_ratings <- rowMeans(Onlyratings, na.rm = TRUE)

hist(user_average_ratings, main = "Distribution of the average rating per user",
     col = "blue")


matrix_rating <- as.matrix(Onlyratings)


matrix_rating <- as(matrix_rating,"realRatingMatrix")

evaluation <- evaluationScheme(matrix_rating, method="split", train=0.95, given=1, goodRating=3)



#COSINE - User-User
set.seed(5)
User_User_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                         param=list(normalize = NULL, method="Cosine"))
set.seed(5)
User_User_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                             param=list(normalize = "center",method="Cosine"))
set.seed(5)
User_User_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_1 <- predict(User_User_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_2 <- predict(User_User_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_3 <- predict(User_User_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods <- rbind(
  User_User_Nonormal_Cosine = calcPredictionAccuracy(predict_1, getData(evaluation, "unknown")),
  User_User_meancentered_Cosine = calcPredictionAccuracy(predict_2, getData(evaluation, "unknown")),
  User_User_Zscored_Cosine  = calcPredictionAccuracy(predict_3, getData(evaluation, "unknown"))
)
kable(error_User_methods)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.


#Euclidean - User-User
set.seed(5)
User_User_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))
set.seed(5)
User_User_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                                param=list(normalize = "center",method="Euclidean"))
set.seed(5)
User_User_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_4 <- predict(User_User_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_5 <- predict(User_User_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_6 <- predict(User_User_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean <- rbind(
  User_User_Nonormal_Euclidean = calcPredictionAccuracy(predict_4, getData(evaluation, "unknown")),
  User_User_meancentered_Euclidean = calcPredictionAccuracy(predict_5, getData(evaluation, "unknown")),
  User_User_Zscored_Euclidean  = calcPredictionAccuracy(predict_6, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.



#COSINE - Item-Item
set.seed(5)
Item_Item_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                         param=list(normalize = NULL, method="Cosine"))
set.seed(5)
Item_Item_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                             param=list(normalize = "center",method="Cosine"))
set.seed(5)
Item_Item_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_7 <- predict(Item_Item_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods_Item <- rbind(
  Item_Item_Nonormal_Cosine = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Cosine= calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Cosine  = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Item)

#Based on root mean squared characteristics Item-Item non normalization performed better compared to other two methods.



#Euclidean - Item-Item
set.seed(5)
Item_Item_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))
set.seed(5)
Item_Item_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                                param=list(normalize = "center",method="Euclidean"))
set.seed(5)
Item_Item_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_7 <- predict(Item_Item_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean_Item <- rbind(
  Item_Item_Nonormal_Euclidean = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Euclidean = calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Euclidean = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean_Item)

#Based on root mean squared characteristics Item-Item Z-score performed better compared to other two methods.


conclusion_results <- data.frame(rbind(error_User_methods,error_User_methods_Euclidean,error_User_methods_Item,error_User_methods_Euclidean_Item
))

conclusion_results <- conclusion_results[order(conclusion_results$RMSE ),]

conclusion_results <- conclusion_results[1:(nrow(conclusion_results)-1),]

kable(conclusion_results)

barplot(conclusion_results$RMSE, col = "white", main = "Barplot of Model RMSE's", las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(conclusion_results),cex.names=.6)


###########The best model is User-User Non normalized using Euclidean distance


#####Predicting using best model

predict_ratings<- predict(Item_Item_Nonormal_Cosine,matrix_rating , type="ratings")

predict_ratings_output <- as(predict_ratings, "matrix")
predict_ratings_output <- as.data.frame(predict_ratings_output)
write.csv(conclusion_results,"part2_RMSE.csv")
write.csv(predict_ratings_output,"Part2_Ratings.csv")


##########################################################################
############################# PART 4 #####################################
##########################################################################
rm(list = ls())

setwd("C:\\Users\\mahen\\OneDrive - University of California, Davis\\Desktop\\School\\BAX 401 Fall - Information, Insight and Impact\\Class 8\\HW4\\")


HW4_Data_2021_Modified <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 4")

summary(HW4_Data_2021_Modified$Count)

#On an average everyone rated approximately 16 movies.



Onlyratings<- HW4_Data_2021_Modified[,5:ncol(HW4_Data_2021_Modified)]

user_average_ratings <- rowMeans(Onlyratings, na.rm = TRUE)

hist(user_average_ratings, main = "Distribution of the average rating per user",
     col = "blue")


matrix_rating <- as.matrix(Onlyratings)


matrix_rating <- as(matrix_rating,"realRatingMatrix")

evaluation <- evaluationScheme(matrix_rating, method="split", train=0.8, given=3, goodRating=3)



#COSINE - User-User
set.seed(5)
User_User_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                         param=list(normalize = NULL, method="Cosine"))

User_User_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                             param=list(normalize = "center",method="Cosine"))

User_User_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "UBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_1 <- predict(User_User_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_2 <- predict(User_User_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_3 <- predict(User_User_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods <- rbind(
  User_User_Nonormal_Cosine = calcPredictionAccuracy(predict_1, getData(evaluation, "unknown")),
  User_User_meancentered_Cosine = calcPredictionAccuracy(predict_2, getData(evaluation, "unknown")),
  User_User_Zscored_Cosine  = calcPredictionAccuracy(predict_3, getData(evaluation, "unknown"))
)
kable(error_User_methods)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.


#Euclidean - User-User
set.seed(5)
User_User_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))
set.seed(5)
User_User_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                                param=list(normalize = "center",method="Euclidean"))
set.seed(5)
User_User_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "UBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_4 <- predict(User_User_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_5 <- predict(User_User_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_6 <- predict(User_User_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean <- rbind(
  User_User_Nonormal_Euclidean = calcPredictionAccuracy(predict_4, getData(evaluation, "unknown")),
  User_User_meancentered_Euclidean = calcPredictionAccuracy(predict_5, getData(evaluation, "unknown")),
  User_User_Zscored_Euclidean  = calcPredictionAccuracy(predict_6, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean)

#Based on root mean squared characteristics User-User non normalization performed better compared to other two methods.



#COSINE - Item-Item

Item_Item_Nonormal_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                         param=list(normalize = NULL, method="Cosine"))

Item_Item_meancentered_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                             param=list(normalize = "center",method="Cosine"))

Item_Item_Zscored_Cosine <- Recommender(getData(evaluation, "train"), "IBCF", 
                                        param=list(normalize = "Z-score",method="Cosine"))



predict_7 <- predict(Item_Item_Nonormal_Cosine, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Cosine, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Cosine, getData(evaluation, "known"), type="ratings")



error_User_methods_Item <- rbind(
  Item_Item_Nonormal_Cosine = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Cosine= calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Cosine  = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Item)

#Based on root mean squared characteristics Item-Item non normalization performed better compared to other two methods.



#Euclidean - Item-Item

Item_Item_Nonormal_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                            param=list(normalize = NULL, method="Euclidean"))

Item_Item_meancentered_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                                param=list(normalize = "center",method="Euclidean"))

Item_Item_Zscored_Euclidean <- Recommender(getData(evaluation, "train"), "IBCF", 
                                           param=list(normalize = "Z-score",method="Euclidean"))



predict_7 <- predict(Item_Item_Nonormal_Euclidean, getData(evaluation, "known"), type="ratings")

predict_8 <- predict(Item_Item_meancentered_Euclidean, getData(evaluation, "known"), type="ratings")

predict_9 <- predict(Item_Item_Zscored_Euclidean, getData(evaluation, "known"), type="ratings")



error_User_methods_Euclidean_Item <- rbind(
  Item_Item_Nonormal_Euclidean = calcPredictionAccuracy(predict_7, getData(evaluation, "unknown")),
  Item_Item_meancentered_Euclidean = calcPredictionAccuracy(predict_8, getData(evaluation, "unknown")),
  Item_Item_Zscored_Euclidean = calcPredictionAccuracy(predict_9, getData(evaluation, "unknown"))
)
kable(error_User_methods_Euclidean_Item)

#Based on root mean squared characteristics Item-Item Z-score performed better compared to other two methods.


conclusion_results <- data.frame(rbind(error_User_methods,error_User_methods_Euclidean,error_User_methods_Item,error_User_methods_Euclidean_Item
))

conclusion_results <- conclusion_results[order(conclusion_results$RMSE ),]

conclusion_results <- conclusion_results[1:(nrow(conclusion_results)-2),]

kable(conclusion_results)

barplot(conclusion_results$RMSE, col = "white", main = "Barplot of Model RMSE's", las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(conclusion_results),cex.names=.6)


###########The best model is User-User Non normalized using Euclidean distance


#####Predicting using best model

predict_ratings<- predict(User_User_Nonormal_Cosine,matrix_rating , type="ratings")

predict_ratings_output <- as(predict_ratings, "matrix")
predict_ratings_output <- as.data.frame(predict_ratings_output)
write.csv(conclusion_results,"part4_RMSE.csv")
write.csv(predict_ratings_output,"Part4_Ratings.csv")

