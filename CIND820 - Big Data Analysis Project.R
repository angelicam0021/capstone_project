library(dplyr)
library(MASS)
library(ggplot2)
library(class)
library(gmodels)
library(naivebayes)

###Reading the file
googleApps = read.csv("/Users/user/Documents/CIND820 - Big Data Analytics Project/googleAppsNew4.csv",header =  T, sep = ",")
View(googleApps)
str(googleApps)
summary(googleApps) 

###Turn Rating into factor with three levels
googleApps$Rating <- factor(googleApps$Rating, order = TRUE, levels = c('High','Medium','Low'))
googleApps$Category <- as.factor(googleApps$Category)
googleApps$Genres <- as.factor(googleApps$Genres)


##Replace NAs with median
googleApps$Size[is.na(googleApps$Size)] <- median(googleApps$Size, na.rm = TRUE)


##Correlation between Category and Genres
tbl <- table(googleApps$Category,googleApps$Genres)
chisq.test(tbl)


##Remove Application Name
googleApps <- googleApps[-1]
googleApps <- googleApps[-9] 

##Univariate Analysis
aggregate(googleApps$Reviews,by = list(Rating = googleApps$Rating), FUN =mean)
aggregate(googleApps$Size,by = list(Rating = googleApps$Rating), FUN =mean, na.rm = T)
aggregate(googleApps$Installs,by = list(Rating = googleApps$Rating), FUN =mean)
aggregate(googleApps$Price,by = list(Rating = googleApps$Rating), FUN =mean)


count_Category <- table(googleApps$Category)
count_Rating <- table(googleApps$Rating)
count_Type <- table(googleApps$Type)
count_CR <- table(googleApps$Content.Rating)
count_Genre <- table(googleApps$Genre)


###Relative frequency for Rating
rating.freq = table(count_Rating)
rating.freq
rating.relfreq = rating.freq / nrow(googleApps)
school.relfreq



barplot(count_Rating,main="Rating Distribution", xlab="Rating")

barplot(counts_Category, main="Category Distribution", xlab="Category")  ## Doesn't show all category labels

ggplot(googleApps, aes (x = Category)) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

hist(googleApps$Reviews, xlab = "Reviews", main = "Histogram of Reviews")

hist(googleApps$Size, xlab = "Size", main = "Histogram of Size")

hist(googleApps$Installs, xlab = "Installs", main = "Histogram of Installs")

barplot(count_Type,main="Type Distribution", xlab="Type")

hist(googleApps$Price, xlab = "Price", main = "Histogram of Price")

barplot(count_CR,main="Content Rating Distribution", xlab="Content Rating")


barplot(count_Genre,main="Genre Distribution", xlab="Genre")    ## Doesn't show all Genre levels
ggplot(googleApps, aes (x = Genres)) + geom_bar() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Summary Statistics (Mean, Median, Std)
summary(googleApps) 

## Bivariate Analysis - Relationship between Rating and Independent Variables 
ggplot(googleApps, aes(x = Category, fill = Rating)) + geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
boxplot(googleApps$Reviews ~ googleApps$Rating, col="orange", main="Distribution of Reviews",ylab="Reviews", xlab="Rating") 
boxplot(googleApps$Size ~ googleApps$Rating, col="orange", main="Distribution of Size",ylab="Size", xlab="Rating") 
ggplot(googleApps, aes(x = Type, fill = Rating)) + geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
boxplot(googleApps$Installs ~ googleApps$Rating, col="orange", main="Distribution of Installs",ylab="Installs", xlab="Rating") 
boxplot(googleApps$Price ~ googleApps$Rating, col="orange", main="Distribution of Price",ylab="Price", xlab="Rating") 
ggplot(googleApps, aes(x = googleApps$Content.Rating, fill = Rating)) + geom_bar(position = "dodge") 
ggplot(googleApps, aes(x = Genres, fill = Rating)) + geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 60, hjust = 1))





table(googleApps$Rating)   # The proportions for Rating is uneven. We can see that majority of the rating is High, compared to Medium and Low.

googleApps$rating <- factor(googleApps$Rating,
                        levels = c("High", "Medium", "Low"),
                        labels = c("H", "M","L"))

round(prop.table(table(googleApps$rating)) * 100, digits = 1)

##Feature Scaling
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

googleApps_new <- as.data.frame(lapply(googleApps[3:8], normalize))    
googleApps_new <- cbind(googleApps$rating,googleApps_new)
View(googleApps_new)


##Split the dataset into training and test set
set.seed(123)
index <- sample(1:nrow(googleApps_new), 0.65 *nrow(googleApps_new))
googleApps_train <- googleApps_new[index,]
googleApps_test <- googleApps_new[-index,]
View(googleApps_test_labels)



##Using KNN algorithmn
googleApps_train_labels <- googleApps_train[,1]
googleApps_test_labels <- googleApps_test[,1]


googleApps_test_pred <- knn(train = googleApps_train[,2:7],
                     test = googleApps_test[,2:7],
                     cl = googleApps_train_labels, k=10)

googleApps_test_pred <- knn(train = googleApps_train[,2:7],
                            test = googleApps_test[,2:7],
                            cl = googleApps_train_labels, k=96)

###Confusion Matrix
table(Actual=googleApps_test_labels, Predicted=googleApps_test_pred)
CrossTable(x = googleApps_test_labels, y = googleApps_test_pred, prop.chisq = F)


##Using Naive Bayes Algorithm

cl_naivebayes <- naive_bayes(googleApps_train$`googleApps$rating` ~ ., data = googleApps_train )

y_pred <- predict(cl_naivebayes, newdata = googleApps_test) 
y_pred

##Confusion Matrix
table(googleApps_test$`googleApps$rating`, y_pred) 
CrossTable(x = googleApps_test$`googleApps$rating`,y = y_pred, prop.chisq = F )



