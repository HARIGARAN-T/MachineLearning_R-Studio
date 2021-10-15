# importing the data set
google_data <- read.csv("C:\\Users\\my pc\\Desktop\\MBA-BA\\googleplaystore.csv")
head(google_data,5) # inspecting first 5 rows
dim(google_data)    # dimension of data frame
summary(google_data)# summary of data frame
boxplot(google_data$Rating)
hist(google_data$Rating)
is.na.data.frame(google_data)
sum(is.na.data.frame(google_data))  
# clean outlier in rating
attach(google_data)
summary(Rating)
iqr <- 4.5- 4
upfen_rat <- 4.5+1.5*iqr
upfen_rat
summary(google_data)
google_data_1 <- subset(google_data,google_data$Rating <= upfen_rat| is.na(google_data$Rating))
boxplot(google_data_1$Rating)
hist(google_data_1$Rating)

google_data_1$Rating[is.na(google_data_1$Rating)] <- median(google_data_1$Rating,na.rm = TRUE)
sum(is.na(google_data_1$Rating))
summary(google_data_1)
str(google_data_1)



#scaling and cleaning size of installation
changesize <- function(Size){
  if (grepl("M", Size, fixed = TRUE)){
    x=substr(Size,1,nchar(Size)-1)
    x=as.double(x)
    x=x*100000
    return(x)
  }
  else if (grepl("k", Size, fixed = TRUE)){
    x=substr(Size,1,nchar(Size)-1)
    x=as.double(x)
    x=x*1000
    return(x)
  }
  else{
    x <- 0
    return(x)
  }
}
google_data_1$Size <- changesize(google_data_1$Size)
is.na(google_data_1$Size)
sum(is.na(google_data_1$Size))
google_data_1$Size[is.na(google_data_1$Size)] <- mean(google_data_1$Size,na.rm = TRUE)
sum(is.na(google_data_1$Size))

#cleaning  istall
google_data_1$Installs <- gsub("\\+","",google_data_1$Installs)
google_data_1$Installs <- gsub("\\,","",google_data_1$Installs)
google_data_1$Installs <- as.double(google_data_1$Installs)



#cleaning content rating classification

summary(google_data_1$Content.Rating)
google_data_1$Content.Rating <- factor(google_data_1$Content.Rating)
levels(google_data_1$Content.Rating)
nlevels(google_data_1$Content.Rating)
google_data_1$Content.Rating_c <- as.numeric(as.factor(google_data_1$Content.Rating))

# cleaning category
google_data_1$Category <- factor(google_data_1$Category)
#cleaning of genres
summary(google_data_1$Genres)
google_data_1$Genres <- factor(google_data_1$Genres)
levels(google_data_1$Genres)
nlevels(google_data_1$Genres)
google_data_1$Genres_c <- as.numeric(as.factor(google_data_1$Genres))

# cleaning prices
google_data_1$Price <- as.numeric(gsub("\\$","",google_data_1$Price))

#cleaning reviews
google_data_1$Reviews <- as.numeric(google_data_1$Reviews)

# dropping of unrelated/ unnecessary column

google_data_1 <- google_data_1[,-c(1,11,12,13)]

#dummy variables for encoding 
library(fastDummies)


#converting the types - dummy
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(google_data_1$Type)

google_data_1$Type[is.na(google_data_1$Type)] <- Mode(google_data_1$Type)
sum(is.na(google_data_1$Type))

#dummy variable for category and types
google_data_1$Type <-replace(google_data_1$Type,google_data_1$Type == "NaN",Mode(google_data_1$Type))
google_data_2 <- dummy_cols(google_data_1,select_columns = c("Type","Category"))


# dropping
google_data_2<- google_data_2[,-c(1,6,8,9)]

#correlation matrix
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
cormat <- round(x=cor(google_data_2[1:5]))
melt.cormat<- melt(cormat)
head(melt.cormat)
ggplot(data = melt.cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
chart.Correlation(google_data_2[,c(1:5)],histogram = TRUE,method = "pearson",pch=19)


############ RANDOM_FOREST ##############


#load required libraries
library(stats)
library(dplyr)
library(randomForest)
library(Metrics)
# data interpretation
View(google_data_2)
str(google_data_2)
index <- sample(2,nrow(google_data_2),replace = TRUE,prob = c(0.7,0.3))

#training data
training <- google_data_2[index==1,]
#testing data
testing <- google_data_2[index==2,]

# random forest model
RFM <- randomForest(Rating~.,data=training)
print(RFM)
attributes(RFM)

#prediction with train data
p1 <- predict(RFM,training[,-1])
par(mar=c(1,1,1,1))
plot(training$Rating,p1,xlab = "Rating(training)",ylab = "Predicted Rating")
training_set_predict <- data.frame(Actual_Value<- training$Rating,Predict_Value <- p1)
training_set_predict
mean_absolute_error_training <- mae(training$Rating,p1)
mean_squared_error_training <- mse(training$Rating,p1)
root_squared_error_training <-rmse(training$Rating,p1)
mean_absolute_error_training
mean_squared_error_training
root_squared_error_training
#prediction test data set
p2<- predict(RFM,testing[,-1])
plot(testing$Rating,p2,xlab = "Rating(testing)",ylab = "Predicted Rating")
testing_set_predict <- data.frame(Actual_Value <-testing$Rating,Predict_Value <- p2)
testing_set_predict
mean_absolute_error_testing <- mae(testing$Rating,p2)
mean_squared_error_testing<- mse(testing$Rating,p2)
root_squared_error_testing <-rmse(testing$Rating,p2)
mean_absolute_error_training
mean_squared_error_training
root_squared_error_training
#error rate of our random forest model
plot(RFM)

#no. nodes in tree
hist(treesize(RFM),main="no. of noodes in tree",col="green")

#variable importance
varImpPlot(RFM)
importance(RFM)



#save model to RDS file
saveRDS(RFM,"RFM.rds")
