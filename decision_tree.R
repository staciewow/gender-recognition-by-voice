setwd("/Volumes/STACY JIANG/DS_Spring2017/ML-DATS6202/PROJECT")
voice <- read.csv("voice.csv", header = T, sep = ",")
counts <- summary(voice$label)  #50% male and 50% female, perfectly balanced 

set.seed(258)
#shuffle the dataset and call the result shuffled
n <- nrow(voice)
index <- sample(1:n, n*0.8)
train <- voice[index, ] 
test <- voice[-index, ]
train_x <- train[,-21]
test_x <- test[,-21]
train_y <- train[,21]
test_y <- test[,21]

#decision tree 1
library(rpart)
fit1 <- rpart(label ~., data = train, method = "class") #since the "label" is a factor, method set to be class
summary(fit1)
pred1 <- predict(fit1, test_x, type = "class")
confusionMatrix(pred1,test$label)
library(rattle)
fancyRpartPlot(fit1)

#decision tree 2
trControl <- trainControl(method = "cv",  number=10, verboseIter = FALSE)
fit2 <- suppressWarnings(caret::train(label ~ ., 
                                      data = train, 
                                      method = "C5.0",
                                      preProcess = c("BoxCox", "center", "scale", "pca"),
                                      tuneLength = 7, 
                                      trControl = trControl,
                                      metric = 'Accuracy'))
summary(fit2)
pred2 <- predict(fit2, test_x)
confusionMatrix(pred2,test$label)


#decision tree 2
trControl <- trainControl(method = "cv",  number=10, verboseIter = FALSE)
fit3 <- suppressWarnings(caret::train(label ~ ., 
                                      data = train, 
                                      method = "C5.0",
                                      preProcess = c("BoxCox", "center", "scale", "nzv"),
                                      tuneLength = 7, 
                                      trControl = trControl,
                                      metric = 'Accuracy'))
summary(fit3)
pred3 <- predict(fit3, test_x)
confusionMatrix(pred3,test$label)