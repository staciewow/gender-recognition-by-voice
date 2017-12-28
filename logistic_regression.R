#import data
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

fit <- glm(label ~ ., family = binomial(link = "logit"), data = train)
summary(fit)
#converting log-odds to probability:
exp(coef(fit))/(1+exp(coef(fit)))*100

#Goodness of Fit 1
#with linear regression, R^2 tells us the proportion of variance in the dependent variable that is explained by the predictor.
#with logistic regression, R^2 is valueable as well, such as McFadden's R^2. 
#McFadden's R^2 = 1 - ln(Lm)/Ln(L0), 
#where Ln(Lm) is the log likelihood for fitted model and L0 is likelihood for null model with only an intercept as a predictor
#McFadden's R^2 is between 0 and 1. And "0" means no predictive power.
library(pscl)
pR2(model)

#Goodness of Fit 2
#Hosmer-Lemeshow 
#smaller p-value indicates a good fit to the data while large p-value indicates a poor fit
library(ResourceSelection)
hoslem.test(train$label, fitted(fit))

#Tests of individual predictor 1: Wald Test
#Wald test: to evaluate the statistical significance of each coefficient in the model  
#it is calculated by taking the ratio of the square of the regression coefficient to the square of the standard error of the coefficient.
#for example
library(survey)
regTermTest(fit, "mindom")
regTermTest(fit, "meanfun")

#Tests of individual predictor 2: Variable Importance
#it calculate absoluate values of t-statistics for each parameter.
require(caret)
varImp(fit)

pred <- predict(model, newdata = test, type = 'response')
summary(pred)

prediction <- ifelse(prediction > 0.5, 1, 0)
glm_table <- table(predict=prediction , truth=test_y )
glm_table

