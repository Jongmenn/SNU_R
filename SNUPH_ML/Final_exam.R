pacman::p_load(dplyr,lubridate,ggplot2,caret,glmnet,psych,corrplot,ggcorrplot,gridExtra,
               gbm,e1071,xgboost,pROC,rsample,PRROC,MLmetrics,precrec,naivebayes,doParallel,kernlab,
               ROSE)

library(ISLR)
data(College)
str(College)

# 1.1. Split the data set into a training set and a test set
nrow(College)
set.seed(1234)

#7:3
index<-sample(nrow(College), nrow(College) * 0.7)

train <- College[index,]
test  <- College[-index,]

nrow(train)
nrow(test)

# 1.2. Fit a linear model using least squares on the training set, and report the test error obtained.

#Fit a Linear model 
fit        <- lm(Apps ~ ., data = train)
lm_test.mse<-mean((predict(fit, newdata=test) - test$Apps)^2)
lm_test.mse

# 1.3.Fit a ridge regression model on the training set, with $\lambda$ chosen by

#벌점화회귀 패키지
library(glmnet)

#벌점화 회귀를 위해 행렬형태로 자료 만들어줌
mm.train  <- model.matrix(Apps ~ ., data = train)
mm.test   <- model.matrix(Apps ~ ., data = test)
fit.ridge <- cv.glmnet(mm.train,y=train$Apps, data=train, alpha = 0)
fit.ridge$lambda.min
x11();plot(fit.ridge)
#Prediction
pred.test      <- predict(fit.ridge, newx=mm.test, s = fit.ridge$lambda.min)
ridge_test.mse <- mean((pred.test - test$Apps)^2)
ridge_test.mse

# 1.4. Fit a lasso model on the training set, with  chosen by cross-validation. 
# Report the test error obtained, along with the number of non-zero coefficient estimates.
fit.lasso <- cv.glmnet(mm.train,y=train$Apps, data=train, alpha = 1)
fit.lasso$lambda.min
x11();plot(fit.lasso)

coef(fit.lasso) #축소된 계수들 확인
#Prediction
pred.test      <- predict(fit.lasso, newx=mm.test, s = fit.lasso$lambda.min)
lasso_test.mse <- mean((pred.test - test$Apps)^2)
lasso_test.mse

#1.5. Comment on the results obtained. How accurately can we predict the number of college applications received? 
#     Is there much difference among the test errors resulting from these three approaches?
lm_test.mse
ridge_test.mse
lasso_test.mse

dim(train)
dim(test)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
head(Caravan,2)
dim(Caravan)

train<-Caravan[1:1000,]
test <-Caravan[-c(1:1000),]


#3.2. Fit a boosting model to the training set with “Purshase” as the response and 
#     the other variables as predictors. Use 1,000 trees, and a shrinkage value of 0.01. 
#     Which predictors appear to the most important? Report the predction error on the test data.
library(gbm)
set.seed(1234)
#1000개 트리, shrinkage:0.01
gbm.fit <- gbm(as.numeric(Purchase == "Yes") ~ ., 
               data = train, 
               n.trees = 1000, 
               shrinkage = 0.01)
head(summary(gbm.fit))

pred.gbm1=factor(ifelse(predict(gbm.fit,newdata=test,type="response")>0.2,1,0),
                 levels=c(1,0))
test.y   =factor(as.numeric(test$Purchase == "Yes"),levels=c(1,0))

confusionMatrix(pred.gbm1,
                test.y )

# 3.3. Repeat the similar analysis using the random forest
library(randomForest)
floor(sqrt(length(train)))

set.seed(1234)
rf.fit <- randomForest(factor(as.numeric(Purchase == "Yes")) ~ ., 
                       data = train, 
                       importance=TRUE,
                       mtry=9,
                       ntree=500)
rf.fit
head(importance(rf.fit))
x11();varImpPlot(rf.fit)
predict(rf.fit,newdata=test,type="prob")[,2]

pred.rf1=factor(ifelse(predict(rf.fit,newdata=test,type="prob")[,2]>0.2,1,0),levels=c(1,0))

confusionMatrix(pred.rf1,
                test.y )


#Precision-recall curve-RF
pr_gbm1<-evalmod(scores=predict(gbm.fit,newdata=test,type="response"),labels=test.y)
pr_rf1 <-evalmod(scores=predict(rf.fit ,newdata=test,type="prob")[,2],labels=test.y)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#4. (120 score) Generate a simulated two-class data set with 100 observations and two features 
#   in which is a visible but non-linear separation between the two classes. 
#   Show that in this setting, a support vector machine with a polynomial kernel (with degree greater than 1) or 
#   a radial kernel outperform a support vector classifier on the training data. 
#   Which technique performs best on the test data? 
#   Make plots and report training and test error rates in order to back up your assertions.

# 균일분포에서 자료 랜덤하게 생성
set.seed(10)
data <- data.frame(
  x = runif(100),
  y = runif(100)
)
score <- (2*data$x-0.5)^2 + (data$y)^2 - 0.5
data$class <- factor(ifelse(score > 0, "red", "blue"))

p <- ggplot(data, aes(x = x, y = y, color = class)) + 
  geom_point(size = 2) + scale_colour_identity()
x11();p

train <- 1:50
test <- 51:100

fits <- list(
  "Radial" = svm(class ~ ., data = data[train, ], kernel = "radial"),
  "Polynomial" = svm(class ~ ., data = data[train, ], kernel = "polynomial", degree = 2),
  "Linear" = svm(class ~ ., data = data[train, ], kernel = "linear")
)

err <- function(model, data) {
  out <- table(predict(model, data), data$class)
  (out[1, 2] + out[2, 1]) / sum(out)
}

x11();plot(fits[[1]], data)
x11();plot(fits[[2]], data)
x11();plot(fits[[3]], data)
sapply(fits, err, data = data[train, ])
sapply(fits, err, data = data[test, ])





