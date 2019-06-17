## Question 11:
# in this problem you will develop a model to predict high or low gas mileage
# this will be based on the auto dataset
data("Auto")
dim(Auto)
str(Auto)
# a. 
# create a classification (binary) variable that contains 1 if mpg is a value above the dataset median
# 0 otherwise

(auto_df <- Auto %>% 
    as.tibble() %>% 
    mutate(mpg01 = if_else(mpg > median(mpg), 1, 0)) %>% 
    dplyr::select(., -mpg))
dim(auto_df)
str(auto_df)
# A tibble: 392 x 9
#     cylinders displacement horsepower weight acceleration  year origin name                      mpg01
# <      dbl>        <dbl>      <dbl>  <dbl>        <dbl> <dbl>  <dbl> <fct>                     <dbl>
# 1      8.00          307        130   3504        12.0   70.0   1.00 chevrolet chevelle malibu     0
# 2      8.00          350        165   3693        11.5   70.0   1.00 buick skylark 320             0
# 3      8.00          318        150   3436        11.0   70.0   1.00 plymouth satellite            0
# 4      8.00          304        150   3433        12.0   70.0   1.00 amc rebel sst                 0
# 5      8.00          302        140   3449        10.5   70.0   1.00 ford torino                   0
# 6      8.00          429        198   4341        10.0   70.0   1.00 ford galaxie 500              0
# 7      8.00          454        220   4354         9.00  70.0   1.00 chevrolet impala              0
# 8      8.00          440        215   4312         8.50  70.0   1.00 plymouth fury iii             0
# 9      8.00          455        225   4425        10.0   70.0   1.00 pontiac catalina              0
# 10      8.00          390        190   3850         8.50  70.0   1.00 amc ambassador dpl           0
# # ... with 382 more rows


# b. 
# explore the data graphically in order to investigate the association between mpg01 and other features

# correlation matrix
cor(keep(auto_df, is.numeric))

# auto_df%>%
#   keep(is.numeric)%>%
#   cor(.)%>%
#   as.data.frame()


#               cylinders displacement horsepower     weight acceleration       year     origin      mpg01
# cylinders     1.0000000    0.9508233  0.8429834  0.8975273   -0.5046834 -0.3456474 -0.5689316 -0.7591939
# displacement  0.9508233    1.0000000  0.8972570  0.9329944   -0.5438005 -0.3698552 -0.6145351 -0.7534766
# horsepower    0.8429834    0.8972570  1.0000000  0.8645377   -0.6891955 -0.4163615 -0.4551715 -0.6670526
# weight        0.8975273    0.9329944  0.8645377  1.0000000   -0.4168392 -0.3091199 -0.5850054 -0.7577566
# acceleration -0.5046834   -0.5438005 -0.6891955 -0.4168392    1.0000000  0.2903161  0.2127458  0.3468215
# year         -0.3456474   -0.3698552 -0.4163615 -0.3091199    0.2903161  1.0000000  0.1815277  0.4299042
# origin       -0.5689316   -0.6145351 -0.4551715 -0.5850054    0.2127458  0.1815277  1.0000000  0.5136984
# mpg01        -0.7591939   -0.7534766 -0.6670526 -0.7577566    0.3468215  0.4299042  0.5136984  1.0000000

library(GGally)

# pairs plot
ggpairs(auto_df %>% dplyr::select(., -name) %>% mutate(mpg01 = factor(mpg01)),aes(color = mpg01))

# we see that cars with high horsepower are bad with mpg!
# we see that the more weight a car has the worse it is at mpg!
# 

# final data set for modeling
auto_mod <- auto_df %>% dplyr::select(., -name) %>% mutate(mpg01 = factor(mpg01)) %>% 
  as.tibble()


# c. 
# create a test and training data set
library(caret)

# define splits
intrain <- createDataPartition(auto_mod$mpg01, p = .6, list = F)

# create test and training
train <- auto_mod[intrain,]
test <- auto_mod[-intrain,] 
dim(train)
dim(test)

# d.
# perform lda on the training data in order to predict mpg01 using the variables most associated with mpg01 from our EDA

library(MASS)

# lda fit
lda_fit <- lda(mpg01 ~ horsepower + weight + displacement, data = train)

# lda summary stats

# group means
lda_fit$means
# horsepower   weight displacement
# 0  128.90678 3587.339     271.0424
# 1   78.84746 2346.585     115.5254

# the singluar values which give ratio of between and within group standard deviations on linear discriminant variables
# these squares are the F-statistics
lda_fit$svd
# [1] 18.36453

# see the prior probabilities the lda call estimated to approximate the bayes decision boundary
lda_fit$prior
# 0   1 
# 0.5 0.5 


# predict using lda
lda_predict <- predict(lda_fit, newdata = test, type = "response") %>% 
  as.data.frame() %>% 
  dplyr::select(class)

# confusion matrix
cm <- confusionMatrix(lda_predict$class, test$mpg01)

cm$table
#         Reference
# Prediction  0  1
#          0 67  4
#          1 11 74

cm$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 9.038462e-01   8.076923e-01   8.463703e-01   9.451789e-01   5.000000e-01   3.680652e-27   1.213353e-01



# e. 
# perform QDA on the training data and test on the test dataset

# qda fit
qda_fit <- qda(mpg01 ~ horsepower + weight + displacement, data = train)

# predict using qda
qda_predict <- predict(qda_fit, newdata = test, type = "response") %>% 
  as.data.frame() %>% 
  dplyr::select(class)

# confusion matrix
cm <- confusionMatrix(qda_predict$class, test$mpg01)

cm$table
#                 Reference
# Prediction      0  1
#              0 69  7
#              1  9 71

cm$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 8.974359e-01   7.948718e-01   8.387888e-01   9.402300e-01   5.000000e-01   3.271911e-26   8.025873e-01


# f. 
# perform a logistic regression 

# logistic regression fit
log_fit <- glm(mpg01 ~ horsepower + weight + displacement, data = train,
               family = "binomial")

# logistic regression predict
(log_pred <- predict(log_fit, newdata = test, type = "response") %>% 
    as.tibble() %>% 
    mutate(class = if_else(value > .5, "1", "0")))

# develop confusion matrix
confusionMatrix(log_pred$class, test$mpg01)$table
# Reference
# Prediction  0  1
# 0 70  8
# 1  8 70
confusionMatrix(log_pred$class, test$mpg01)$overall
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 8.974359e-01   7.948718e-01   8.387888e-01   9.402300e-01   5.000000e-01   3.271911e-26   1.000000e+00 


# g. 
# perform a KNN on the training data in order to predict mpg01

ks <- c(5,10,15,20,25)

cm_list <- list(k5 = NULL, k10 = NULL, k15 = NULL, k20 = NULL, k25 = NULL)

for (i in seq_along(ks)) {
  
  
  knn_fit <- knn(
    train = as.matrix(cbind(train$horsepower, train$weight, train$displacement)),
    test = as.matrix(cbind(test$horsepower, test$weight, test$displacement)), 
    cl = train$mpg01,
    k = ks[[i]]
  )
  
  cm_list[i] <- confusionMatrix(knn_fit, test$mpg01)$overall %>% 
    as.tibble() %>% 
    filter(row_number() == 1)  
  
}

(cm_df <- as.data.frame(cm_list))

#       k5       k10       k15       k20       k25
# 1 0.8910256 0.8974359 0.8910256 0.8782051 0.8910256

# K at 10 seems to give the best overall prediction - test accuracy is 89.74%


