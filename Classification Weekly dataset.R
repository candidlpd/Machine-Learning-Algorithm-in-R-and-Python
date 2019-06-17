# use the weekly dataset to answer the following questions

# load the data
library(ISLR)
data("Weekly")
getwd()
setwd("C:/Users/candi/OneDrive/Documents/Machine Learning in R")
# a. 
# produce some numerical and graphical summaries of the weekly dataset

? view the strucutre
str(Weekly)
# 'data.frame':	1089 obs. of  9 variables:
# $ Year     : num  1990 1990 1990 1990 1990 1990 1990 1990 1990 1990 ...
# $ Lag1     : num  0.816 -0.27 -2.576 3.514 0.712 ...
# $ Lag2     : num  1.572 0.816 -0.27 -2.576 3.514 .?.
# $ Lag3     : num  -3.936 1.572 0.816 -0.27 -2.576 ...
# $ Lag4     : num  -0.229 -3.936 1.572 0.816 -0.27 ...
# $ Lag5     : num  -3.484 -0.229 -3.936 1.572 0.816 ...
# $ Volume   : num  0.155 0.149 0.16 0.162 0.154 ...
# $ Today    : num  -0.27 -2.576?3.514 0.712 1.178 ...
# $ Direction: Factor w/ 2 levels "Down","Up": 1 1 2 2 2 1 2 2 2 1 ...

# quick summary
summary(Weekly)
# Year           Lag1               Lag2               Lag3               Lag4         
# Min.   :1990   Min.   :-18.1950   Min.  ?:-18.1950   Min.   :-18.1950   Min.   :-18.1950  
# 1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580   1st Qu.: -1.1580  
# Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410   Median :  0.2380  
# Mean   :2000   Mea?   :  0.1506   Mean   :  0.1511   Mean   :  0.1472   Mean   :  0.1458  
# 3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
# Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260 ?
# Lag5              Volume            Today          Direction 
# Min.   :-18.1950   Min.   :0.08747   Min.   :-18.1950   Down:484  
# 1st Qu.: -1.1660   1st Qu.:0.33202   1st Qu.: -1.1540   Up  :605  
# Median :  0.2340   Median :1.00268   Median :  0.24?0             
# Mean   :  0.1399   Mean   :1.57462   Mean   :  0.1499             
# 3rd Qu.:  1.4050   3rd Qu.:2.05373   3rd Qu.:  1.4050             
# Max.   : 12.0260   Max.   :9.32821   Max.   : 12.0260 

# pairs plot
pairs(Weekly, col = Weekly$Direc?ion)

# correlation matrix
(Weekly_df <- Weekly %>% 
    keep(is.numeric) %>% 
    cor(.) %>% 
    as.data.frame(.))
#               Year         Lag1        Lag2        Lag3         Lag4         Lag5      Volume
# Year    1.00000000 -0.032289274 -0.033390?1 -0.03000649 -0.031127923 -0.030519101  0.84194162
# Lag1   -0.03228927  1.000000000 -0.07485305  0.05863568 -0.071273876 -0.008183096 -0.06495131
# Lag2   -0.03339001 -0.074853051  1.00000000 -0.07572091  0.058381535 -0.072499482 -0.08551314
# Lag3   -0.?3000649  0.058635682 -0.07572091  1.00000000 -0.075395865  0.060657175 -0.06928771
# Lag4   -0.03112792 -0.071273876  0.05838153 -0.07539587  1.000000000 -0.075675027 -0.06107462
# Lag5   -0.03051910 -0.008183096 -0.07249948  0.06065717 -0.075675027  1.000?00000 -0.05851741
# Volume  0.84194162 -0.064951313 -0.08551314 -0.06928771 -0.061074617 -0.058517414  1.00000000
# Today  -0.03245989 -0.075031842  0.05916672 -0.07124364 -0.007825873  0.011012698 -0.03307778
# Today
# Year   -0.032459894
# Lag1   -0.0750?1842
# Lag2    0.059166717
# Lag3   -0.071243639
# Lag4   -0.007825873
# Lag5    0.011012698
# Volume -0.033077783
# Today   1.000000000

# we notice a strong correlation between Year and Volume


# b. 
# use the full dataset to perform a logistic regressi?n with Direction as the repsonse
# use the five lag variables and Volume as predictors
# are there any significant features? what are they?

# fit our logistic regression model
log_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume - 1,
     ?         family = "binomial", data = Weekly)

summary(log_fit)

# Call:
# glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
#                     Volume - 1, family = "binomial", data = Weekly)
# 
# Deviance Residuals: 
#         Min      1Q  M?dian      3Q     Max  
# -1.726  -1.191   1.033   1.148   1.553  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)  
# Lag1   -0.032730   0.026177  -1.250   0.2112  
# Lag2    0.068196   0.026685   2.556   0.0106 *
# Lag3   -0.008099   0.0?6447  -0.306   0.7594  
# Lag4   -0.019420   0.026231  -0.740   0.4591  
# Lag5   -0.006856   0.026230  -0.261   0.7938  
# Volume  0.056925   0.026792   2.125   0.0336 *
#         ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (?ispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1509.7  on 1089  degrees of freedom
# Residual deviance: 1496.1  on 1083  degrees of freedom
# AIC: 1508.1
# 
# Number of Fisher Scoring iterations: 4

# it seems that Lag2 and Volu?e are statisticall significant



# c.
# compute the confusion matrixand overall fraction of correct guesses for our model
# explain what the confusion matrix is giving us and the types of mistakes our classifier is giving us...

library(caret)

log_pred <? predict(log_fit, type = "response") %>% 
  as.tibble() %>% 
  mutate(class = if_else(value > .5, "Up", "Down"))

confusionMatrix(log_pred$class, reference = Weekly$Direction)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Down  Up
#       ?own   54  48
#       Up    430 557
# 
# Accuracy : 0.5611         
# 95% CI : (0.531, 0.5908)
# No Information Rate : 0.5556         
# P-Value [Acc > NIR] : 0.369          
# 
# Kappa : 0.035          
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensi?ivity : 0.11157        
# Specificity : 0.92066        
# Pos Pred Value : 0.52941        
# Neg Pred Value : 0.56434        
# Prevalence : 0.44444        
# Detection Rate : 0.04959        
# Detection Prevalence : 0.09366        
# Balanced Accuracy : 0?51612        
# 
# 'Positive' Class : Down 

# we see here from the confusion matrix output the overall accuracy is .56
# the specificity is .92 - we are able to predict the market going up most of the time
# the sensitivity is .11 - we do not do a good jo? of predicting when the market goes down


# d.
# now fit a logistic regression using a training period from 1990 to 2008
# use Lag2 as the only feature
# compute the confusion matrix on the test data

# define test and training datasets
train <- Weekly %>? 
  filter(Year < 2009)

# train1 <- Weekly %>% 
#   as.data.frame() %>%
#   filter(Year < 2009)

# train2<-subset(Weekly, Year < 2009)
# dim(train2)


test <- Weekly %>% 
  filter(Year >= 2009)


# fit our logistic regression model
log_fit <- glm(Directio? ~ Lag2,
               family = "binomial", data = train)

summary(log_fit)
# Call:
# glm(formula = Direction ~ Lag2, family = "binomial", data = train)
# 
# Deviance Residuals: 
#         Min      1Q  Median      3Q     Max  
# -1.536  -1.264   1.021   1?091   1.368  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  0.20326    0.06428   3.162  0.00157 **
#         Lag2         0.05810    0.02870   2.024  0.04298 * 
#         ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '?' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1354.7  on 984  degrees of freedom
# Residual deviance: 1350.5  on 983  degrees of freedom
# AIC: 1354.5
# 
# Number of Fisher Scoring iterations: 4


# ?redict on test
log_pred <- predict(log_fit, type = "response", newdata = test) %>% 
  as.tibble() %>% 
  mutate(class = if_else(value > .5, "Up", "Down"))

# develop confusion matrix
confusionMatrix(log_pred$class, reference = test$Direction)

# Confusion ?atrix and Statistics
# 
# Reference
# Prediction Down Up
# Down    9  5
# Up     34 56
# 
# Accuracy : 0.625          
# 95% CI : (0.5247, 0.718)
# No Information Rate : 0.5865         
# P-Value [Acc > NIR] : 0.2439         
# 
# Kappa : 0.1414         
#?Mcnemar's Test P-Value : 7.34e-06       
# 
# Sensitivity : 0.20930        
# Specificity : 0.91803        
# Pos Pred Value : 0.64286        
# Neg Pred Value : 0.62222        
# Prevalence : 0.41346        
# Detection Rate : 0.08654        
# Detection ?revalence : 0.13462        
# Balanced Accuracy : 0.56367        
# 
# 'Positive' Class : Down  

# accuracy is .625
# predicting specificity is .91 - slight degradation in predicting the market will go up
# predicting sensistivity is .21 - big improvement?in predicting the market will go down


# e. 
# repeat steps above using LDA

library(MASS)

# lda fit
lda_fit <- lda(Direction ~ Lag2, data = train)

# lda summary stats

# group means
lda_fit$means
# Lag2
# Down -0.03568254
# Up    0.26036581

# the sing?uar values which give ratio of between and within group standard deviations on linear discriminant variables
# these squares are the F-statistics
lda_fit$svd
# [1] 2.039443

# see the prior probabilities the lda call estimated to approximate the bayes deci?ion boundary
lda_fit$prior
# Down        Up 
# 0.4477157 0.5522843


# predict using lda
lda_predict <- predict(lda_fit, newdata = test, type = "response") %>% 
  as.data.frame() %>% 
  dplyr::select(class)

# confusion matrix
confusionMatrix(lda_predict$c?ass, test$Direction)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Down Up
# Down    9  5
# Up     34 56
# 
# Accuracy : 0.625          
# 95% CI : (0.5247, 0.718)
# No Information Rate : 0.5865         
# P-Value [Acc > NIR] : 0.2439     ?   
# 
# Kappa : 0.1414         
# Mcnemar's Test P-Value : 7.34e-06       
# 
# Sensitivity : 0.20930        
# Specificity : 0.91803        
# Pos Pred Value : 0.64286        
# Neg Pred Value : 0.62222        
# Prevalence : 0.41346        
# Detection ?ate : 0.08654        
# Detection Prevalence : 0.13462        
# Balanced Accuracy : 0.56367        
# 
# 'Positive' Class : Down  


# we notice the exact same results as the logistic regression...?



# f.
# repeat the above steps using QDA

# qda fit
qd?_fit <- qda(Direction ~ Lag2, data = train)
summary(qda_fit)

# qda summary stats

# group means
qda_fit$means
# Lag2
# Down -0.03568254
# Up    0.26036581


# see the prior probabilities the qda call estimated to approximate the bayes decision boundary
qd?_fit$prior
# Down        Up 
# 0.4477157 0.5522843


# predict using qda
qda_predict <- predict(qda_fit, newdata = test, type = "response") %>% 
  as.data.frame() %>% 
  dplyr::select(class)

# confusion matrix
confusionMatrix(qda_predict$class, test$Direc?ion)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Down Up
# Down    0  0
# Up     43 61
# 
# Accuracy : 0.5865          
# 95% CI : (0.4858, 0.6823)
# No Information Rate : 0.5865          
# P-Value [Acc > NIR] : 0.5419          
# 
# Ka?pa : 0               
# Mcnemar's Test P-Value : 1.504e-10       
# 
# Sensitivity : 0.0000          
# Specificity : 1.0000          
# Pos Pred Value :    NaN          
# Neg Pred Value : 0.5865          
# Prevalence : 0.4135          
# Detection Rate ? 0.0000          
# Detection Prevalence : 0.0000          
# Balanced Accuracy : 0.5000          
# 
# 'Positive' Class : Down 

# the qda model picks the market going up every time!!
# we get a .58 accuracy just by picking up every time!


# g.
# complet? the same steps using a knn with K = 1 classifier

library(class)

# knn fit
# the input into the knn call is a little different than our typical lm type calls
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

knn_fit <- knn(
  train = as?matrix(train$Lag2), # need to supply matrix of training variables
  test = as.matrix(test$Lag2), # need to supply matrix of testing variables - there is no model call
  cl = train$Direction, # need to provide a vector of the "right answer" to train our mod?l
  k = 1 # specify the number of nearest neighbors to use in the decision process
)

# knn does the fitting and prediction in one step
confusionMatrix(knn_fit, test$Direction)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Down Up
# Down   ?1 30
# Up     22 31
# 
# Accuracy : 0.5             
# 95% CI : (0.4003, 0.5997)
# No Information Rate : 0.5865          
# P-Value [Acc > NIR] : 0.9700          
# 
# Kappa : -0.0033         
# Mcnemar's Test P-Value : 0.3317          
# 
# Sensitivity : ?.4884          
# Specificity : 0.5082          
# Pos Pred Value : 0.4118          
# Neg Pred Value : 0.5849          
# Prevalence : 0.4135          
# Detection Rate : 0.2019          
# Detection Prevalence : 0.4904          
# Balanced Accuracy : 0.4?83          
# 
# 'Positive' Class : Down 


# h.
# which of these methods appear to be the best for this data?
# lda or logistic regression appear to be the best with test accuracy of .625
# for reference the qda model picked UP everytime in the test data?- and got accuracy of .58
# our logistic and lda models improved slightly over this


# i.
# experiement with other models and different transformations

# knn experiment
# model with k = 5, 10, 15, 20, 25, 100

ks <- c(5,10,15,20,25,100, 150, 250)

cm_lis? <- list(k5 = NULL, k10 = NULL, k15 = NULL, k20 = NULL, k25 = NULL, k100 = NULL, k150 = NULL,
                k250 = NULL)

for (i in seq_along(ks)) {
  
  set.seed(100)
  
  knn_fit <- knn(
    train = as.matrix(train$Lag2), # need to supply matrix of tra?ning variables
    test = as.matrix(test$Lag2), # need to supply matrix of testing variables - there is no model call
    cl = train$Direction, # need to provide a vector of the "right answer" to train our model
    k = ks[[i]] # specify the number of near?st neighbors to use in the decision process
  )
  
  cm_list[i] <- confusionMatrix(knn_fit, test$Direction)$overall %>% 
    as.tibble() %>% 
    filter(row_number() == 1)  
  
}

(cm_df <- as.data.frame(cm_list))

#              k5       k10       k15    ?  k20       k25      k100  k150      k250
# 1     0.5480769 0.5673077 0.5865385 0.5961538 0.5384615 0.5769231 0.625 0.5865385

# we see that k20 has the highest test accuracy



# qda with transformed variables
# qda fit
qda_fit <- qda(Direction ~ Lag2 + I?Lag2^2), data = train)
summary(qda_fit)

# qda summary stats

# group means
qda_fit$means
# Lag2
# Down -0.03568254
# Up    0.26036581


# see the prior probabilities the qda call estimated to approximate the bayes decision boundary
qda_fit$prior
# Down   ?    Up 
# 0.4477157 0.5522843


# predict using qda
qda_predict <- predict(qda_fit, newdata = test, type = "response") %>% 
  as.data.frame() %>% 
  dplyr::select(class)

# confusion matrix
confusionMatrix(qda_predict$class, test$Direction)$overall %>% as.?ata.frame()
# .
# Accuracy       6.250000e-01
# Kappa          1.281169e-01
# AccuracyLower  5.246597e-01
# AccuracyUpper  7.180252e-01
# AccuracyNull   5.865385e-01
# AccuracyPValue 2.439500e-01
# McnemarPValue  2.989608e-07

# using a squared term gives ?s the accuracy of our logistic and lda models!!


# our best models are the original logistic regression, lda and the transformed qda
# these all meet at 62% accuracy
# note predicting up on the test data gives us 58% accuracy