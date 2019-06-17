
# fit classification models in order to predict whether a given suburb has a crime rate above or below the median
# explore logistic regression, lda and KNN
getwd()
library(MASS)
data("Boston")

# summary stats of boston dataset
summary(Boston)
# crim    ?           zn             indus            chas              nox               rm       
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850   Min.   :3.561  
# 1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:?.00000   1st Qu.:0.4490   1st Qu.:5.886  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380   Median :6.208  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547   Mean   :6.285  
# 3r? Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240   3rd Qu.:6.623  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710   Max.   :8.780  
# age              dis              rad            ? tax           ptratio          black       
# Min.   :  2.90   Min.   : 1.130   Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
# 1st Qu.: 45.02   1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
# Media? : 77.50   Median : 3.207   Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
# Mean   : 68.57   Mean   : 3.795   Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
# 3rd Qu.: 94.08   3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.?666.0   3rd Qu.:20.20   3rd Qu.:396.23  
# Max.   :100.00   Max.   :12.127   Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
# lstat            medv      
# Min.   : 1.73   Min.   : 5.00  
# 1st Qu.: 6.95   1st Qu.:17.02  
# Median :11.36?  Median :21.20  
# Mean   :12.65   Mean   :22.53  
# 3rd Qu.:16.95   3rd Qu.:25.00  
# Max.   :37.97   Max.   :50.00  

# create the classifier repsonse variable to model on
boston_df <- Boston %>% 
  mutate(crim = if_else(crim > median(crim), 1, 0)) %>% ?  as_tibble()

unique(boston_df$crim)
str(boston_df)
boston_df$crim<-as.factor(boston_df$crim)

# explore the boston dataset to find best features to select
library(purrr)
cor(keep(boston_df, is.numeric))

#               crim          zn       indus      ?  chas         nox          rm         age         dis
# crim     1.00000000 -0.43615103  0.60326017  0.070096774  0.72323480 -0.15637178  0.61393992 -0.61634164
# zn      -0.43615103  1.00000000 -0.53382819 -0.042696719 -0.51660371  0.31199059 -0.56953734? 0.66440822
# indus    0.60326017 -0.53382819  1.00000000  0.062938027  0.76365145 -0.39167585  0.64477851 -0.70802699
# chas     0.07009677 -0.04269672  0.06293803  1.000000000  0.09120281  0.09125123  0.08651777 -0.09917578
# nox      0.72323480 -0.51660?71  0.76365145  0.091202807  1.00000000 -0.30218819  0.73147010 -0.76923011
# rm      -0.15637178  0.31199059 -0.39167585  0.091251225 -0.30218819  1.00000000 -0.24026493  0.20524621
# age      0.61393992 -0.56953734  0.64477851  0.086517774  0.73147010 -0?24026493  1.00000000 -0.74788054
# dis     -0.61634164  0.66440822 -0.70802699 -0.099175780 -0.76923011  0.20524621 -0.74788054  1.00000000
# rad      0.61978625 -0.31194783  0.59512927 -0.007368241  0.61144056 -0.20984667  0.45602245 -0.49458793
# tax    ? 0.60874128 -0.31456332  0.72076018 -0.035586518  0.66802320 -0.29204783  0.50645559 -0.53443158
# ptratio  0.25356836 -0.39167855  0.38324756 -0.121515174  0.18893268 -0.35550149  0.26151501 -0.23247054
# black   -0.35121093  0.17552032 -0.35697654  0.048?88485 -0.38005064  0.12806864 -0.27353398  0.29151167
# lstat    0.45326273 -0.41299457  0.60379972 -0.053929298  0.59087892 -0.61380827  0.60233853 -0.49699583
# medv    -0.26301673  0.36044534 -0.48372516  0.175260177 -0.42732077  0.69535995 -0.37695457 ?0.24992873
# rad         tax    ptratio       black      lstat       medv
# crim     0.619786249  0.60874128  0.2535684 -0.35121093  0.4532627 -0.2630167
# zn      -0.311947826 -0.31456332 -0.3916785  0.17552032 -0.4129946  0.3604453
# indus    0.595129275? 0.72076018  0.3832476 -0.35697654  0.6037997 -0.4837252
# chas    -0.007368241 -0.03558652 -0.1215152  0.04878848 -0.0539293  0.1752602
# nox      0.611440563  0.66802320  0.1889327 -0.38005064  0.5908789 -0.4273208
# rm      -0.209846668 -0.29204783 -0.3?55015  0.12806864 -0.6138083  0.6953599
# age      0.456022452  0.50645559  0.2615150 -0.27353398  0.6023385 -0.3769546
# dis     -0.494587930 -0.53443158 -0.2324705  0.29151167 -0.4969958  0.2499287
# rad      1.000000000  0.91022819  0.4647412 -0.4444128?  0.4886763 -0.3816262
# tax      0.910228189  1.00000000  0.4608530 -0.44180801  0.5439934 -0.4685359
# ptratio  0.464741179  0.46085304  1.0000000 -0.17738330  0.3740443 -0.5077867
# black   -0.444412816 -0.44180801 -0.1773833  1.00000000 -0.3660869  0.3?34608
# lstat    0.488676335  0.54399341  0.3740443 -0.36608690  1.0000000 -0.7376627
# medv    -0.381626231 -0.46853593 -0.5077867  0.33346082 -0.7376627  1.0000000

# pairs plot
ggpairs(boston_df, aes(color = crim))

# let's model with rad, dis, age, nox? indus

# create test and training datasets
inTrain <- createDataPartition(boston_df$crim, p = .65, list = F)

# create splits
train <- boston_df[inTrain,]
test <- boston_df[-inTrain,]


# logistic regression fit
log_fit <- glm(crim ~ lstat + tax + dis, da?a = train, family = "binomial")
summary(log_fit)

# Call:
# glm(formula = crim ~ lstat + tax + dis, family = "binomial", 
#             data = train)
# 
# Deviance Residuals: 
#         Min        1Q    Median        3Q       Max  
# -3.08513  -0.50248   0?03739   0.29076   2.89157  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.319367   0.850909  -1.551    0.121    
# lstat        0.038419   0.030038   1.279    0.201    
# tax          0.008609   0.001626   5.293 1.2?e-07 ***
#         dis         -0.653754   0.124248  -5.262 1.43e-07 ***
#         ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 457.48  on 329  degree? of freedom
# Residual deviance: 222.73  on 326  degrees of freedom
# AIC: 230.73
# 
# Number of Fisher Scoring iterations: 6

# logistic regression predict
library(tibble)
log_pred <- predict(log_fit, newdata = test, type = "response") %>% 
  as.tibble() ?>% 
  mutate(class = if_else(value > .5, 1, 0))

# confusion matrix and model performance on test set
library(caret)
round(confusionMatrix(log_pred$class, test$crim)$overall,3)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull Accuracy?Value  McnemarPValue 
# 0.744          0.489          0.673          0.807          0.500          0.000          0.074


# lda fit
lda_fit <- lda(crim ~ lstat + tax + dis, data = train, family = "binomial")

# lda predict
lda_predict <- predict(lda_fit, n?wdata = test, type = "repsonse")

# confusion matrix and test set accuracy
round(confusionMatrix(lda_predict$class, test$crim)$overall,3)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 0.739         ?0.477          0.667          0.802          0.500          0.000          0.027


# knn experiment
# model with k = 5, 10, 15, 20, 25, 100

library(class)
ks <- c(5,10,15,20,25)

cm_list <- list(k5 = NULL, k10 = NULL, k15 = NULL, k20 = NULL, k25 = NULL)

?or (i in seq_along(ks)) {
  
  
  knn_fit <- knn(
    train = as.matrix(cbind(train$lstat, train$tax, train$dis)),
    test = as.matrix(cbind(test$lstat, test$tax, test$dis)), 
    cl = train$crim,
    k = ks[[i]]
  )
  
  cm_list[i] <- confusionMatrix(knn?fit, test$crim)$overall %>% 
    as.tibble() %>% 
    filter(row_number() == 1)  
  
}

(cm_df <- as.data.frame(cm_list))


#         k5       k10       k15       k20       k25
# 1 0.9034091 0.8806818 0.8181818 0.8181818 0.8068182


