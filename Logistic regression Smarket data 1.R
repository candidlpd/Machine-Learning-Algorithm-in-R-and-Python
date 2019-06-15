                          ### The Stock Market Data ####
#####################CASE I ############################
#INTRODUCTION
#In this paper we will explore a Stock Market data and build a logistic regression model. 
#The S&P 500 Index (formerly Standard & Poor's 500 Index) is a market-capitalization-weighted 
#index of the 500 largest U.S. publicly traded companies by market value, 
#The index is widely regarded as the best single gauge of large-cap U.S. equities. 
#Data: We will make use of an inbuilt dataset called Smarket (S&P Stock Market Data) 
#having 1250 observations and 9 variables. Columns and row names have been given below:


#Year: The year that the observation was recorded
#Lag1: Percentage return for previous day
#Lag2: Percentage return for 2 days previous
#Lag3: Percentage return for 3 days previous
#Lag4: Percentage return for 4 days previous
#Lag5: Percentage return for 5 days previous
#Volume: Volume of shares traded (number of daily shares traded in billions)
#Today: Percentage return for today
#Direction: A factor with levels Down and Up indicating whether the market had a 
#positive or negative return on a given day


# the Smarket data is part of the ISLR library. 
# This data set consists of percentage returns for the S&P 500 stock index
# over 1250 days, from the beginning of 2001 until the end of 2005. 
# For each date, we have recorded the percentage returns for 
# each of the five previous trading days, Lag1 through Lag5. 
# We have also recorded Volume (the number of shares traded on the previous day, in billions),
# Today (the percentage return on the date in question) 
# and Direction (whether the market was Up or Down on this date).
library(ISLR)
head(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket);
summary(Smarket$Direction)
str(Smarket)
#Labelled my Direction attributes ("Up", "Down") to (1,0).
#We won't change Year to the factor form as it will be needed later in the numerci form.
#Smarket$Direction <- factor(Smarket$Direction, levels = c("Up", "Down"), labels = c(1,0)) 





###########IDENTIFYING MISSING DATA ################
#1.# Let's check for any missing values in the data
colSums(is.na(Smarket))
sum(is.na(Smarket))


#2.# list rows of data that have missing values 
Smarket[!complete.cases(Smarket),]


#3.#
par(mfrow=c(1,1))
library(Amelia)
library(mlbench)
missmap(Smarket, col=c("blue", "red"), legend=FALSE)
##Horizontal lines indicate missing data for an instance, vertical blocks represent missing data for an attribute.
##No missing data in this dataset!


#############SCATTER MATRIX AND CORRELATION#################
#1#
par(mfrow=c(1,1))
library(corrplot)
corrplot::corrplot.mixed(cor(Smarket[, -9]), upper="circle")
##A dot-representation was used where blue represents positive correlation and red negative. 
##The larger the dot the larger the correlation. You can see that the matrix is symmetrical and 
##that the diagonal are perfectly positively correlated because it shows the correlation of 
##each variable with itself. Unfortunately, none of the variables are correlated with one another.



#2# pairs plot
pairs(Smarket, col = Smarket$Direction)
##It looks like there's not much correlation going on here.
## correlations between our lag variables and today's returns are almost 0
## the only substantial correlation is between Year and Volume predictors

#3# correlation matrix
(Smarket_df <- Smarket %>% 
    keep(is.numeric) %>% 
    cor(.) %>% 
    as.data.frame(.))

#4#ggpairs plot
library(GGally)
ggpairs(data=Smarket, columns=c(1:9), ggplot2::aes(colour=Direction))



#5# traditional square plot matrix for purely quantitative variables
ggscatmat(Smarket, columns = 1:ncol(Smarket), color = "Direction", alpha = 1, corMethod = "pearson")



#6# distribution of predictors response wise
library(ggplot2)
ggplot(Smarket, aes(x=Lag5, y=Lag2, shape=Direction, color=Direction))+geom_point()


#7# Distribution of predictor variables


############HISTOGRAMS##############
#1# 
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(Smarket[,i], main=names(Smarket)[i],col="orange")
}

#2#Distribution of Predictor variables
par(mfrow = c(1,8))
for (i in 1:8)
{
  hist((Smarket[,i]), main = paste("Distibution of ", colnames(Smarket[i])), xlab = colnames(Smarket[i]))
}
#############BOXPLOT#########################
#1#
par(mfrow=c(1,8))
for(i in 1:8) {
  boxplot(Smarket[,i], main=names(Smarket)[i])
}
##You can see that the Lags and Today all has a similar range. 
##Otherwise, there's no sign of any outliers.


#2#Distribution of Lag1,Lag2....... Direction wise
ggplot(Smarket, aes(x=Direction, y=Lag1))+geom_boxplot()
ggplot(Smarket, aes(x=Direction, y=Lag2))+geom_boxplot()
ggplot(Smarket, aes(x=Direction, y=Lag3))+geom_boxplot()
ggplot(Smarket, aes(x=Direction, y=Lag4))+geom_boxplot()
ggplot(Smarket, aes(x=Direction, y=Lag5))+geom_boxplot()
ggplot(Smarket, aes(x=Direction, y=Volume))+geom_boxplot()

#3# Distribution of Predictors by Response variable
par(mfrow = c(1,8))
for (i in 1:8)
{
  boxplot((Smarket[,i])~ Direction,data = Smarket, main = paste("Distibution of ", colnames(Smarket[i])), xlab = colnames(Smarket[i]))
}


###############DENSITY PLOT BY RESPONSE###################
#1#the density plot by Direction can help see the separation of Up and Down. 
#It can also help to understand the overlap in Direction values for a variable.
library(caret)
x <- Smarket[,1:8]
y <- Smarket[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
##We can see that the Direction values overlap for all of these variables,
##meaning that it's hard to predict Up or Down based on just one or two variables.






#2#
featurePlot(Smarket[,1:8], Smarket[,9], plot="density")
##Feature plot depicts that the direction of the stock going up and down are visually overlapping.
##Only Today attribute seems to make a distinction between the nature. 
##All other varibles are showing similar impacts on the binary output

################LOGISTIC REGRESSION###################
# plotting volume shows that volume of trading has increased over time
par(mfrow=c(1,1))
plot(Smarket$Volume)
# Logistics Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)
###Summary says that the p values of Lags3, Lag4 and Lag5 aren't contributing significantly 
##to the nature of the binary outcome as compared to Lag1 and Lag2.
##Look like none of the coefficients are significant here. 
##It also gives you the null deviance (the deviance just for the mean) and 
##the residual deviance (the deviance for the model with all the predictors). 
##There's a very small difference between the 2, along with 6 degrees of freedom.
## the smallest pvalue is associated with Lag1
## the negative correlation coefficient suggests that a positive return yesterday would lead to a negative return today
## however the Lag1 pvalue is not significant at the alpha = .05 level

# summary table of coefficients
summary(glm.fit)$coef

##################confusion matrix##############################
#1#
contrasts(Smarket$Direction)
library(caret)
library(dplyr)
library(tidyverse)

glm.pred <- predict(glm.fit, type = "response") %>% 
  as.tibble() %>% 
  mutate(class = if_else(value > .5, "Up", "Down"))

confusionMatrix(glm.pred$class, reference = Smarket$Direction)

#2#book style
glm.probs<-predict(glm.fit, newdata=Smarket, type="response")
glm.probs[1:10]
glm.pred1<-as.factor(ifelse(glm.probs>0.5, "Up","Down"))
table(glm.pred1, Smarket$Direction)
## the diagnoal elements of the matrix indicate correct predictions
## the off-diagnoal represent the incorrect predictions
## our model correctly predicted that the market would go up on 507 days and that it would go down 145 days
## this equates to 652 correct predictions out of 1250 observations
#this accuracy formula not work why?
mean(glm.pred1==Smarket$Direction)

library(caret)
confusionMatrix(glm.pred1, Smarket$Direction)

##Accuracy of the model is 52.16%. We haven't created a good model.
# training error
1 - mean(glm.pred1 == Smarket$Direction)
mean(glm.pred1 != Smarket$Direction)


#############SIGMA CURVE##################################
#Plotting a graph: Probability of predictor Vs Direction
Smarket %>%
  mutate(prob = ifelse(Smarket$Direction == "Up", 1, 0)) %>%
  ggplot(aes(Lag1, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Lag1") +
  ylab("Probability of Direction")

###############ANALYSIS#######################
## THIS IS MISLEADING = TRAINING ACCURACY IS NOT EQUAL TO TESTING ACCURACY
## we trained and tested  our model on the same set of data!!
## our TRAINING ERROR rate is 1 - accuracy
## AS WE KNOWN TRAINING ERROR IS OFTEN OVER OPTIMISTIC - IT TENDS TO UNDERESTIMATE THE TESTING ERROR!!
## we need to use TEST and TRAINING datasets to accuracy assess our model!!
##Accuracy of the model is 47.84%. We haven't created a good model
##Let's divide our dataset into training and test it onto the test data. 
##Conclusion: We will remove Lag3, lag4 and Lag5 as their p-values have relatively high magnitudes.



#####################case II ########################################
# recall that our initial model had no significant pvalues for any coefficient
# the smallest being Lag1 which even then was not that small
# maybe by removing the variables that appear not to be helpful we can get a better model?
# predictors that have no relationship with repsonse tend to deteriorate the TEST ERROR!!
# more predictors cause an increase in variance without a corresponding decrease in bias
#This is overfitting
# let's refit the model will the "most important" variables
#Well, you might have overfitted the data. In order to fix this, you're going to fit a smaller model
#and use Lag1, Lag2 as the predictors, thereby leaving out all other variables. 
#The rest of the code is the same
glm.fit2 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial)
summary(glm.fit2)
#compare two models
anova(glm.fit, glm.fit2, test="Chisq")
##Since p values is greater than 0.9358 menaing accepting null hypothesis(predictors can be dropped) indicating 
##null hypothesis is equal to alternative hypothesis i.e. the dropped predictors are not significant.

##Are Lag1 and Lag2 significant? To check, we can compare with the null model

glm.fit2_null<-glm(Direction ~ 1, data=Smarket, family=binomial)
summary(glm.fit2_null)
anova(glm.fit2_null, glm.fit2, test="Chisq")
##Note even Lag1 and Lag2 are not significant. But let's keep them otherwise, we can't do anything


#####################case III ########################################
#we filter our test and training to data before 2005 to build our model and
#we will then test it on post 2005 data


#########################creating training and test data#############################
#1# test and training datasets
train = Smarket %>% 
  as.data.frame() %>% 
  filter(Year < 2005)

dim(train)

test = Smarket[!Smarket$Year %in% train$Year,]
dim(test)



#2#
train = (Smarket$Year < 2005) # vector with Year less than 2005

# filter smarket to only the observations less than 2005
Smarket.2005 =Smarket[!train,]
dim(Smarket.2005)
# create worded direction variable on the holdout data set
Direction.2005 <- Smarket$Direction[!train,]

glm.fit_subset <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family = "binomial", subset = train)
summary(glm.fit3_subset)

#3#
#Training Data: Data prior to 2005
library(caTools)
training_set <- subset(Smarket, Year<2005)
dim(training_set)
#Test Data: Data of Year=2005
test_set <- subset(Smarket, Year==2005)

regressor2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = training_set, family = binomial)
summary(regressor2)

#we use this in our model
#################Logistic regression #########################
glm.fit3 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = training_set, family = binomial)
summary(glm.fit3)
glm.probs3 <- predict(glm.fit3, type="response", newdata = test_set)
glm.probs3[1:5]
glm.pred2 <- ifelse(glm.probs3>0.5, "Up", "Down")
table(glm.pred2, test_set$Direction)
mean(glm.pred2==test_set$Direction)
confusionMatrix(glm.pred2, test_set$Direction)
##worse than the previous case


#####################case IV ########################################
Smarket.train<-subset(Smarket, Year < 2005, select=c(Lag1, Lag2, Direction))
dim(Smarket.train)
Smarket.test<-subset(Smarket, !(Year<2005), select=c(Lag1, Lag2, Direction))
dim(Smarket.test)
#build model on trianing data
glm.fit4<-glm(Direction~ Lag1+Lag2, data=Smarket.train, family=binomial(link='logit'))
summary(glm.fit4)
#summary of estimated probablilities for training data

glm.probs44<-predict(glm.fit4, type="response")
summary(glm.probs44)
#estimated probabiliteis for test data
glm.probs4<-predict(glm.fit4, type="response", newdata=Smarket.test)
#predicted classes using 0.5 cutoff
glm.pred4<-ifelse(glm.probs4>=0.5, "Up", "Down")
table(glm.pred4, Smarket.test$Direction)
mean(glm.pred4==test_set$Direction)
#test error rate
1-mean(glm.pred4==test_set$Direction)
#Accuracy 
confusionMatrix(factor(glm.pred4),test_set$Direction)

##############################DECISION BOUNDARY ##########################################
#1#
coef(glm.fit4)
-coef(glm.fit4)[1:2]/coef(glm.fit)[3]

plot(Smarket.test[, c("Lag1", "Lag2")], col = ifelse(Smarket.test[, "Direction"] == "Up", "green", "red"))
abline(coef = -coef(glm.fit4)[1:2]/coef(glm.fit4)[3])
# Logistic regression decision boundary

# beta0 + beta1 * Lag1 + beta2 * Lag2 = 0
# implying that Lag2 = -(beta0/beta2)  -(beta1/beta2)*Lag1
# is equation of the line

#2#



decisionplot <- function(model, data, class = NULL, predict_type = "class",
                        resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

decisionplot(glm.fit4,Smarket.test, class="Direction", main="Logistic Regression")

#3#


glm.fit4
coef(glm.fit4)

slope <- coef(glm.fit4)[2]/(-coef(glm.fit4)[3])
slope
intercept <- coef(glm.fit4)[1]/(-coef(glm.fit4)[3]) 
intercept

library(lattice)
xyplot(Lag2 ~Lag1 , data = Smarket.test, groups = Direction,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })


#########################ROC and AUC##########################
library(pROC)

roc.lr <- roc(Smarket.test[, "Direction"], glm.probs4, levels = c("Down", "Up"))

roc.lr

# Area under the curve: 0.5584

#####################case V ########################################
#Well, you might have overfitted the data. In order to fix this, you're going to fit a smaller model
#and use Lag1, Lag2, Lag3 as the predictors, thereby leaving out all other variables.
#the rest of the code is the same.

# Fit a smaller model
glm.fit5= glm(Direction ~ Lag1 + Lag2 + Lag3, data = training_set, family = binomial)
glm.probs5= predict(glm.fit5, newdata = test_set, type = "response")
glm.pred5= ifelse(glm.probs5 > 0.5, "Up", "Down")
table(glm.pred5, test_set$Direction)
mean(glm.pred5==test_set$Direction)
1-mean(glm.pred4==test_set$Direction)
summary(glm.fit5)
confusionMatrix(factor(glm.pred5),test_set$Direction)
##Well, you got a classification rate of 59%, not too bad.
##using the smaller model appears to perform better.
##Nothing became significant, at least the P-values are better,
##indicating an increase in prediction of performance.









########################conclusion#####################################
## glm  does not assume a linear relationship between dependent and independent variables.
##However, it assumes a linear relationship between link function and independent variables in 
## logit model I hope you have learned something valuable!


