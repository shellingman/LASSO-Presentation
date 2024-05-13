##TRU LASSO Presentation##
set.seed(14)
rm(list = ls()) #Clear Environment
####Packages####
if(!require(glmnet)) install.packages("glmnet")
library(glmnet)
#if(!require(stargazer)) install.packages("stargazer")
#library(stargazer)

####Load Data####
Cars <- mtcars
head(Cars) #Different measures


####Min-Max Scaling (0-1) Function####

norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}
####Define Model####

#Response Variable
y <- Cars$hp #horse power

#Predictors
x <- data.matrix(Cars[,c('mpg', 'wt', 'drat', 'qsec')]) #Miles/Gallon #Weight (1000lbs) #Rear axle Ratio #1/4 mile time  
#stargazer(head(x),summary=FALSE)
#Scaled#
y <- norm_minmax(y)
x <- apply(x, MARGIN = 2, FUN = norm_minmax)


####Model Estimation####

##OLS Model
lm <- lm(y~x)
coef(lm)

##Small lambda
#alpha = 1 for LASSO
m1 <- glmnet(x,y,alpha=1,lambda = 0.001)
coef(m1)

##Large lambda
m2 <- glmnet(x,y,alpha=1,lambda = 5)
coef(m2)


##Cross Validation##
#perform 10-fold cross-validation to find optimal lambda value
cv1 <- cv.glmnet(x, y, nfolds = 10, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv1$lambda.min
best_lambda

#Plot the progress of MSE by lambda value#
plot(cv1)

#Plot the coef. progress
fit <- glmnet(x,y)
plot(fit)

##Best Model##

m3 <- glmnet(x,y,alpha=1,lambda = best_lambda)
coef(m3) #drat set to zero

