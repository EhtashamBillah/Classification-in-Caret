require(corrplot)
require(Amelia)
require(VIM) 
require(mice) 
require(ggplot2)
require(tidyverse)
require(dplyr)
require(glmnet)
require(caret)
require(stargazer)
require(pastecs)
require(DataExplorer)
require(lubridate)
require(xgboost)
require(plyr)
require(DMwR)
require(doSNOW)
require(parallel)



# setting up parallel computing
getDoParWorkers()
getDoParRegistered()
getDoParName()
numberofcores = detectCores()
cl <- makeCluster(spec = numberofcores, type="SOCK")   # Setting up clusters for Parallel Computing
registerDoSNOW(cl)                                     # Registering clusters
#stopCluster(cl)  


# loading the datta
df <- read.csv('Assignment_data.csv', sep = ';')

##################################################################
# Exploratory Data Analysis
##################################################################

# Descriptive statistics
head(df, n= 10)
summary(df)
str(df)
View(stat.desc(df))
dim(df)

# removing the ID variables
df_short <- df[,-c(1,2,3,16)]
# transforming NULL values to NA values
df_short <- na_if(df_short, 'NULL')


# Visualizing missing values
plot_missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + 
    scale_fill_manual(values=c("#f6db2f","#12226c"),name="Missing\n(0=Yes,1=No)") + 
    theme_light() + ylab("") + xlab("") + ggtitle(title)
}
plot_missing(df_short[,colSums(is.na(df_short)) > 0])


# Distribution of Dependent Variable Target
prop.table(table(df_short$Target))

# Histogram, barplot, box plot, density plot, correlation plot, scatter plot
plot_str(df_short)
plot_histogram(df_short)
plot_density(df_short)
plot_bar(df_short)
plot_intro(df_short)
plot_boxplot(df_short, by = "Variable_45")
plot_correlation(df_short)

ggplot(data = df_short)+
  geom_point(aes(x = Variable_20, y = Variable_37, color = Variable_45))+
  ggtitle('Scatter plot of Variable_20 and Variable_37 by gender')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
  

##################################################################
# Feature engineering
##################################################################
# checking each variable to understand the variable type
summary(df_short$Variable_45)
class(df_short$Variable_45)
sort(unique(df_short$Variable_45))

ggplot(data = df_short)+
  geom_histogram(aes(x = Variable_20), bins = 20,  fill = '#008080', color = 'black')

ggplot(data =df_short)+
  geom_point(aes(x = Variable_41, y = Variable_9), color = '#611cf2', alpha = 0.7)


######### changing variable types
# re-encoding the categorical variables (needed because of changing NULL to NA)

df_short$Variable_13 <- factor(df_short$Variable_13)
df_short$Variable_14 <- factor(df_short$Variable_14)
df_short$Variable_17 <- factor(df_short$Variable_17)
df_short$Variable_18 <- factor(df_short$Variable_18)
df_short$Variable_19 <- factor(df_short$Variable_19)
df_short$Variable_23 <- factor(df_short$Variable_23)
df_short$Variable_24 <- factor(df_short$Variable_24)
df_short$Variable_25 <- factor(df_short$Variable_25)
df_short$Variable_26 <- factor(df_short$Variable_26)
df_short$Variable_29 <- factor(df_short$Variable_29)
df_short$Variable_35 <- factor(df_short$Variable_35)
df_short$Variable_36 <- factor(df_short$Variable_36)
df_short$Variable_40 <- factor(df_short$Variable_40)
df_short$Variable_41 <- factor(df_short$Variable_41)

# 1. converting to categorical variable 
df_short$Variable_2 <- factor(df_short$Variable_2)
df_short$Variable_4 <- factor(df_short$Variable_4)
df_short$Target <- factor(df_short$Target) 

# 2. converting to numeric variable
df_short$Variable_6 <- as.numeric(as.character(df_short$Variable_6))
df_short$Variable_7 <- as.numeric(as.character(df_short$Variable_7))
df_short$Variable_15 <- as.numeric(as.character(df_short$Variable_15))
df_short$Variable_16 <- as.numeric(as.character(df_short$Variable_16))
df_short$Variable_20 <- as.numeric(as.character(df_short$Variable_20))
df_short$Variable_21 <- as.numeric(as.character(df_short$Variable_21))
df_short$Variable_22 <- as.numeric(as.character(df_short$Variable_22))
df_short$Variable_27 <- as.numeric(as.character(df_short$Variable_27))
df_short$Variable_37 <- as.numeric(as.character(df_short$Variable_37))
df_short$Variable_38 <- as.numeric(as.character(df_short$Variable_38))
df_short$Variable_39 <- as.numeric(as.character(df_short$Variable_39))

# 3. converting to ordinal variable
df_short$Variable_8 <- factor(df_short$Variable_8,
                              levels = c(0,100,199,300,500),
                              labels = c("0","100","199","300","500"),
                              ordered = TRUE)

df_short$Variable_23 <- factor(df_short$Variable_23,
                              levels = c(0,1,2,3,4,5,6,11),
                              labels = c("0","1","2","3","4","5","6","11"),
                              ordered = TRUE)

df_short$Variable_29 <- factor(df_short$Variable_29,
                               levels = c(0,1,2,3,4,5,10),
                               labels = c("0","1","2","3","4","5","10"),
                               ordered = TRUE)


#removing variable_3, varibale_5, variable_28,30,31,32,33,34 since they contain only one value (zero variance)
df_short <- df_short[,-c(3,5,33,35:39)]
# removing variable_42,43,44 since most of the dates are missing
df_short <- df_short[,-c(39:41)]
# removing variables first_status_day_date,first_status_time_of_day and arrived_date
df_short <- df_short[,-c(7,8,11)]

# converting due_date and paid_date variables to date format
df_short$due_date <- dmy(df_short$due_date)
df_short$paid_date <- dmy(df_short$paid_date)

#creating a variable to find how early the amount was paid
df_short$days_paid_earlier <- as.numeric(df_short$due_date - df_short$paid_date)

# removing due_date, paid_date variables and variable_14(duplicate of varibale 13)
df_short <- df_short[,-c(6,7,14)]
df_short<- df_short[,c(35,1:34)]


# Creating a function to find missing values
missing_values <- function(data){
  for (name in colnames(data)){
    print(paste('total missing values in', name, 'is :', sum(is.na(data[name]))))
  }
}
missing_values(df_short)



# impute missing values by classification and regression trees (CART)
# numeric impute: var 6,7,15,16,20,21,22,27,37,38,39
# cat impute : var13,17,18,19,23,24,25,26,29,35,36
all_imputed_df <- mice(df_short, m=1, maxit = 500, method = 'cart', seed = 2019)
summary(all_imputed_df)
imputed_data <- complete(all_imputed_df,1) # taking the 10th dataset. Note: we can take any of the 10 imputed dataset

df_final <- imputed_data
df_final$Target <- df_short$Target

# feature selection using LASSO through cross validation
cv_lasso <- cv.glmnet(x= model.matrix( Target ~ ., imputed_data),
                      y = imputed_data$Target,
                      family = "binomial",
                      standardize = TRUE,
                      alpha = 1,
                      nfolds = 10,
                      type.measure = "class")

lambda_and_error <- cbind(cv_lasso$lambda,cv_lasso$cvm)
plot(cv_lasso)
coef<-coef(cv_lasso,s='lambda.min',exact=TRUE)
index<-which(coef!=0)
optimum_features<-row.names(coef)[index]    
optimum_features<-optimum_features[-1]
dataset_lasso <- df_final[,c("days_paid_earlier","Variable_7","Variable_12",
                             "Variable_13","Variable_17","Variable_18","Variable_19",
                             "Variable_24","Variable_27","Variable_35",
                             "Variable_37","Variable_38","Variable_39","Variable_40",
                             "Variable_45",'Target')] 
levels(dataset_lasso$Target) <- c("no","yes")
# splitting the dataset
train_data <- subset(dataset_lasso, !is.na(Target))
test_data <- subset(dataset_lasso, is.na(Target))

#proportion of two classes in the training data
prop.table(table(imputed_data$Target))

# over sampling because of unbalanced class in target variable
new_train_data <- SMOTE(form=Target~.,data=train_data,perc.over = 100)
nrow(subset(new_train_data, Target == 'no'))
nrow(subset(new_train_data, Target == 'yes'))
nrow(subset(train_data, Target == 'no'))
nrow(subset(train_data, Target == 'yes'))

##################################################################
# Model fitting
#################################################################

######################################################
# 1. Linear classification model (logistic regression)
######################################################
# models supported by caret
names(getModelInfo())
# The tunable parameters for a given model
modelLookup("glmnet")

control <- trainControl(method="repeatedcv",
                        number = 10,
                        repeats = 10,
                        classProbs = T,
                        summaryFunction = twoClassSummary,
                        allowParallel = TRUE)


# Model fitting using cross validation
param_grid_ridge <- expand.grid(alpha = 0,lambda = seq(0.001,0.5,by = 0.001)) # alpha = 0 means ridge
model_ridge_logistic<- train(form = Target ~.,
                             data = new_train_data,
                             method = "glmnet",
                             preProcess=c("center","scale"),
                             metric = "ROC",
                             trControl = control,
                             tuneGrid = param_grid_ridge)
                             


# optimal hyperparameter
model_ridge_logistic$bestTune
#performance grid
model_ridge_logistic
#best coefficients
coef(model_ridge_logistic$finalModel,model_ridge_logistic$bestTune$lambda)
# visualization
plot(model_ridge_logistic)


# variable importance
importance_ridge_logistic <- varImp(model_ridge_logistic)
plot(importance_ridge_logistic,col="#8470ff",main="Variable Importance (Logistic Ridge  Regression)")

# prediction
logistic_pred <- predict(model_ridge_logistic,newdata = test_data[,-16])
logistic_prob <- predict(model_ridge_logistic,newdata = test_data[,-16],type = "prob")
sum(logistic_pred == 'yes')/sum(logistic_pred == 'no')

log_pred_data_class <- as.data.frame(logistic_pred)
log_pred_data_prob <- as.data.frame(logistic_prob)
log_pred_com <- cbind(log_pred_data_class,log_pred_data_prob)


write.xlsx(log_pred_com, "Logistic_regression_predictions.xlsx", row.names = F)

######################################################
# 2. Non-linear classification models
######################################################

##############################################
# A. extreme gradient boosting trees
#############################################
# Model fitting using cross validation
xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10,15,20,25),
                       eta = c(0.1,0.2,0.4),
                       gamma = 0,
                       colsample_bytree = 0.7,
                       min_child_weight = 1,
                       subsample = 1)
dim(xgbGrid)
model_xgb<- train(form = Target ~.,
                  data = new_train_data,
                  method = 'xgbTree',
                  preProcess=c("center","scale"),
                  metric = "ROC",
                  trControl = control,
                  tuneGrid = xgbGrid)



# optimal hyperparameter
model_xgb$bestTune
#performance grid
model_xgb
# visualization
plot(model_xgb)
# variable importance
importance_xgb <- varImp(model_xgb)
plot(importance_xgb,col="#8470ff",main="Variable Importance (Extreme Gradient Boosting)")


# prediction
xgb_pred <- predict(model_xgb,newdata = test_data[,-16]) 
xgb_prob <- predict(model_xgb,newdata = test_data[,-16],type = "prob") 
sum(xgb_pred == 'yes')/sum(xgb_pred == 'no')


xgb_pred_data_class <- as.data.frame(xgb_pred)
xgb_pred_data_prob <- as.data.frame(xgb_prob)
xgb_pred_com <- cbind(xgb_pred_data_class,xgb_pred_data_prob)

write.xlsx(xgb_pred_com, "xgb_predictions.xlsx", row.names = F)

###########################################
# B. random forest
###########################################
# model fittitng with grid search
control_random <- trainControl(method="repeatedcv",
                               number = 10,
                               repeats = 10,
                               classProbs = T,
                               summaryFunction = twoClassSummary,
                               search = 'random')
model_rf<- train(form = Target ~.,
                  data = new_train_data,
                  method = 'rf',
                  preProcess=c("center","scale"),
                  metric = "ROC",
                  trControl = control_random,
                  tuneLength=10)

# optimal hyperparameter
model_rf$bestTune
#performance grid
model_rf
# visualization
plot(model_rf)
# variable importance
importance_rf <- varImp(model_rf)
plot(importance_rf,col="#8470ff",main="Variable Importance (Random Forest)")


# prediction
rf_pred <- predict(model_rf,newdata = test_data[,-16]) 
rf_prob <- predict(model_rf,newdata = test_data[,-16],type = "prob") 
sum(rf_pred == 'yes')/sum(rf_pred == 'no')


rf_pred_data_class <- as.data.frame(rf_pred)
rf_pred_data_prob <- as.data.frame(rf_prob)
rf_pred_com <- cbind(rf_pred_data_class,rf_pred_data_prob)

write.xlsx(rf_pred_com, "random_forest_predictions.xlsx", row.names = F)

##########################################
# C. bagged CART (no tuning hyperparameter)
##########################################
model_cart<- train(form = Target ~.,
                 data = new_train_data,
                 method = 'treebag',
                 nbagg = 50,
                 preProcess=c("center","scale"),
                 metric = "ROC",
                 trControl = control)

# optimal hyperparameter
model_cart$bestTune
#performance grid
model_cart

# variable importance
importance_cart <- varImp(model_cart)
plot(importance_cart,col="#8470ff",main="Variable Importance (Random Forest)")

# prediction
cart_pred <- predict(model_cart,newdata = test_data[,-16]) 
cart_prob <- predict(model_cart,newdata = test_data[,-16],type = "prob") 
sum(cart_pred == 'yes')/sum(cart_pred == 'no')



cart_pred_data_class <- as.data.frame(cart_pred)
cart_pred_data_prob <- as.data.frame(cart_prob)
cart_pred_com <- cbind(cart_pred_data_class,cart_pred_data_prob)


write.xlsx(cart_pred_com, "CART_predictions.xlsx", row.names = F)


####################################################################
# Comparison of the models
####################################################################
results <- resamples(list(RF = model_rf,
                          XGB = model_xgb,
                          Logistic = model_ridge_logistic,
                          CART = model_cart))
summary(results)


# visual comparison
# 1.
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(results, layout = c(3, 1))

# 2. 
trellis.par.set(caretTheme())
dotplot(results, metric = "ROC")

# 3. 
trellis.par.set(theme1)
xyplot(results, what = "BlandAltman")

# 4.
splom(results)

# Since models are fit on the same versions 
# of the training data, it makes sense to make 
# inferences on the differences between models. 
# In this way we reduce the within-resample 
# correlation that may exist. We can compute the 
# differences, then use a simple t-test to evaluate 
# the null hypothesis that there is no difference between models.

dif_values <- diff(results)
dif_values
summary(dif_values)


# visulization
trellis.par.set(theme1)
bwplot(dif_values, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(dif_values)

