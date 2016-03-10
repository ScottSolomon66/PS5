## 4625 - R Programming
## Problem Set 5
## March 10
## Scott Solomon

rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("/Users/scottsolomon/Documents/Git/Class/Problem Set 5/anes_timeseries_2012_stata12.dta")


## making it a data frame
anes<-as.data.frame(anes)

##setting up the blank data frame to be used with the varibles I want
anesNew<-NULL


## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score

model1 <- lm(anes$ft_dpc ~ anes$ft_hclinton)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))

predict.lm(model1)

## we would expect a Obama score of 71.7


## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness


## first I am finding variables that I will use for my model


## obama_feeling (the dependent variable)
obama_feeling<-anes$ft_dpc # importing the data
missing_obama_feeling<-which(obama_feeling < 0) # finding the no responses, which are coded as negatives
obama_feeling[missing_obama_feeling]<-NA # coding the negatives NA
# placing the data into the blank data.frame
anesNew$obama_feeling<-obama_feeling

###### variables for model 1 ###### 

## campaign_finance (campfin_limcorp)
## codes whether people want to limit corporate donations or not

campaign_finance<-anes$campfin_limcorp # grabbing the data
campaign_finance<-as.character(campaign_finance) # making it a character 
campaign_finance<-substr(campaign_finance, 0, 1) # substringing the important characters
missing_campaign_finance<-grep("-", campaign_finance) # finding the negative (misssing) values
campaign_finance[missing_campaign_finance]<-NA # coding them NA
indifferent<-grep("3", campaign_finance) # finding the no responses
campaign_finance[indifferent]<-NA # coding them as NA
campaign_finance<-as.numeric(campaign_finance) # making it numeric
## adding it to the data.frame
anesNew$campaign_finance<-campaign_finance

## inequality_larger (ineq_incgap)
## respondent states whether they think inequality is larger or smaller than 20 years ago
## the process is the same here as it is for campaign_finance

inequality_larger<-anes$ineq_incgap
inequality_larger<-as.character(inequality_larger)
inequality_larger<-substr(inequality_larger, 0, 1)
missing_inequality_larger<-grep("-", inequality_larger)
inequality_larger[missing_inequality_larger]<-NA
indifferent<-grep("3", inequality_larger)
inequality_larger[indifferent]<-NA
inequality_larger<-as.numeric(inequality_larger)

anesNew$inequality_larger<-inequality_larger

###### variables for model 2 ###### 

## vp_feelings (ftpo_dvpc)
## gives respondents feelings on the vp candidate from 0 to 100
## follows the same process as obama_feeling

vp_feelings<-anes$ftpo_dvpc # grabbing the data
vp_feelings_missing<-which(vp_feelings < 0)
vp_feelings[vp_feelings_missing]<-NA

anesNew$vp_feelings<-vp_feelings

## hillary_feelings
## gives respondents feelings on hillary clinton from 0 to 100
## follows the same process as obama_feeling and vp_feeling

hillary_feelings<-anes$ft_hclinton
hillary_feelings_missing<-which(hillary_feelings < 0)
hillary_feelings[hillary_feelings_missing]<-NA

anesNew$hillary_feelings<-hillary_feelings

###### variables for model 3 ###### 

## rep_econ_blame (ecblame_rep)
## whether or not respondents blame republicans for the economy
## follows same pattern as campaign_finance and income_inequality

rep_econ_blame<-anes$ecblame_rep
rep_econ_blame<-as.character(rep_econ_blame)
rep_econ_blame<-substr(rep_econ_blame, 0, 1)
rep_econ_blame_missing<-grep("-", rep_econ_blame)
rep_econ_blame[rep_econ_blame_missing]<-NA
rep_econ_blame<-as.numeric(rep_econ_blame)

anesNew$rep_econ_blame<-rep_econ_blame

## banks_econ_blame (ecblame_rep)
## whether or not respondents blame big banks for the economy
## follows same pattern as rep_econ_blame

banks_econ_blame<-anes$ecblame_bank
banks_econ_blame<-as.character(banks_econ_blame)
banks_econ_blame<-substr(banks_econ_blame, 0, 1)
banks_econ_blame_missing<-grep("-", banks_econ_blame)
banks_econ_blame[banks_econ_blame_missing]<-NA
banks_econ_blame<-as.numeric(banks_econ_blame)

anesNew$banks_econ_blame<-banks_econ_blame


## have to remove the obama_feeling where it's zero because of a divide by 0 error later
obama_zero<-which(anesNew$obama_feeling == 0)
anesNew$obama_feeling[obama_zero]<-NA

## making it a data frame
anesNew<-as.data.frame(anesNew)

## ommitting the NA values from the data frame
anesNew<-na.omit(anesNew)

## subsetting the data into the training and test sets
anesNew_training<-anesNew[1:1363,]
anesNew_test<-anesNew[1364:2726,]

##### Question 2 ######

## making the linear regression models based off of the variables subsetting above
## this is performed on the test function
model1<-lm(anesNew_training$obama_feeling ~ anesNew_training$campaign_finance + anesNew_training$inequality_larger, data=anesNew_training)
model2<-lm(anesNew_training$obama_feeling ~ anesNew_training$vp_feelings + anesNew_training$hillary_feelings, data=anesNew_training)
model3<-lm(anesNew_training$obama_feeling ~ anesNew_training$banks_econ_blame + anesNew_training$rep_econ_blame, data=anesNew_training)

## making the predictions based off of the linear regression models above
model1_prediction<-predict(model1, anesNew_training[,2:3])
model2_prediction<-predict(model2, anesNew_training[,4:5])
model3_prediction<-predict(model3, anesNew_training[,6:7])

## making the 3 models into one matrix
model_prediction_matrix<-cbind(model1_prediction, model2_prediction, model3_prediction)

## making the values in the matrix numeric
model_matrix<-apply(model_prediction_matrix, MARGIN = 1, FUN = function(model_prediction_matrix){
  as.numeric(model_prediction_matrix)}
  )

#ensuring it is  a matrix
model_matrix<-as.matrix(model_matrix)

####### Question 3 ###########

## making the equations for 

### median_takes_vector() ###
## this function takes the vector of observed value and the matrix of predicted values
## it returns the median

median_takes_matrix<-function(observed, predicted){
  median_observed<-median(observed) # finding the median of the observed
  median_predicted<-apply(predicted, MARGIN = 1, FUN = function(predicted){
    median(predicted) # applying the median to all the rows in the matrix
  }
  )
  # subsetting all of the values
  median_model1<-median_predicted[1] 
  median_model2<-median_predicted[2]
  median_model3<-median_predicted[3]
  # returning the values as a list
  return(list(median_observed, median_model1, median_model2, median_model3)) 
}

### median_differnce() ###
## takes two objects and find the mean between the two

median_difference<-function(x,y){
  # running the median calculuations
  median_difference_stat<-median(x)-median(y)
  # returning the values
  return(median_difference_stat)
}

### median_difference_takes_vector() ###
## takes the observed vector and the predicted matrix
## returns the difference in median from predicted and observed for each model

median_difference_takes_vector<-function(observed, predicted){
  median_difference_observed<-median_difference(observed, observed)
  median_difference_predicted<-apply(predicted, MARGIN = 1, FUN = function(x = observed, y = predicted){
    median_difference(observed, predicted)
  }
  )
  median_difference_model1<-median_difference_predicted[1]
  median_difference_model2<-median_difference_predicted[2]
  median_difference_model3<-median_difference_predicted[3]
  return(list(median_difference_observed, median_difference_model1, median_difference_model2, median_difference_model3))
}

### rmse() ###
## takes a vector and returns the rmse statistic

rmse<-function(x){
  rmse_stat<-(sum(x^2)/length(x))
  rmse_stat<-sqrt(rmse_stat)
  return(rmse_stat)
}

### rmse_takes_matrix() ###
## takes the observed vector and predicted matrix and returns the rmse statistics

rmse_takes_matrix<-function(observed, predicted){
  rmse_observed<-rmse(observed)
  rmse_predicted<-apply(predicted, MARGIN = 1, FUN = function(predicted){
    rmse(predicted)
  }
  )
  rmse_model1<-rmse_predicted[1]
  rmse_model2<-rmse_predicted[2]
  rmse_model3<-rmse_predicted[3]
  return(list(rmse_observed, rmse_model1, rmse_model2, rmse_model3))
}

### rmsle() ###
## takes two values and finds the rmsle statistic

rmsle<-function(x, y){
  rmsle_stat<-(sum(log(y+1)-log(x+1)))^2
  rmsle_stat<-(rmsle_stat)/length(x)
  rmsle_stat<-sqrt(rmsle_stat)
  return(rmsle_stat)
}

### rmsle_take_matrix() ###
## takes an observed vector and predixced matrix vector and gives the rmsle statistics

rmsle_takes_matrix<-function(observed, predicted){
  rmsle_observed<-rmsle(observed, observed)
  rmsle_predicted<-apply(predicted, MARGIN = 1, FUN = function(x = observed, y =predicted){
    rmsle(predicted, observed)
  }
  )
  rmsle_model1<-rmsle_predicted[1]
  rmsle_model2<-rmsle_predicted[2]
  rmsle_model3<-rmsle_predicted[3]
  return(list(rmsle_observed, rmsle_model1, rmsle_model2, rmsle_model3))
}

### mape() ###
## takes two objects and returns the mape statistic

mape<-function(x, y){
  mape_stat<-sum(abs(y-(x))/abs((x)))
  mape_stat<-mape_stat*100
  mape_stat<-mape_stat/length(y)
  return(mape_stat)
}

### mape_takes_matrix() ###
## takes an observed vector and predixced matrix vector and gives the mape statistics

mape_takes_matrix<-function(observed, predicted){
  mape_observed<-mape(x = observed, y = observed)
  mape_predicted<-apply(predicted, MARGIN = 1, FUN = function(x = observed, y = predicted){
    mape(observed, predicted)
  }
  )
  mape_model1<-mape_predicted[1]
  mape_model2<-mape_predicted[2]
  mape_model3<-mape_predicted[3]
  return(list(mape_observed, mape_model1, mape_model2, mape_model3))
}

### meape() ###
## takes two objects and returns the meape statistic

meape<-function(x, y){
  meape_stat<-(abs(y-(x))/abs((x)))
  meape_stat<-meape_stat*100
  meape_stat<-median(meape_stat)
  return(meape_stat)
}

### meape_takes_matrix() ###
## takes an observed vector and predixced matrix vector and gives the meape statistics

meape_takes_matrix<-function(observed, predicted){
  meape_observed<-meape(observed, observed)
  meape_predicted<-apply(predicted, MARGIN = 1, FUN = function(x = observed, y = predicted){
    meape(observed, predicted)
  }
  )
  meape_model1<-meape_predicted[1]
  meape_model2<-meape_predicted[2]
  meape_model3<-meape_predicted[3]
  return(list(meape_observed, meape_model1, meape_model2, meape_model3))
}

### fit_stats() ###
## takes ## takes an observed vector and predixced matrix vector
## returns median, median difference, rmse, rmsle, mape, meape in a matrix with proper labels
## allows you to choose which statistics you want returned in the matrix (Question 4)

fit_stats<-function(observed, predicted, median = T, median_diff = T, rmse = T, rmsle = T, mape = T, meape = T){
  ## these functions determine which statistics are returned
  if (median == T) median_stat<-median_takes_matrix(observed, predicted)
  else median_stat<-NULL
  
  if (median_diff == T) median_diff_stat<-median_difference_takes_vector(observed, predicted)
  else median_diff_stat<-NULL
  
  if (rmse == T) rmse_stat<-rmse_takes_matrix(observed, predicted)
  else rmse_stat<-NULL
  
  if (rmsle == T) rmsle_stat<-rmsle_takes_matrix(observed, predicted)
  else rmsle_stat<-NULL
  
  if (mape == T) mape_stat<-mape_takes_matrix(observed, predicted)
  else mape_stat<-NULL
  
  if (meape == T) meape_stat<-meape_takes_matrix(observed, predicted)
  else meape_stat<-NULL
  
  ## creating the matrix to return all the values
  
  ## combining the statistics
  fit_stat_matrix<-cbind(median_stat, median_diff_stat, rmse_stat, rmsle_stat, mape_stat, meape_stat)
  ## labeling the rows
  row_labels<-c("Observed", "Model 1 Prediction", "Model 2 Prediction", "Model 3 Prediction")
  ## turning it into a matrix
  fit_stat_matrix<-as.data.frame(fit_stat_matrix, row.names = row_labels)
  return(fit_stat_matrix)
}

## running the function to test if for the training subset
fit_stats(anesNew_training$obama_feeling, model_matrix)

### Question 5 ###

## running the predictions for the test data
model1_test_prediction<-predict(model1, anesNew_test[,2:3])
model2_test_prediction<-predict(model2, anesNew_test[,4:5])
model3_test_prediction<-predict(model3, anesNew_test[,6:7])

## making it into a matrix
model_test_prediction_matrix<-cbind(model1_test_prediction, model2_test_prediction, model3_test_prediction)

## making all of the values numeric
model_test_matrix<-apply(model_test_prediction_matrix, MARGIN = 1, FUN = function(model_test_prediction_matrix){
  as.numeric(model_test_prediction_matrix)}
)

## ensuring its a matrix
model_test_matrix<-as.matrix(model_test_matrix)


## running fit_stats for the test values
fit_stats(anesNew_test$obama_feeling, model_test_matrix)
