## 4625 - R Programming
## Problem Set 5
## March 10
## Name

rm(list=ls())
set.seed(12435)
options(stringsAsFactors=F)
library(foreign)

## read in data
anes <- read.dta("/Users/scottsolomon/Documents/Git/Class/Problem Set 5/anes_timeseries_2012_stata12.dta")

anes<-as.data.frame(anes)

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



obama_feeling<-anes$ft_dpc
missing_obama_feeling<-which(obama_feeling < 0)
obama_feeling[missing_obama_feeling]<-NA

anesNew$obama_feeling<-obama_feeling

liberal_rating<-anes$libcpre_self
liberal_rating<-as.character(liberal_rating)
liberal_rating<-substr(liberal_rating, 0, 1)
missing_liberal_rating<-grep("-",liberal_rating)
liberal_rating[missing_liberal_rating]<-NA
liberal_rating<-as.numeric(liberal_rating)

anesNew$liberal_rating<-liberal_rating

campaign_finance<-anes$campfin_limcorp
campaign_finance<-as.character(campaign_finance)
campaign_finance<-substr(campaign_finance, 0, 1)
missing_campaign_finance<-grep("-", campaign_finance)
campaign_finance[missing_campaign_finance]<-NA
indifferent<-grep("3", campaign_finance)
campaign_finance[indifferent]<-NA
campaign_finance<-as.numeric(campaign_finance)

anesNew$campaign_finance<-campaign_finance

inequality_larger<-anes$ineq_incgap
inequality_larger<-as.character(inequality_larger)
inequality_larger<-substr(inequality_larger, 0, 1)
missing_inequality_larger<-grep("-", inequality_larger)
inequality_larger[missing_inequality_larger]<-NA
indifferent<-grep("3", inequality_larger)
inequality_larger[indifferent]<-NA
inequality_larger<-as.numeric(inequality_larger)

anesNew$inequality_larger<-inequality_larger

vp_feelings<-anes$ftpo_dvpc
vp_feelings_missing<-which(vp_feelings < 0)
vp_feelings[vp_feelings_missing]<-NA

anesNew$vp_feelings<-vp_feelings

hillary_feelings<-anes$ft_hclinton
hillary_feelings_missing<-which(hillary_feelings < 0)
hillary_feelings[hillary_feelings_missing]<-NA

anesNew$hillary_feelings<-hillary_feelings

rep_econ_blame<-anes$ecblame_rep
rep_econ_blame<-as.character(rep_econ_blame)
rep_econ_blame<-substr(rep_econ_blame, 0, 1)
rep_econ_blame_missing<-grep("-", rep_econ_blame)
rep_econ_blame[rep_econ_blame_missing]<-NA
rep_econ_blame<-as.numeric(rep_econ_blame)

anesNew$rep_econ_blame<-rep_econ_blame

banks_econ_blame<-anes$ecblame_bank
banks_econ_blame<-as.character(banks_econ_blame)
banks_econ_blame<-substr(banks_econ_blame, 0, 1)
banks_econ_blame_missing<-grep("-", banks_econ_blame)
banks_econ_blame[banks_econ_blame_missing]<-NA
banks_econ_blame<-as.numeric(banks_econ_blame)

anesNew$banks_econ_blame<-banks_econ_blame

obama_zero<-which(anesNew$obama_feeling == 0)

anesNew$obama_feeling[obama_zero]<-NA

anesNew<-as.data.frame(anesNew)

anesNew<-na.omit(anesNew)


anesNew_training<-anesNew[1:1462,]
anesNew_test<-anesNew[1463:2925,]



model1<-lm(anesNew_training$obama_feeling ~ anesNew_training$campaign_finance + anesNew_training$inequality_larger, data=anesNew_training)
model2<-lm(anesNew_training$obama_feeling ~ anesNew_training$vp_feelings + anesNew_training$hillary_feelings, data=anesNew_training)
model3<-lm(anesNew_training$obama_feeling ~ anesNew_training$banks_econ_blame + anesNew_training$rep_econ_blame, data=anesNew_training)

model1_prediction<-predict(model1, anesNew_training[,2:3])
model2_prediction<-predict(model2, anesNew_training[,4:5])
model3_prediction<-predict(model3, anesNew_training[,6:7])

model_prediction_matrix<-cbind(model1_prediction, model2_prediction, model3_prediction)

model_matrix<-apply(model_prediction_matrix, MARGIN = 1, FUN = function(model_prediction_matrix){
  as.numeric(model_prediction_matrix)}
  )

model_matrix<-as.matrix(model_matrix)


median_takes_vector<-function(observed, predicted){
  median_observed<-median(observed)
  median_predicted<-apply(predicted, MARGIN = 1, FUN = function(predicted){
    median(predicted)
  }
  )
  median_model1<-median_predicted[1]
  median_model2<-median_predicted[2]
  median_model3<-median_predicted[3]
  return(list(median_observed, median_model1, median_model2, median_model3))
}

rmse<-function(x){
  rmse_stat<-(sum(x^2)/length(x))
  rmse_stat<-sqrt(rmse_stat)
  return(rmse_stat)
}

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

rmsle<-function(x, y){
  rmsle_stat<-(sum(log(y+1)-log(x+1)))^2
  rmsle_stat<-(rmsle_stat)/length(x)
  rmsle_stat<-sqrt(rmsle_stat)
  return(rmsle_stat)
}

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


mape<-function(x, y){
  mape_stat<-sum(abs(y-(x))/abs((x)))
  mape_stat<-mape_stat*100
  mape_stat<-mape_stat/length(y)
  return(mape_stat)
}

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



meape<-function(x, y){
  meape_stat<-(abs(y-(x))/abs((x)))
  meape_stat<-meape_stat*100
  meape_stat<-median(meape_stat)
  return(meape_stat)
}

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

meape_takes_matrix(anesNew_training$obama_feeling, model_matrix)

fit_stats<-function(observed, predicted){
  median_stat<-median_takes_vector(observed, predicted)
  rmse_stat<-rmse_takes_matrix(observed, predicted)
  rmsle_stat<-rmsle_takes_matrix(observed, predicted)
  mape_stat<-mape_takes_matrix(observed, predicted)
  meape_stat<-meape_takes_matrix(observed, predicted)
  fit_stat_matrix<-cbind(median_stat, rmse_stat, rmsle_stat, mape_stat, meape_stat)
  row_labels<-c("Observed", "Model 1 Prediction", "Model 2 Prediction", "Model 3 Prediction")
  fit_stat_matrix<-as.data.frame(fit_stat_matrix, row.names = row_labels)
  return(fit_stat_matrix)
}

fit_stats(anesNew_training$obama_feeling, model_matrix)


