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

str(anesNew)



model1<-lm(anesNew$obama_feeling ~ anesNew$campaign_finance + anesNew$inequality_larger)
model2<-lm(anesNew$obama_feeling ~ anesNew$vp_feelings + anesNew$hillary_feelings)


anesNew<-as.data.frame(anesNew)

liberal_rating_model<-lm(obama_feeling_regression_data ~ liberal_rating_regression_data)

liberal_rating_prediction<-predict(liberal_rating_model)

?predict
