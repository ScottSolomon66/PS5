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



## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score

model1 <- lm(anes$ft_dpc ~ anes$ft_hclinton)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))

ft_hclinton

## we would expect a Obama score of 71.7


## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness
