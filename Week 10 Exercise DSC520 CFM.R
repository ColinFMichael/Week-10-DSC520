# Assignment: ASSIGNMENT 10
# Name: Michael, Colin
# Date: 2021-11-06

#1.)

## Set the working directory to the root of your DSC 520 directory
library(caTools)
setwd("/Users/colinmichael/Desktop/Data Science/DSC520")

thoracic_df <- read.csv("thoracic.csv")

thoracic_df

#split<-sample.split(thoracic_df, splitratio =.8)
#split
#train <-subset(thoracic_df, split == "TRUE")

mymodel <- glm(Risk1Yr ~ AGE + PRE32 + PRE30 + PRE25 + PRE19 + PRE17 + PRE14 + PRE11 + PRE10 + PRE9 + PRE8 
               + PRE7 + PRE6 + PRE5 + PRE4 + DGN, data = thoracic_df, family = 'binomial')
summary(mymodel)

res<- predict(mymodel, type = 'response')

#The variable with the greatest effect on survival rate is DGN8, with a coefficient value of 1.803e+01

confmatrix<- table(Actual_Value=thoracic_df$AGE, Predicted_value = res >.5)

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

#The accuracy percentage of my model is 21.2%.

#2
#a
binary_df <- read.csv("binary-classifier-data.csv")

mymodel2 <- glm(label ~ x + y, data = binary_df, family = 'binomial')

mymodel2

res2<- predict(mymodel, type = 'response')

conf2matrix<- table(Actual_Value=binary_df$label, Predicted_value = res2 >.5)

(conf2matrix[[1,1]] + conf2matrix[[2,2]]) / sum(conf2matrix)

#The accuracy of the model is 58.3%


