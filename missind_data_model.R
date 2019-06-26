setwd('Project')
#the Neural Network Package
library(neuralnet)
#Caret is used for creating the confusion matrix
library(caret)
#K fold cross validation package
library(modelr)
library(plyr)
#XXXXXXXXXXXXXXXX Data preprocessing XXXXXXXXXXXXXXXXXXXXX
#Scale numeric attributes
ckd = read.csv('original_imputed_data.csv')
numeric_cols <- sapply(ckd, is.numeric)
standardize_data <- function(x){ (x - min(x))/(max(x) - min(x)) }
ckd[numeric_cols] <- lapply(ckd[numeric_cols], standardize_data)
#str(ckd)
write.csv(ckd, file = "imputed_cases_numeric_scaled.csv",row.names=FALSE)
ckd <- read.csv("imputed_cases_numeric_scaled.csv", stringsAsFactors = TRUE)
#convert factors to numeric by changing them into new columns +0/-1 removes the intercept column from the dataset
converted_to_numeric_column <- model.matrix(~age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wc+rc+htn+dm+cad+appet+pe+ane+class+0, ckd) 
head(converted_to_numeric_column)

#save the transformed data to a file
write.csv(converted_to_numeric_column, file = "transformed_imputed_data.csv",row.names=FALSE)
data = read.csv('transformed_imputed_data.csv')