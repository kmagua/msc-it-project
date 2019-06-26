setwd('Project')
#the Neural Network Package
library(neuralnet)
#boot is used for crossvalidation
library(caret)
library(e1071)
#K fold cross validation package
library(modelr)
library(plyr)
#used to shuffle data
library(merTools)
library(xlsx)

#convert missing value character to R's Missing value keyword
ckd <- read.csv("chronic_kidney_disease.csv", na.strings = c("?", "\t?"), stringsAsFactors = TRUE)
#write back the tranformed data into a separate csv
write.csv(ckd, file = "ckd_data_converted.csv",row.names=FALSE, na="")
#write complete cases to a new CSV file
no_missing <- na.omit(ckd)
write.csv(no_missing, file = "original_complete_cases.csv",row.names=FALSE)

# read complete cases file
ckd <- read.csv("original_complete_cases.csv", stringsAsFactors = TRUE)
#Scale numeric attributes
numeric_cols <- sapply(ckd, is.numeric)
standardize_data <- function(x){ (x - min(x))/(max(x) - min(x)) }
ckd[numeric_cols] <- lapply(ckd[numeric_cols], standardize_data)
#str(ckd)
write.csv(ckd, file = "original_complete_cases_numeric_scaled.csv",row.names=FALSE)
ckd <- read.csv("original_complete_cases_numeric_scaled.csv", stringsAsFactors = TRUE)
#convert factors to numeric by changing them into new columns +0/-1 removes the intercept column from the dataset
converted_to_numeric_column <- model.matrix(~age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wc+rc+htn+dm+cad+appet+pe+ane+class+0, ckd) 
head(converted_to_numeric_column)

#save the transformed data to a file
write.csv(converted_to_numeric_column, file = "transformed_data.csv",row.names=FALSE)
data = read.csv('transformed_data.csv')
#find correlation between attributes
correlation <- cor(data$pcnormal, data$classnotckd, use="complete.obs", method="kendall") 

#XXXXXXXXXXXXXXXX The CUSTOM MODEL FITTING FUNCTION XXXXXXXXXXXXXXXXXXXXX
columns <- NULL
fit_model <- function(k, folding, scene, columns){
  for(i in 1:k){
    train.data <- as.data.frame(folding$train[[i]])
    test.data <- as.data.frame(folding$test[[i]])
    n_cols <- ncol(train.data) - 1
    table(train.data$classnotckd)
    nn <- neuralnet(f, data=train.data, hidden=c(5,4), linear.output = T)
    #png(file = paste("./images/fold - ",i , scene , " model.png"))
    #print(plot(nn))
    #dev.off()
    #Test the model
    pr.nn <- compute(nn,test.data[,1:n_cols])
    #Confusion Matrix & Classification error. Saved as png
    prediction <- pr.nn$net.result
    classified_prediction <- ifelse(prediction>0.5,'No CKD','CKD')
    test_data <- ifelse(test.data$classnotckd == 1, 'No CKD','CKD')
    cm <- confusionMatrix(factor(classified_prediction), factor(test_data))
    confusion_matrix <- as.table(cm)
    png(file = paste("./images/fold ",i , scene , " confusion matrix.png"))
    fourfoldplot(confusion_matrix)
    dev.off()
    
    #Save the merged training and test data to a file for current fold
    train.data$type <- 'train'
    test.data$type <- 'test'
    write.csv(rbind(train.data, test.data), file = paste("./10-fold-data/",i , scene , " - fold.csv"),row.names=FALSE)
    pbar$step()
  }
}

# RANDOMIZE THE DATA
rows <- sample(nrow(data))
data <- data[rows, ]
write.csv(data, file = "randomized_records.csv",row.names=FALSE)

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#               1 St test is with the categorical data converted to binary without scaling
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#randomize data
data < read.csv('randomized_records.csv')
set.seed(769)
#find attribute correlation
res <- cor(data)
write.table(round(res, 2), file='./images/my_data.txt', sep="\t")

attribute_labels <- names(data)
f <- as.formula(paste("classnotckd ~", paste(attribute_labels[!attribute_labels %in% "classnotckd"], collapse = " + ")))
k <- 6
pbar <- create_progress_bar('text')
pbar$init(k)
set.seed(897)
folding <- crossv_kfold(data,k)
confusion_matrix <- NULL
fit_model(k,folding)


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#         FITTING THE MODEL with data whose correlation with the outcome variable >60 
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#data prep
data < read.csv('randomized_records.csv')
correlation <- read.csv('attributes_correlation.txt')
features <- correlation[abs(correlation$classnotckd) <=0.6, 1]
features <- as.vector(features)
new_data <- subset(data, select=append(features, 'classnotckd'))

#
attribute_labels <- names(new_data)
f <- as.formula(paste("classnotckd ~", paste(attribute_labels[!attribute_labels %in% "classnotckd"], collapse = " + ")))
k <- 6
pbar <- create_progress_bar('text')
pbar$init(k)
set.seed(897)
folding <- crossv_kfold(new_data,k)
confusion_matrix <- NULL
fit_model(k,folding, 'Correlation less than 60')

