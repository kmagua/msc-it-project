setwd('Project')
#the Neural Network Package
library(neuralnet)
#Caret is used for creating the confusion matrix
library(caret)
#K fold cross validation package
library(modelr)
library(plyr)

#XXXXXXXXXXXXXXXX The CUSTOM MODEL FITTING FUNCTION XXXXXXXXXXXXXXXXXXXXX
fit_model <- function(k, folding,f,scene){
  accuracy <- matrix(nrow = k, ncol = 3)
  for(i in 1:k){
    train.data <- as.data.frame(folding$train[[i]])
    test.data <- as.data.frame(folding$test[[i]])
    n_cols <- ncol(train.data) - 1
    table(train.data$classnotckd)
    nn <- neuralnet(f, data=train.data, hidden=c(5,4), linear.output = T)
    plot(nn)
    dev.copy(png, paste("./images/fold - ",i , scene , " model.png"))
    dev.off()
    #Test the model
    pr.nn <- compute(nn,test.data[,1:n_cols])
    #Confusion Matrix & Classification error. Saved as png
    prediction <- pr.nn$net.result
    classified_prediction <- ifelse(prediction>0.5,'No CKD','CKD')
    test_data <- ifelse(test.data$classnotckd == 1, 'No CKD','CKD')
    cm <- confusionMatrix(factor(classified_prediction), factor(test_data))
    accuracy[i,1] <- cm$overall['Accuracy']
    accuracy[i,2] <- cm$overall['Kappa']
    accuracy[i,3] <- cm$overall['AccuracyPValue']
    confusion_matrix <- as.table(cm)
    png(file = paste("./images/fold - ",i , scene , " confusion matrix.png"))
    fourfoldplot(confusion_matrix)
    dev.off()
    
    #Save the merged training and test data to a file for current fold
    train.data$type <- 'train'
    test.data$type <- 'test'
    write.csv(rbind(train.data, test.data), file = paste("./10-fold-data/",i , scene , " - fold.csv"),row.names=FALSE)
    pbar$step()
  }
  return (accuracy)
}
#XXXXXXXXXXXXXXXX CUSTOM MODEL ACCURACY PLOT FUNCTION XXXXXXXXXXXXXXXXXXXXX
calculateAccuracy <- function(accuracy, chartTitle, k){
  colnames(accuracy) <- c("Accuracy", "Kappa", "AccuracyPValue")
  accuracy <- rbind(accuracy, colMeans(accuracy))
  accuracy_results <- as.data.frame(accuracy)
  
  #calc percentage accuracy
  accuracy_results$PercentageAccuracy <- accuracy_results$Accuracy * 100
  df.mean = accuracy_results %>% 
    mutate(ymean = mean(PercentageAccuracy))
  #created as a factor to prevent inadvertent ordering of X axis
  x <- factor(c(row.names(accuracy_results)), levels = c(row.names(accuracy_results)))
  y <- accuracy_results$PercentageAccuracy
  ggplot(accuracy_results, aes(x, y, fill=Accuracy)) +
    geom_col() +
    ggtitle(chartTitle) +
    labs(caption = paste("Red line is for Average Accuracy at ", accuracy_results$PercentageAccuracy[k+1], '%')) + 
    ylab('% Accuracy') +
    xlab('Cross Validation Folds') +
    geom_errorbar(data=df.mean, aes(x, ymax = ymean, ymin = ymean),
                  size=0.5, linetype = "longdash", inherit.aes = F, width = 1, color = 'red')
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                             SECTION 0
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                           Data Preprocessing for Initial complete cases
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#convert missing value character to R's Missing value keyword
ckd <- read.csv("chronic_kidney_disease.csv", na.strings = c("?", "\t?", ""), stringsAsFactors = TRUE)
#write back the tranformed data into a separate csv
write.csv(ckd, file = "ckd_data_converted.csv",row.names=FALSE, na="NA")
#write complete cases to a new CSV file
no_missing <- na.omit(ckd)
write.csv(no_missing, file = "original_complete_cases.csv",row.names=FALSE)

# read complete cases file
ckd <- read.csv("original_complete_cases.csv", stringsAsFactors = TRUE)
#XXXXXXXXXXXXXXXX Data preprocessing XXXXXXXXXXXXXXXXXXXXX
#Scale numeric attributes
numeric_cols <- sapply(ckd, is.numeric)
standardize_data <- function(x){ (x - min(x))/(max(x) - min(x)) }
ckd[numeric_cols] <- lapply(ckd[numeric_cols], standardize_data)
#str(ckd)
write.csv(ckd, file = "original_complete_cases_numeric_scaled.csv",row.names=FALSE)
ckd <- read.csv("original_complete_cases_numeric_scaled.csv", stringsAsFactors = TRUE)
#convert factors to numeric by changing them into new columns +0/-1 removes the intercept column from the dataset
converted_to_numeric_column <- model.matrix(~age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wc+rc+htn+dm+cad+appet+pe+ane+class+0, ckd) 

#save the transformed data to a file
write.csv(converted_to_numeric_column, file = "transformed_data.csv",row.names=FALSE)

# RANDOMIZE THE DATA
randomize_data <- function(data, filename){
  set.seed(113)
  rows <- sample(nrow(data))
  data <- data[rows, ]
  write.csv(data, file = filename,row.names=FALSE)
  return (data)
}

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                         SECTION 1
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#               1 st test is with the categorical data converted to binary without scaling
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#randomize data
data = read.csv('transformed_data.csv')
data <- randomize_data(data, "randomized_records.csv")
#find Linear correlation of the outcome with other attributes
res <- cor(data,method = "kendall")
write.table(round(res, 2), file='./images/my_data_new.txt', sep="\t")

# XXXXXXXXXXXXXXXXXXXXXXXXXXXX Fitting the preprocessed original cases XXXXXXXXXXXXXXXXXXXXXXXXXXXX
attribute_labels <- names(data)
f <- as.formula(paste("classnotckd ~", paste(attribute_labels[!attribute_labels %in% "classnotckd"], collapse = " + ")))
k <- 6
pbar <- create_progress_bar('text')
pbar$init(k)
set.seed(769)
folding <- crossv_kfold(data,k)
confusion_matrix <- NULL
fit_model(k,folding, f, ' - Original Data - ')


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                         SECTION 2
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#         FITTING THE MODEL with data whose correlation with the outcome variable >60 
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#data prep, USING THE SAME randomized data as the previous model
data < read.csv('randomized_records.csv')
correlation <- read.csv('attributes_correlation.txt')
features <- correlation[abs(correlation$classnotckd) <=0.6, 1]
features <- as.vector(features)
new_data <- subset(data, select=append(features, 'classnotckd'))

# XXXXXXXXXXXXXXXXXXXXXXXXXXXX Fitting the lower (<=60) correlation data XXXXXXXXXXXXXXXXXXXXXXXXXXXX
attribute_labels <- names(new_data)
f <- as.formula(paste("classnotckd ~", paste(attribute_labels[!attribute_labels %in% "classnotckd"], collapse = " + ")))
k <- 6
pbar <- create_progress_bar('text')
pbar$init(k)
set.seed(769)
folding <- crossv_kfold(new_data,k)
confusion_matrix <- NULL
fit_model(k,folding, f, ' - Correlation less than 60 - ')


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                                         SECTION 3
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#                           FITTING THE MODEL with imputed data 
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
ckd = read.csv('original_imputed_data.csv')
numeric_cols <- sapply(ckd, is.numeric)
standardize_data <- function(x){ (x - min(x))/(max(x) - min(x)) }
ckd[numeric_cols] <- lapply(ckd[numeric_cols], standardize_data)
#str(ckd)
write.csv(ckd, file = "imputed_cases_numeric_scaled.csv",row.names=FALSE)
ckd <- read.csv("imputed_cases_numeric_scaled.csv", stringsAsFactors = TRUE)
#convert factors to numeric by changing them into new columns +0/-1 removes the intercept column from the dataset
converted_to_numeric_column <- model.matrix(~age+bp+sg+al+su+rbc+pc+pcc+ba+bgr+bu+sc+sod+pot+hemo+pcv+wc+rc+htn+dm+cad+appet+pe+ane+class+0, ckd)
#save the transformed data to a file
write.csv(converted_to_numeric_column, file = "transformed_imputed_data.csv",row.names=FALSE)
data <- read.csv('transformed_imputed_data.csv')
data <- randomize_data(data, "randomized_imputed_records.csv")

# XXXXXXXXXXXXXXXXXXXXXXXXXXXX Fitting the Imputed data model XXXXXXXXXXXXXXXXXXXXXXXXXXXX
attribute_labels <- names(data)
f <- as.formula(paste("classnotckd ~", paste(attribute_labels[!attribute_labels %in% "classnotckd"], collapse = " + ")))
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)
set.seed(769)
folding <- crossv_kfold(data,k)
accuracy <- fit_model(k,folding, f, ' - Imputed Data - ')
# XXXXXXXXXXXXXXXXXXXXXXXXXXXX Model Accuracy Plot XXXXXXXXXXXXXXXXXXXXXXXXXXXX
calculateAccuracy(accuracy, "Imputed Data Accuracy Plot",k)
