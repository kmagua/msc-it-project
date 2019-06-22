setwd('Project')
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
ckd[numeric_cols] <- lapply(ckd[numeric_cols], scale)
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
correlation
#XXXXXXXXXXXXXX LOAD NEURALNT PACKAGE XXXXXXXXXXXXXX
library(neuralnet)
#boot is used for crossvalidation
library(boot)
set.seed(769)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
f <- as.formula(paste("classnotckd ~", paste(n[!n %in% "classnotckd"], collapse = " + ")))
nn_model.cor = NULL
for(i in 1:k){
    index <- sample(1:nrow(data),round(0.9*nrow(data)))
    train.cv <- data[index,]
    
    test.cv <- data[-index,]
    
    n <- names(train.cv)
    nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
    pr.nn <- compute(nn,test.cv[,1:25])
    pr.nn <- pr.nn$net.result #*(max(data$classnotckd)-min(data$classnotckd))+min(data$classnotckd)
    nn_model.cor[i] <- cor(pr.nn, test.cv$classnotckd)
    #test.cv.r <- (test.cv$classnotckd)*(max(data$classnotckd)-min(data$classnotckd))+min(data$classnotckd)   
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    
    #plot(nn)
    #Save the merged training and test data to a file for current fold
    train.cv$type <- 'train'
    test.cv$type <- 'test'
    write.csv(rbind(train.cv, test.cv), file = paste("./10-fold-data/",i , " - fold.csv"),row.names=FALSE)
    pbar$step()
}
cv.error
mean(cv.error)

nn_model.cor
mean(nn_model.cor)
