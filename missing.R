setwd('Project')
#used to visualize missing data
library(visdat)
library(ggplot2)
library(dplyr)
library(naniar)
#used for data imputation
library(VIM)
#used to print summary statistics
library(skimr)
ckd.data <-read.csv('ckd_data_converted.csv')
#create a plot to visualize the whole dataset
vis_miss(ckd.data, sort_miss=TRUE)
png('./images/data_visual_initial.png')
vis_dat(ckd.data)
dev.off()
#create a plot to visualize missing variable values sorted from the greatest
png('./images/data_visual_missing_variable_percentage.png')
vis_miss(ckd.data, sort_miss = TRUE)
dev.off()
#number of missing values in each case
png('./images/data_missing_values_per_record.png')
gg_miss_case(ckd.data)+ labs(x = "Records", y = "Number of Attributes")
dev.off()
miss_case_summary(ckd.data)

#imputations for missing data k = 12
attribute_labels <- as.vector(names(ckd.data))
imputable_fields <- attribute_labels[1:24]
imputed_data  <- kNN(ckd.data, k = , variable = imputable_fields, numFun = weighted.mean)
ckd.data.kNN <- subset(imputed_data, select = age:class)
write.csv(ckd.data.kNN, file = "original_imputed_data.csv",row.names=FALSE)
#imputation report
png('./images/data_imputation_report.png')
aggr(imputed_data, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
dev.off()

#imputations for missing data k = 24
attribute_labels <- as.vector(names(ckd.data))
imputable_fields <- attribute_labels[1:24]
imputed_data  <- kNN(ckd.data, k = 24, variable = imputable_fields, numFun = weighted.mean)
ckd.data.kNN <- subset(imputed_data, select = age:class)
write.csv(ckd.data.kNN, file = "original_imputed_datak_7.csv",row.names=FALSE)

#imputations for missing data k = 7
attribute_labels <- as.vector(names(ckd.data))
imputable_fields <- attribute_labels[1:24]
imputed_data  <- kNN(ckd.data, k = 7, variable = imputable_fields, numFun = weighted.mean)
ckd.data.kNN <- subset(imputed_data, select = age:class)
write.csv(ckd.data.kNN, file = "original_imputed_datak_7.csv",row.names=FALSE)
