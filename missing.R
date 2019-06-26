setwd('Project')
#used to visualize missing data
library(visdat)
library(ggplot2)
library(dplyr)
library(naniar)
#used for data imputation
library(VIM)

ckd.data <-read.csv('ckd_data_converted.csv')
#create a plot to visualize the whole dataset
png('./images/data_visual_initial.png')
vis_dat(ckd.data)
dev.off()
#create a plot to visualize missing variable values sorted from the greatest
png('./images/data_visual_missing_variable_percentage.png')
vis_miss(ckd.data, sort_miss = TRUE)
dev.off()
#number of missing values in each case
png('./images/data_missing_values_per_record.png')
gg_miss_case(airquality)
dev.off()

#imputations for missing data
attribute_labels <- as.vector(names(ckd.data))
imputable_fields <- attribute_labels[1:24]
imputed_data  <- kNN(ckd.data, k = 12, variable = imputable_fields)
ckd.data.kNN <- subset(imputed_data, select = age:class)
  write.csv(ckd.data.kNN, file = "original_imputed_data.csv",row.names=FALSE)
#imputation report
png('./images/data_imputation_report.png')
aggr(imputed_data, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
dev.off()
