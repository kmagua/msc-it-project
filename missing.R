png(filename="images/rbc.png")
hist(ckd$rbc)
dev.off()
library(naniar)
n_miss(ckd) #1012
n_complete(ckd)

miss_var_summary(ckd)#no of missing per variable
miss_case_summary(ckd)
str(no_missing)
