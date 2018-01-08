library(ff)
library(ffbase)

working_dir <- "D:/PharmaInsight/original/"
out_medicaid <- "D:/PharmaInsight/formatted/medicaid.csv"
#-----------------------Start of Selection for Medicaid-------------------#
setwd(working_dir)
files <- Sys.glob("State_Drug_Utilization_Data_201*.csv")

for (i in 1:length(files)) { 
  print(files[i])
  holder <-  read.csv.ffdf(file=files[i], header=FALSE, skip=1, VERBOSE=TRUE,
                           first.rows=100000, next.rows=200000, colClasses=rep("factor", 20))
  
  if (files[i] == "State_Drug_Utilization_Data_2010.csv") { holder <- moveMe(holder, "V9", "after", "V10")}
  
  if (i == 1) {
    medicaid = holder
  } else { 
    medicaid <- ffdfappend(medicaid, holder[,], adjustvmode=TRUE) 
  }
  
}

medicaid_df <- as.data.frame(medicaid)

name_list <- c('utilization.type',
               'state', 
               'labeler.code', 
               'product.code', 
               'package.size', 
               'year', 
               'quarter', 
               'product.name', 
               'suppression.used', 
               'units.reimbursed', 
               'number.of.prescriptions', 
               'total.amount.reimbursed', 
               'medicaid.amount.reimbursed', 
               'non.medicaid.amount.reimbursed', 
               'quarter.begin', 
               'quarter.begin.date', 
               'latitude', 
               'longitude', 
               'location', 
               'ndc9')

colnames(medicaid_df) <- name_list
medicaid_df$ndc9 <- as.character(medicaid_df$ndc9)
medicaid_df$ndc9 <- substr(medicaid_df$ndc9, 2, nchar(medicaid_df$ndc9))

medicaid_df$ndc11 <- paste0(medicaid_df$labeler.code, 
                            medicaid_df$product.code, 
                            medicaid_df$package.size)
#medicaid_df <- moveMe(medicaid_df, "ndc9", "before", "utilization.type")
#medicaid_df <- moveMe(medicaid_df, "ndc11", "after", "ndc9")

write.csv(medicaid_df, out_medicaid, row.names = FALSE)
#-----------------------End of Selection for Medicaid---------------------#