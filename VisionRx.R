###########################################################################
# Blue Diesel Data Science
# Purpose: Program to reformat csv files from various sources so 
# they can be more easily loaded into mySQL or SQL
# Author: Greg Foss
# Start Date: 7/21/2015
# Version: 6.0.0
###########################################################################
#---------------LIBRARIES-------------------------------------------------#
library(stringr)
#library(dplyr)
library(plyr)
library(rlist)
# library(ff)
# library(ffbase)
library(splitstackshape)
#library(myutils)
library(fuzzyjoin)
library(tidyr)
library(sqldf)
###########################################################################
#--------------CONSTANTS--------------------------------------------------#
in_products <- "D:/PharmaInsight/original/products.txt"
in_patents <- "D:/PharmaInsight/original/patent.txt"
in_fda <- "D:/PharmaInsight/original/product.txt"
in_fdap <- "D:/PharmaInsight/original/package.txt"
in_labeler <- "D:/PharmaInsight/original/ndc-to-labeler.csv"
in_exclusivity <- "D:/PharmaInsight/original/exclusivity.txt"
in_dmf <- "D:/PharmaInsight/original/dmf.csv"
in_nadac <- "D:/PharmaInsight/original/NADAC__National_Average_Drug_Acquisition_Cost_.csv"
out_orange <- "D:/PharmaInsight/formatted/orange.csv"
out_fda <- "D:/PharmaInsight/formatted/fda.csv" 
out_dmf <- "D:/PharmaInsight/formatted/dmf.csv"
out_pcl <- "D:/PharmaInsight/formatted/pcl.csv"
out_ct <- "D:/PharmaInsight/formatted/ct.csv"
out_nadac <- "D:/PharmaInsight/formatted/nadac.csv"
out_nadac_c <- "D:/PharmaInsight/formatted/nadac_c.csv"
working_dir <- "D:/PharmaInsight/original/"
###########################################################################

#---------------------Start-of-Selection for Orange Book------------------#
#for Products
products <- read.csv(in_products, sep = "~", header = TRUE, 
                     colClasses=c("factor", "factor", "factor", "factor", "factor", "factor", "factor"))
products$applkey <- paste0(products$Appl_Type, products$Appl_No, products$Product_No)
products <- products[!duplicated(products$applkey),]
products <- products[c(15, 1:14)]
products <- plyr::rename(products, c('DF.Route' = 'Drug_Route', 
                               'TE_Code' = 'Therapeutic_Equivalence', 
                               'RLD' = 'Reference_Listed_Drug', 
                               'RS' = 'Reference_Standard'))

#for Patents
patents <- read.csv(in_patents, sep = "~", header = TRUE, 
                    colClasses=c("factor", "factor", "factor", "factor", "factor", "factor", "factor"))
patents$applkey <- paste0(patents$Appl_Type, patents$Appl_No, patents$Product_No)
patents <- patents[!duplicated(patents$applkey), ]
patents$Patent_Expire_Date_Text <- as.Date(patents$Patent_Expire_Date_Text, '%b %d, %Y') 
patents <- plyr::rename(patents, c('Patent_Expire_Date_Text' = 'Patent_Expiration_Date'))
patents <- patents[c(10, 1:9)]

#for Exclusivity
exclusivity <- read.csv(in_exclusivity, sep = "~", header = TRUE, 
                        colClasses=c("factor", "factor", "factor", "factor", "factor"))
exclusivity$applkey <- paste0(exclusivity$Appl_Type, exclusivity$Appl_No, exclusivity$Product_No)
exclusivity <- exclusivity[!duplicated(exclusivity$applkey), ]
exclusivity$Exclusivity_Date <- as.Date(exclusivity$Exclusivity_Date, '%b %d, %Y') 
exclusivity <- exclusivity[c(6, 1:5)]

prod_pat <- merge(products, patents, all.x = TRUE)
orange <- merge(prod_pat, exclusivity, all.x=TRUE)  

orange$Approval_Date <- gsub('Approved Prior to ', '', orange$Approval_Date)
#orange$Approval_Date <- as.Date(orange$Approval_Date, '')
library(lubridate)
orange$Approval_Date <- mdy(orange$Approval_Date)
#as.Date(orange$Approval_Date, '%B %d,%Y')
#orange$Approval_Date <- as.Date(orange$Approval_Date, '%d-%b-%y')

write.csv(orange, out_orange, row.names = FALSE, fileEncoding = "UTF-8")
rm(exclusivity, patents, products, prod_pat)
#---------------------End-of-Selection for Orange Book--------------------#
#-----------------------Start of Selection for FDA------------------------#
  fda <- read.csv(in_fda, header = TRUE, sep="\t", stringsAsFactors = FALSE)
  labeler <- read.csv(in_labeler, sep = ",", header = TRUE, 
                      colClasses=c("factor", "factor"))
  names(fda) <- tolower(names(fda))
  fda <- plyr::rename(fda, c("productndc" = "ndc9"))
  #bring in the package and merge before dropping productid
  fdap <- read.csv(in_fdap, header = TRUE, sep="\t")
  names(fdap) <- tolower(names(fdap))
  fda <- merge(fda, fdap)
  
  fda$productid <- NULL
  fda$productndc <- NULL
  
  fda$applicationnumber <- str_replace_all(fda$applicationnumber, "[a-zA-Z]", "")
  
  #The labeler code is the first '-' 
  fda$ndc9 <- str_pad(fda$ndc9, 9, pad = "0")  #REVIEW THIS ENTRY
  fda <- cSplit(fda, "ndc9", "-", type.convert = FALSE, drop = FALSE)
  fda$ndc9_2 <- NULL
  fda <- plyr::rename(fda, c("ndc9_1" = "labelercode"))
  labeler <- plyr::rename(labeler, c("Firm.Name" = "firmname", "NDC.Labeler.Code" = "labelercode"))
  
  fda<- merge(fda, labeler, by.x = "labelercode", by.y = "labelercode", all.x = TRUE)
  
  #replace blank substance names with nonproprietary name and then proprietaryname
  fda$substancename <- ifelse(fda$substancename == "", fda$nonproprietaryname, fda$substancename)
  fda$substancename <- ifelse(fda$substancename == "", fda$proprietaryname, fda$substancename)
  
  fda <- as.data.frame(fda)
  fda <- fda[c(1:13, 21, 14:20)]
  
  fda <- plyr::rename(fda, c("ndcpackagecode" = "ndc11"))
  fda <- fda[c(1, 2, 20, 3:19, 21)]
  fda <- fda[c(1:3, 21, 4:20)]
  
  fda <- cSplit(fda, "ndc11", "-", drop = FALSE, type.convert = FALSE)
  fda$ndc11_1 <- str_pad(fda$ndc11_1, 5, pad = "0")
  fda$ndc11_2 <- str_pad(fda$ndc11_2, 4, pad = "0")
  fda$ndc11_3 <- str_pad(fda$ndc11_3, 2, pad = "0")
  fda$ndc11 <- paste0(fda$ndc11_1, fda$ndc11_2, fda$ndc11_3)
  fda$ndc11_1 <- NULL
  fda$ndc11_2 <- NULL
  fda$ndc11_3 <- NULL
  
  fda$ndc9 <- ifelse(regexpr('\\d{5}-\\d{3}$', fda$ndc9) == TRUE,
                    sub('-', '0', fda$ndc9), sub('-', '', fda$ndc9))
  
  stringi::stri_replace_all_fixed(
    fda$labelername, 
    c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
    c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
    vectorize_all = FALSE
  )
  
  fda <- cSplit(fda, "dosageformname", ",")
  
  fda <- fda[!duplicated(fda$ndc11), ]
  
  #create an applkey for orange book.
  fda$applkey <- paste0(substr(fda$marketingcategoryname, 1, 1), fda$applicationnumber,
                  sprintf("%03.0f", as.numeric(ave(fda$applicationnumber, fda$applicationnumber, FUN=seq_along))))

fda_orange <- merge(fda, orange, by = "applkey", all.x = TRUE, all.y = TRUE)

require(dplyr)
fo <- fda_orange %>%
  filter(is.na(substancename))
fo1 <- fda_orange %>%
  filter(!is.na(substancename))
fo1$floatindicator <- 0


fo$substancename <- fo$Ingredient
fo$routename <- fo$Drug_Route
fo$firmname <- fo$Applicant_Full_Name
fo$applicationnumber <- fo$Appl_No
fo$floatindicator <- 1

fo$ndc11 <- paste0('NIFDA', sample(1e11, size = nrow(fo), replace = TRUE))

fo2 <- rbind(fo1, fo)
fo2 <- fo2[c(2:14, 1, 15:48)]

drops = c('Appl_Type', 'Appl_No', 'Product_No', 'Ingredient', 'Drug_Route', 'Trade_Name', 
          'Applicant', 'Strength', 'Therapeutic_Equivalence', 'Approval_Date', 
          'Reference_Listed_Drug', 'Reference_Standard', 'Type', 'Applicant_Full_Name', 
          'Patent_No', 'Patent_Expiration_Date', 'Drug_Substance_Flag', 'Drug_Product_Flag', 
          'Patent_Use_Code', 'Delist_Flag', 'Exclusivity_Code', 'Exclusivity_Date')

`%ni%` <- Negate(`%in%`)
fo3 <- subset(fo2,select = names(fo2) %ni% drops)

# on " causing problems
fo3$firmname <- gsub('"', '', fo3$firmname)

#trim active ingredient unit 
fo3$active_ingred_unit <- strtrim(fo3$active_ingred_unit, 900)
fo3$substancename <- strtrim(fo3$substancename, 900)
fo3$pharm_classes <- strtrim(fo3$pharm_classes, 900)
fo3$marketingcategoryname <- strtrim(fo3$marketingcategoryname, 50)

fo3$firmname <- gsub("[^[:alnum:]///' ]", " ", fo3$firmname)
fo3$firmname <- toupper(fo3$firmname)
fo3$firmname <- gsub("\\bLLC\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bINC\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bLTD\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bLIMITED\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bGMBH\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bPHARM\\b", "PHARMACEUTICALS", fo3$firmname)
fo3$firmname <- gsub("\\bPHARMA\\b", "PHARMACEUTICALS", fo3$firmname)
fo3$firmname <- gsub("\\bPHARMACEUTICAL\\b", "PHARMACEUTICALS", fo3$firmname)
fo3$firmname <- gsub("\\bPHARM\\b", "PHARMACEUTICALS", fo3$firmname)
fo3$firmname <- gsub("\\bCORP\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bCORPORATION\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bLABORATORY\\b", "LABORATORIES", fo3$firmname)
fo3$firmname <- gsub("\\bSPA\\b", "", fo3$firmname)
fo3$firmname <- gsub("\\bINCORPORATED\\b", "", fo3$firmname)
fo3$firmname <- gsub("^\\s+|\\s+$", "", fo3$firmname)

#|||||||Attempt to clean up the substance field in FDA. 11/18/2017||||||||||||
fo3$substancename <- toupper(fo3$substancename)
fo3$substancename <- str_replace_all(fo3$substancename, "[^[:alnum:]///' ]", "")
fo3$substancename <- gsub("TABLETS", "", fo3$substancename)
fo3$substancename <- gsub("TABLET", "", fo3$substancename)
fo3$substancename <- gsub("CAPSULES", "", fo3$substancename)
fo3$substancename <- gsub("CAPSULE", "", fo3$substancename)
fo3$substancename <- gsub("\\bHCL\\b", "", fo3$substancename)
fo3$substancename <- gsub("\\bHYDROCHLORIDE\\b", "", fo3$substancename)
fo3$substancename <- gsub("\\bFORMULATION\\b", "", fo3$substancename)
fo3$substancename <- gsub("\\bGENERIC\\b", "", fo3$substancename)
fo3$substancename <- gsub("\\bNOS\\b", "", fo3$substancename)
fo3$substancename <- trimws(fo3$substancename, which = c("both", "left", "right"))
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

write.table(fo3, out_fda, row.names = FALSE, sep=",", fileEncoding = "UTF-8")
rm(fda_orange, fdap, fo, fo1, fo2)
#-----------------------End of Selection for FDA--------------------------#
#---------------------Start-of-Selection for Pharma Classes---------------#
select.me <- c('ndc11','pharm_classes')
fda_df <- as.data.frame(fda)
pc <- fda_df[, select.me]
rm(fda_df)

pc$pharm_classes <- as.character(pc$pharm_classes)
pc_classes <- data.frame(str_split_fixed(pc$pharm_classes, ",", n=51), stringsAsFactors=F)
pc_comb <- cbind(pc, pc_classes)
pc_comb$pharm_classes <- NULL

p_moa <- "\\[MoA]"
p_epc <- "\\[EPC]"
p_che <- "\\[Chemical/Ingredient]"
p_pe <- "\\[PE]"

pc_comb_reshape <- pc_comb

id_variables <- c("ndc11")
library(reshape2)
pc_comb_reshape <- melt(pc_comb_reshape, 
                        id.vars = id_variables, 
                        variable.name = "column", 
                        value.name = "value")

pc_comb_reshape$column <- NULL

pc_comb_reshape <- pc_comb_reshape[!(is.na(pc_comb_reshape$value) | pc_comb_reshape$value==""), ]

tmp_list <- list()
pc_list <- list() 
rm(pc_comb)

for (i in 1:nrow(pc_comb_reshape)){
  tmp_list <- list()
  if (grepl(p_moa, pc_comb_reshape$value[i])){
    tmp_list <- list(pc_comb_reshape$ndc11[i], "Mechanism Of Action", pc_comb_reshape$value[i])
    pc_list <- c(pc_list, list(tmp_list))
  } else if (grepl(p_epc, pc_comb_reshape$value[i])){
    tmp_list <- list(pc_comb_reshape$ndc11[i], "Established Pharmacologic Class", pc_comb_reshape$value[i])
    pc_list <- c(pc_list, list(tmp_list))
  } else if (grepl(p_che, pc_comb_reshape$value[i])){
    tmp_list <- list(pc_comb_reshape$ndc11[i], "Chemical/Ingredient", pc_comb_reshape$value[i])
    pc_list <- c(pc_list, list(tmp_list))
  } else if (grepl(p_pe, pc_comb_reshape$value[i])){
    tmp_list <- list(pc_comb_reshape$ndc11[i], "Physiologic Effect", pc_comb_reshape$value[i])
    pc_list <- c(pc_list, list(tmp_list))
  }
}

for (i in 1:length(pc_list)){
  pc_list[[i]][[3]] <- gsub(p_che, replacement = "", pc_list[[i]][[3]])
  pc_list[[i]][[3]] <- gsub(p_epc, replacement = "", pc_list[[i]][[3]])
  pc_list[[i]][[3]] <- gsub(p_moa, replacement = "", pc_list[[i]][[3]])
  pc_list[[i]][[3]] <- gsub(p_pe, replacement = "", pc_list[[i]][[3]])
}

p_mt <- do.call(rbind, pc_list)
colnames(p_mt) <- c('ndc11', 'factor', 'substancename')

write.csv(p_mt, out_pcl, row.names = FALSE)
rm(p_mt, pc_comb_reshape, pc_list, pc_classes, pc)

p_mt <- read.csv(out_pcl, stringsAsFactors = FALSE)
p_mt$ndc9 <- str_sub(p_mt$ndc11, 0, 9)
p_mt <- subset(p_mt, grepl('^\\d+$', p_mt$ndc11))
p_mt$substancename <- tolower(p_mt$substancename)
p_mt <- unique(p_mt)
write.csv(p_mt, out_pcl, row.names = FALSE)

#---------------------End-of-Selection for Pharma Classes------------------#
#---------------------Start-of-Selection for Substance Names---------------#
subst1 <- cSplit(fda, "substancename", ";", "long")

drops = c('producttypename', 'proprietaryname', 'proprietarynamesuffix', 'nonproprietaryname', 
          'dosageformname_1', 'dosageformname_2', 'dosageformname_3', 'dosageformname_4', 
          'routename', 'startmarketingdate', 'endmarketingdate', 'marketingcategoryname', 
          'labelername', 'pharm_classes', 'deaschedule', 'applicationnumber', 'active_numerator_strength', 
          'active_ingred_unit', 'packagedescription', 'firmname', 'ndc9')

`%ni%` <- Negate(`%in%`)
subst1 <- subset(subst1,select = names(subst1) %ni% drops)

subst2 <- as.data.frame(gsub("[^[:alnum:]///' ]", "", subst1$substancename))
names(subst2) <- c("substancename")
subst2 <- cbind(subst1, subst2)
subst2[,3] <- NULL
rm(subst1)
#---------------------End-of-Selection for Substance Names------------------#
#---------------------Start-of-Selection for Drug Master File---------------#
dmf <- read.csv(in_dmf, stringsAsFactors = FALSE)
substance_key <- as.data.frame(unique(subst2$substancename))
names(substance_key) <- 'substancename'
#substance_key$substancename <- as.character(substance_key$substancename)

#!!!!!!!!!!!!For some reason this previously working code just stopped perhaps memory problems. 
library(stringr)
library(fuzzyjoin)
dmf$SUBJECT <- gsub("[^[:alnum:] ]", "", dmf$SUBJECT)

###Temporarily write this out and see if running it in R not R Studio would make a difference
write.csv(dmf, 'd:/pharmainsight/formatted/dmf_temp.csv', row.names = FALSE, fileEncoding = "UTF-8")
write.csv(substance_key, 'd:/pharmainsight/formatted/sub_key_temp.csv', row.names = FALSE, fileEncoding = "UTF-8")
dmf <- read.csv("d:/pharmainsight/formatted/dmf_temp.csv", stringsAsFactors = FALSE)
substance_key <- read.csv("d:/pharmainsight/formatted/sub_key_temp.csv", stringsAsFactors = FALSE)
substance_key$substancename <- gsub("[^[:alnum:] ]", "", substance_key$substancename)

################   
f <- function(x, y) {
  # tests whether y is an ingredient of x
  str_detect(x, regex(paste0("\\b", y, "\\b"), ignore_case = TRUE))
}

new_dmf1 <- fuzzy_join(dmf,
                       substance_key,
                       by = c("SUBJECT" = "substancename"),
                       match_fun = f)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

new_dmf1$SUBMIT.DATE <- as.Date(new_dmf1$SUBMIT.DATE, '%m/%d/%Y')

subst_dmf <- merge(new_dmf1, subst2, by.x = 'substancename', by.y = 'substancename', all.x = TRUE, all.y = TRUE)
sub_dmf <- subst_dmf[which(subst_dmf$substancename != ''), ] 
sub_dmf <- sub_dmf[c(2:7, 1, 8:10)]
sub_dmf$ndc9 <- str_sub(sub_dmf$ndc11, 0,9)

sub_dmf$labelercode <- NULL
sub_dmf$applkey <- NULL

sub_dmf <- sub_dmf[!duplicated(sub_dmf[c(1,7,8)]),]
sub_dmf <- plyr::rename(sub_dmf, c("DMF." = "DMF_No"))
sub_dmf <- sub_dmf[!is.na(sub_dmf$DMF_No), ]

#write.csv(sub_dmf, out_dmf, sep="||^||", row.names = FALSE)
write.table(sub_dmf, out_dmf, row.names = FALSE, sep="||^||", fileEncoding = "UTF-8")
#---------------------End-of-Selection for Drug Master File-----------------#

#---------------------Start-of-Selection for NADAC----------------#
setwd(working_dir)
files <- Sys.glob("NADAC_as_of_*.csv")
nadac <- do.call(rbind,lapply(files,read.csv))
#nadac <- read.csv(in_nadac, stringsAsFactors = FALSE)
nadac$NDC <- str_pad(nadac$NDC, 11, pad = "0")
nadac$ndc9 <- str_sub(nadac$NDC, 0,9)
nadac <- nadac[c(1:2, 13, 3:12)]
nadac <- nadac[!duplicated(nadac[c(2,13)]),]
write.csv(nadac, out_nadac, row.names = FALSE, fileEncoding = "UTF-8")
#---------------------End-of-Selection for NADAC----------------#
#---------------------Start-of-Selection for NADAC Comparison----------------#
setwd(working_dir)
files <- Sys.glob("NADAC_Comparison*.csv")
nadac_c <- do.call(rbind,lapply(files,read.csv))

nadac_c$NDC <- str_pad(nadac_c$NDC, 11, pad = "0")
nadac_c$ndc9 <- str_sub(nadac_c$NDC, 0,9)

nadac_c <- plyr::rename(nadac_c, c("NDC.Description" = "NDC_Description", 
                             "NDC" = "NDC11", 
                             "ndc9" = "NDC9", 
                             "Old.NADAC.Per.Unit" = "Old_NADAC_Per_Unit", 
                             "New.NADAC.Per.Unit" = "New_NADAC_Per_Unit", 
                             "Classification.for.Rate.Setting" = "Classification", 
                             "Percent.Change" = "Percent_Change", 
                             "Primary.Reason" = "Primary_Reason", 
                             "Start.Date" = "Start_Date", 
                             "End.Date" = "End_Date", 
                             "Effective.Date" = "Effective_Date"))

nadac_c <- nadac_c[!duplicated(nadac_c[c(2,9)]),]
write.csv(nadac_c, out_nadac_c, row.names = FALSE, fileEncoding = "UTF-8")
#---------------------End-of-Selection for NADAC Comparison----------------#


#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#
#---------------------Start-of-Selection for Clinical Trials----------------#
library(feedeR)

url_start <- "https://clinicaltrials.gov/ct2/results/rss.xml?rcv_d=100&lup_d=&sel_rss=new14&term="
url_end <- "&count=1000"

# ct_list <- list()
# 
# tl <- do.call(c, unlist(ct_list, recursive=FALSE))
# tl <- c(tl, substance = substance_key$substancename[i])
# ct_list <- c(ct_list, list(tl))

ct_df <- data.frame()
for (i in 1:nrow(substance_key)){
  url = paste0(url_start, substance_key$substancename[i], url_end)
  url <- gsub(url, pattern =' ', replacement = '%20' )
  print(url)
  t_df <- NULL
  try(
    t_df <- as.data.frame(feed.extract(url)), silent = TRUE)
  if(is.data.frame(t_df) && nrow(t_df)>=1){
    t_df$substance = substance_key$substancename[i]
    print(t_df$substance[i])
  }
  ct_df <- rbind(ct_df, t_df)
}

ct_df$title <- NULL
ct_df$link <- NULL
ct_df$updated <- NULL
ct_df$items.hash <- NULL
ct_df <- rename(ct_df, c("items.title" = "title", 
                         "items.date" = "date", 
                         "items.link" = "link"))

ct_df <- ct_df[!duplicated(ct_df$link),]
write.csv(ct_df, out_ct, row.names = FALSE, fileEncoding = "UTF-8")
#---------------------End-of-Selection for Clinical Trials------------------#

#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#

