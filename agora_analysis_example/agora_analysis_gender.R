rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")
library(reachR)
library(dplyr)
library(data.table)
source("./hypegrammaR/dependencies.R")
getwd()

###E fails here because the data argument is not carried over to map_to_case function inside load_analysis_plan
  analysisplan <-load_analysis_plan(data, 
                                   analysis.plan.file = "./analysis_framework.csv",
                                   independent.var.column = "independent variable",
                                   dependent.var.column = "dependent variable",
                                   hypothesis.column = "hypothesis type", disaggregation.variable = "Disaggregate")
  
debug(load_analysis_plan)
###E

analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]


results<-apply_data_analysis_plan(data,analysisplan)


result_table<-map_list_of_results_to_dataframe(results)

result_table$indicator <- gsub("VAR\\.[0-9]*\\.\\.\\.","",result_table$indicator)
result_table$indicator <- gsub("\\."," ",result_table$indicator)

split_by_gender<-result_table %>% split.data.frame(result_table$independent.var.value)

comparison_horizontal<-data.frame(split_by_gender$`Male`,"female"=split_by_gender$`Female`$numbers)
comparison_horizontal$absolute.difference<-comparison_horizontal$numbers - comparison_horizontal$female
comparison_horizontal$percent.difference<- comparison_horizontal$absolute.difference / comparison_horizontal$female
comparison_horizontal$percent.difference[grep("Pearson's",comparison_horizontal$test.type)]<-NA


comparison_horizontal$adjusted.p.value<-comparison_horizontal$p.value*nrow(analysisplan)
comparison_horizontal$significant.at.005<-comparison_horizontal$adjusted.p.value<0.05
diff_comb<-comparison_horizontal$percent.difference
diff_comb[is.na(diff_comb)]<-comparison_horizontal$absolute.difference[is.na(diff_comb)]
# comparison_horizontal$difference.over.7.percent <- abs(diff_comb)>0.07
# comparison_horizontal$significant.and.large.difference<-comparison_horizontal$difference.over.7.percent & comparison_horizontal$significant.at.005




comparison_horizontal %>% glimpse
comparison_horizontal %>% write.csv("comparison_raw.csv")

