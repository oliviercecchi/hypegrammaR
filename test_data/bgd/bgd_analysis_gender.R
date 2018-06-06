rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")

data<-load_assessment_bgd()


analysisplan<-data.frame(
  repeat.for="VAR.11...enter.the.survey.site.",
  independent.var="VAR.20...what.is.the.gender.of.the.head.of.the.family.",
  dependent.var=names(data),
  hypothesis.type="group_difference",
  case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
  ,stringsAsFactors = F)

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

