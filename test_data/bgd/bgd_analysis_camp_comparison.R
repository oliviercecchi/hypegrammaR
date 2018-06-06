rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")

data<-load_assessment_bgd()

camps <- select_mulitpleify(data$VAR.11...enter.the.survey.site.,keep.values=TRUE)
data <- cbind(data, camps)
# data.plan.per.camp <- many_plans(unique(data$VAR.11...enter.the.survey.site.))
# data.plan.per.camp <- data.plan.per.camp[c(1,2)]

analysisplan<-map_to_analysis_plan_all_vars_as_dependent(each.x.as.independent.in.var = "VAR.11...enter.the.survey.site.",data = data)

results <- apply_data_analysis_plan(data,analysisplan)
result_table <- map_list_of_results_to_dataframe(results)
is.other<-result_table$independent.var.value=="other"

split_by_isother <- result_table %>% split.data.frame(is.other)

comparison_horizontal<-data.frame(split_by_isother$`FALSE`,"other"=split_by_isother$`TRUE`$numbers)
comparison_horizontal$absolute.difference<-comparison_horizontal$numbers - comparison_horizontal$other
comparison_horizontal$percent.difference<- comparison_horizontal$absolute.difference / comparison_horizontal$other
comparison_horizontal$percent.difference[grep("Pearson's",comparison_horizontal$test.type)]<-NA


# comparison_horizontal %>% write.csv("camp_comparison.csv")
comparison_horizontal$adjusted.p.value<-comparison_horizontal$p.value*nrow(analysisplan)
comparison_horizontal$significant.at.005<-comparison_horizontal$adjusted.p.value<0.05
diff_comb<-comparison_horizontal$percent.difference
diff_comb[is.na(diff_comb)]<-comparison_horizontal$absolute.difference[is.na(diff_comb)]
comparison_horizontal$difference.over.7.percent <- abs(diff_comb)>0.07
comparison_horizontal$significant.and.large.difference<-comparison_horizontal$difference.over.7.percent & comparison_horizontal$significant.at.005

comparison_horizontal %>% write.csv("camp_comparison2.csv")

