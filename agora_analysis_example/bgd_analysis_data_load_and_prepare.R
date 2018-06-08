# DEPENDENCIES
# empty wd
rm(list=ls())
require("dplyr")
require("reachR")
require("data.table")
require("knitr")
require("survey")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
script_path<-"../R"
script_files<-paste(script_path,list.files(script_path),sep="/")

sapply(script_files,source)

source("./bgd_analysis_functions.R")

# LOAD DATA ETC

load_assessment_agora<-function(){

    sf<-load_samplingframe(sampling.frame.file = "./Agora_Kampala_sampling_frame.csv",
                           sampling.frame.population.column = "population",
                           sampling.frame.stratum.column  = "stratum",
                           data.stratum.column = "stratum",
                           return.stratum.populations = T)

    # questionnaire<-load_questionnaire(data = "test_data/bgd/BGD_Cross_camp.csv",
    #                                   questions.file = "test_data/bgd/questionnaire_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
    #                                   choices.file = "test_data/bgd/Choices_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
    #                                   choices.label.column.to.use = "english")



data<- reachR:::read.csv.auto.sep("./Kampala_Agora_clean.csv", stringsAsFactors = F)
sf <- reachR:::read.csv.auto.sep("./Agora_Kampala_sampling_frame.csv")  

data <- change_Excel_REF_to_NA(data)

is.numeric.fuzzy.convert(data$pay.rent.amount)
data$pay.rent.amount %>% table

numeric.data <- lapply(data, is.numeric.fuzzy.convert)
numeric.data <- lapply(data, function(x){if(is.numeric.fuzzy.convert(as.character(x))) {
  x <- gsub(",", ".", x)
  return(suppressWarnings(x %>% as.character %>% as.numeric))}
  else{return(x)}})

data <- numeric.data

#WEIGHTS
weights_ref <- as.numeric(gsub(",", ".", data$weight))
strata_ref <- data_ref$name.slum
library(survey)
design <- svydesign(ids=~0, weights = weights_ref, strata = strata_ref, data = data, na.rm = T)

    # PREP DATA

    data<- data[,-which(colnames(data)==".please.record.the.location..precision")]
    data<-lapply(data,function(x){x[which(x=="")]<-NA;x}) %>% as.data.frame(stringsAsFactors=F)
    colnames(data)<-paste0("VAR.",1:ncol(data),"...",colnames(data))
    data$VAR.11...enter.the.survey.site. <- gsub(" ", ".", data$VAR.11...enter.the.survey.site.)
    return(data)
}









