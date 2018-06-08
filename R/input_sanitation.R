change_Excel_REF_to_NA <- function(data){
  data[data == "#REF!"] <- NA
  return(data)
}



sanitise_group_difference<-function(data,dependent.var,independent.var){


  independent_less_than_30 <- length(unique(data[[independent.var]])) <= 30

  if(!independent_less_than_30){
  return(list(success=FALSE,message="can not test group difference with 30 or more unique values in the independent variable"))
  }

  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    return(list(success=FALSE,message="can not test group difference with <2 different values in the dependent variable"))
  }


  which_independent_more_than_one_record <- table(data[[independent.var]])
  which_independent_more_than_one_record <- which_independent_more_than_one_record[which(which_independent_more_than_one_record>1)]
  which_independent_more_than_one_record <- names(which_independent_more_than_one_record)
  data <- data[data[[independent.var]] %in% which_independent_more_than_one_record,]


  at_least_two_independent_groups <- (data[[independent.var]] %>% unique %>% length) > 1


  if(!at_least_two_independent_groups){
    return(list(success=FALSE,message="can not test group difference with <2 unique values in the independent variable with at least 2 records)"))
  }

  return(list(success=TRUE,data=data))
}


###E counts numerical variables as categorical :( (nb.rooms)). How do I get data to read in as an agrument
load_analysis_plan <- function(data, 
                               analysis.plan.file, 
                               independent.var.column, 
                               dependent.var.column, 
                               hypothesis.column, 
                               disaggregation.variable){
insure.is.single.value(analysis.plan.file)
insure.is.single.value(independent.var.column)
insure.is.single.value(dependent.var.column)
insure.is.single.value(hypothesis.column)

ap_raw <- reachR:::read.csv.auto.sep(analysis.plan.file, stringsAsFactors = F, 
                            header = T)
dependent.var.column <- reachR:::to_alphanumeric_lowercase(dependent.var.column)
independent.var.column <- reachR:::to_alphanumeric_lowercase(independent.var.column)
hypothesis.column <- reachR:::to_alphanumeric_lowercase(hypothesis.column)
case_vector <- mapply(map_to_case, ap_raw[[hypothesis.column]], ap_raw[[dependent.var.column]], ap_raw[[independent.var.column]]) %>% unlist
ap_case <- data.frame(ap_raw, case_vector, stringsAsFactors = F) 
return(ap_case)
}



sanitise_data<-function(data,
                        dependent.var,
                        independent.var,
                        case){


  dep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  indep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  if(length(dep_var_name_in_data_headers)==0){
    stop("dependent.var not found in data")
  }
  if(length(indep_var_name_in_data_headers)==0){
    stop("independent.var not found in data")
  }


  if(length(dep_var_name_in_data_headers)>1){
    stop(paste("more than 1 data column name matching dependent.var (may be too similar):",
               dep_var_name_in_data_headers,collapse="\n"))
  }
  if(length(indep_var_name_in_data_headers)>1){
    stop(paste("more than 1 data column name matching independent.var (may be too similar):",
               dep_var_name_in_data_headers,collapse="\n"))  }




  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="not a data frame or data frame without data"))}

  # remove records with NA in dependent or independent
  data<-data[!is.na(data[[dependent.var]]) & !is.na(data[[independent.var]]),]
  data<-data[(data[[dependent.var]]!="") & (data[[independent.var]]!=""),]


  # still have data?
  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="no data (after removing records with NA in dependent or independent variable)"))}


  if((grep("group_difference",case) %>% length)>0){
    group_difference<-sanitise_group_difference(data,
                                                dependent.var = dependent.var,
                                                independent.var = independent.var)
    if(group_difference$success==F){return(group_difference)}

  }

  if(case=="CASE_group_difference_categorical_categorical"){
    if(length(unique(data[[dependent.var]]))>50){
      return(list(success=F,message="can not perform chisquared test on >50 unique values in the dependent variable."))

    }
  }

return(list(success=T,data=data))


}



















sanitise_is_good_dataframe<-function(data){

  if(!is.data.frame(data)){return(FALSE)}
  if(ncol(data)<1){return(FALSE)}
  if(nrow(data)<1){return(FALSE)}
  if(as.vector(data) %>% is.na %>% all){return(FALSE)}
  return(TRUE)

  }


















