% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_to_labeled.R
\name{labels_summary_statistic}
\alias{labels_summary_statistic}
\title{Add labels to results}
\usage{
labels_summary_statistic(summary.statistic, questionnaire,
  label.dependent.var.value = T, label.independent.var.value = T,
  label.dependent.var = F, label.independent.var = F,
  independent.linebreak = T, dependent.linebreak = F)
}
\arguments{
\item{questionnaire}{koboquest `questionnaire` object; output from load_questionnaire()}

\item{result}{hypegrammaR `result` object; output from map_to_result().}
}
\description{
Add labels to results
}
\details{
if the variable wasn't found in the questionnaire, or the choice wasn't found in the corresponding list of choices, the affected values will remain unchanged.
}
