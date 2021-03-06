% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappings.R
\name{map_to_file}
\alias{map_to_file}
\title{Save outputs to files}
\usage{
map_to_file(object, filename, ...)
}
\arguments{
\item{object}{The object you want to save as a file}

\item{filename}{The name of the file that is produced. The extension needs to match the type of object you want to save (csv for tables, jpg/pdf for images)}
}
\description{
Save outputs to files
}
\examples{
# some table:
mytable<-data.frame(a=1:10,b=1:10)
map_to_file(mytable,"mytable.csv")

# some graphic made with ggplot:
mygraphic<-ggplot(mytable,aes(a,b))+geom_point()
map_to_file(mygraphic,"visualisation.jpg")
map_to_file(mygraphic,"visualisation.pdf")
}
