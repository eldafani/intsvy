\name{pirls.table}
\alias{pirls.table}
\alias{pirls2016.table}
\title{
Frequency table
}
\description{
pirls.table produces a frequency table for a categorical variable printing percentages and standard errors. Information about weight is extracted from \code{intsvy:::pirls_conf}.
}
\usage{
pirls.table(variable, by, data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{variable}{
The data label with the variable to be analysed.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
}
   \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
pirls.table returns a data frame with percentages and standard errors.
}

\seealso{
timss.table, pisa.table
}
\examples{
\dontrun{
# PIRLS: Exhibit 2.19 User Guide 2011, p. 30
pirls.table(variable="ASDHELA", by="IDCNTRYL", data=pirls)
}}
