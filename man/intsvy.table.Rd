\name{intsvy.table}
\alias{intsvy.table}
\title{
Frequency table
}
\description{
intsvy.table produces a frequency table for a categorical variable printing percentages and standard errors.
}
\usage{
intsvy.table(variable, by, data, config)
}

\arguments{
  \item{variable}{
The data label with the variable to be analysed.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.table returns a data frame with percentages and standard errors.
}

\seealso{
timss.table, pirls.table
}
\examples{
\dontrun{
intsvy.table(variable="ASDGSLM", by="IDCNTRYL", data=timss4, 
  config = intsvy:::timss_conf)
intsvy.table(variable="ST08Q01", by="CNT", data=pisa, config=pisa_conf)
}
}
