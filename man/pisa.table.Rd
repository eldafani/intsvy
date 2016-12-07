\name{pisa.table}
\alias{pisa.table}
\alias{pisa2015.table}
\title{
Frequency table
}
\description{
pisa.table produces a frequency table for a categorical variable printing percentages and standard errors.
Use the pisa2015.table() for data from PISA 2015 study.
}
\usage{
pisa.table(variable, by, data, export = FALSE, name = "output", folder = getwd())
pisa2015.table(variable, by, data, export = FALSE, name = "output", folder = getwd())
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
pisa.table returns a data frame with percentages and standard errors.
}

\seealso{
timss.table, pirls.table
}
\examples{
\dontrun{
# Table A2.4a, p.274 International Report 2012 Volume I
pisa.table(variable="ST01Q01", by="IDCNTRYL", data=pisa)
# Table III.2.1a, p. 232, International Report 2012 Volume III
pisa.table(variable="ST08Q01", by="IDCNTRYL", data=pisa)
# PISA 2015
pisa2015.table(variable="ST004D01T", by="CNT",data=stud2015)
}
}
