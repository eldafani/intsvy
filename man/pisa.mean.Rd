\name{pisa.mean}
\alias{pisa.mean}
\alias{pisa2015.mean}
\title{
Calculates mean of variable
}
\description{
Calculates the mean of an observed variable (NOT one with plausible values) and its standard error.
Use the pisa2015.mean() for data from PISA 2015 study.
}
\usage{
pisa.mean(variable, by, data, export = FALSE, 
name = "output", folder = getwd())
pisa2015.mean(variable, by, data, export = FALSE, 
name = "output", folder = getwd())
}

\arguments{
  \item{variable}{
The label corresponding to the observed variable, for example, ""ESCS"", for the student SES.
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
pisa.mean returns a data frame with means and standard errors.
}

\seealso{
timss.mean, pirls.mean, piaac.mean
}
\examples{
\dontrun{
# PISA 2012: Table II.2.3, p. 183
pisa.mean(variable="ESCS", by="IDCNTRYL", data=pisa)
pisa.mean(variable="PARED", by="IDCNTRYL", data=pisa)

# PISA 2012: Table III.2.3d, p. 252
pisa.mean(variable="BELONG", by="IDCNTRYL", data=pisa)
pisa.mean(variable="BELONG", by=c("IDCNTRYL", "ST04Q01"), data=pisa)

# PISA 2015
pisa2015.mean(variable="AGE", by="CNT", data=stud2015)
}
}
