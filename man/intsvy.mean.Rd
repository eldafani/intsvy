\name{intsvy.mean}
\alias{intsvy.mean}
\title{
Calculates mean of variable 
}
\description{
Calculates mean and standard error of observed variable (NOT one with plausible values).
}
\usage{
intsvy.mean(variable, by, data, export = FALSE, 
name = "output", folder = getwd(), config)
}

\arguments{
  \item{variable}{
The label corresponding to the observed variable, for example, "AGE_R" for age of respondent.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="CNTRYID"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIAAC.
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
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.mean returns a data frame with means and standard errors.
}

\seealso{
pisa.mean, timss.mean, pirls.mean
}
\examples{
\dontrun{
intsvy.mean(variable="READHOME", by="CNTRYID", data=piaac, config=piaac_conf)
intsvy.mean(variable="PARED", by="IDCNTRYL", data=pisa, config=pisa_conf)
intsvy.mean(variable="BSBGSLM", by='IDCNTRYL', data=timss8g, config=timss8_conf)
intsvy.mean(variable='ASBHELA', by= 'IDCNTRYL', data=pirls,config=pirls_conf)
}
}
