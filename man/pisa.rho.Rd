\name{pisa.rho}
\alias{pisa.rho}
\alias{pisa2015.rho}
\title{
Correlation matrix
}
\description{
pisa.rho produces a correlation matrix for observed variables (NOT for plausible values)
Use the pisa2015.rho() for data from PISA 2015 study.
}
\usage{
pisa.rho(variables, by, data, export=FALSE, name= "output", folder=getwd())
pisa2015.rho(variables, by, data, export=FALSE, name= "output", folder=getwd())
}

\arguments{
  \item{variables}{
Data labels for the variables in the correlation matrix (e.g., variables=c("TCHBEHTD", "TCHBEHSO"))
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any 
other categorical variable. 	
}
 \item{data}{
An R object, normally a data frame, containing the data from PISA.
}
  \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) 
that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
pisa.rho returns a matrix including correlation and standard error values.
}

\seealso{
timss.rho, pirls.rho, pirls.rho.pv, timss.rho.pv
}
\examples{
\dontrun{
pisa.rho(variables=c("COGACT", "TCHBEHTD", "TCHBEHSO", "CLSMAN" ), by="IDCNTRYL", data=pisa)
pisa2015.rho(variables=c("AGE", "PV1MATH"), by="CNT", data=stud2015)
}
}
