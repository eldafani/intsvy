\name{pirls.rho}
\alias{pirls.rho}
\title{
Correlation matrix
}
\description{
pirls.rho produces a correlation matrix for observed variables (NOT for plausible values)
}
\usage{
pirls.rho(variables, by, weight = "TOTWGT", data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{variables}{
Data labels for the variables in the correlation matrix (e.g., variables=c("ASRREA01", "ASDAGE") )
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{weight}{
The total weight, "TOTWGT" in the PIRLS dataset.
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
pirls.rho returns a matrix including correlation and standard error values.
}

\seealso{
timss.rho, pirls.rho.pv, timss.rho.pv
}
\examples{
\dontrun{
pirls.rho(variables=c("ASRREA01", "ASDAGE"), by="IDCNTRYL", data=pirls)
}
}
