\name{timss.rho}
\alias{timss.rho}
\title{
Correlation matrix
}
\description{
timss.rho produces a correlations matrix for observed variables (NOT for plausible values)
}
\usage{
timss.rho(variables, by, data, 
export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{variables}{
Data labels for the variables in the correlation matrix.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
 \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
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
timss.rho returns a matrix including correlation and standard error values.
}

\seealso{
pirls.rho, pirls.rho.pv, timss.rho.pv
}
\examples{
\dontrun{
timss.rho(variables=c("BSMMAT01", "BSDGEDUP"), data=timss)
}
}
