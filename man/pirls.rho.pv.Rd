\name{pirls.rho.pv}
\alias{pirls.rho.pv}
\title{Two-way weighted correlation with plausible values
}
\description{
pirls.rho.pv calculates the correlation and standard error among two achievement variables each based on 5 plausible values or one achievement variable and an observed variable (i.e., with observed scores rather than plausible values).
}
\usage{
pirls.rho.pv(variable, pvlabel, by, 
data, export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{variable}{
A data label for the observed variable (e.g., variable="ASDAGE") 
}
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.}
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
pirls.rho.pv returns a matrix with correlations and standard errors.
}

\seealso{
timss.rho.pv, pirls.rho, timss.rho
}
\examples{
\dontrun{
pirls.rho.pv(variable="BSDGEDUP", pvlabel=paste0("BSMMAT0", 1:5), by="IDCNTRYL", data=timss)
}
}
