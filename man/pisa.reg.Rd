\name{pisa.reg}
\alias{pisa.reg}
\title{Regression analysis}
\description{
pisa.reg performs linear regression analysis (OLS) for an observed depedent variable 
(NOT for plausible values)
}
\usage{
pisa.reg(y, x, by, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{y}{
Label for dependent variable.
}
  \item{x}{
Data labels of independent variables.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be 
any other categorical variable. 	
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
pisa.reg prints a data.frame with regression results (i.e., coefficients, standard errors, 
t-values, R-squared) and stores  different regression output including residuals and replicate 
coefficients in a list object of class "intsvy.reg". 
}


\seealso{
pirls.reg, timss.reg, piaac.reg
}
\examples{
\dontrun{
pisa.reg(y="BELONG", x="ST04Q01", by="IDCNTRYL", data=pisa)
}}
