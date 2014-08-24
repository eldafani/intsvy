\name{pisa.reg}
\alias{pisa.reg}
\title{Regression analysis}
\description{
pisa.reg performs linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values)
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
pisa.reg returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list.
If the "by" argument is set, then the returning object is of the class "intsvy.reg" with overloaded function plot().
}


\seealso{
pirls.reg, timss.reg, piaac.reg
}
\examples{
\dontrun{
# Table III.2.3d, p. 252 International Report 2012
pisa.reg(y="BELONG", x="ST04Q01", by="IDCNTRYL", data=pisa)
}}
