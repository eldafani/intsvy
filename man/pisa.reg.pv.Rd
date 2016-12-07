\name{pisa.reg.pv}
\alias{pisa.reg.pv}
\alias{pisa2015.reg.pv}
\title{Regression analysis with plausible values}
\description{
pisa.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
Use the pisa2015.reg.pv() for data from PISA 2015 study.
}
\usage{
pisa.reg.pv(x, pvlabel = "READ", by, data, 
export = FALSE, name = "output", folder = getwd(), std=FALSE)
pisa2015.reg.pv(x, pvlabel = "READ", by, data, 
export = FALSE, name = "output", folder = getwd(), std=FALSE)
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "READ", for overall reading performance.
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
  \item{std}{
A logical value. If TRUE standardised regression coefficients are calculated.
}
}

\value{
pisa.reg.pv prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals, replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg". 
}

\seealso{
timss.reg.pv, pirls.reg.pv, piaac.reg.pv
}
\examples{
\dontrun{
# Table I.2.3a, p. 305, International Report 2012
pisa.reg.pv(pvlabel="MATH", x="ST04Q01", by = "IDCNTRYL", data=pisa)
}
}