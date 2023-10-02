\name{pisa.reg.pv}
\alias{pisa.reg.pv}
\title{Regression analysis with plausible values}
\description{
pisa.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
pisa.reg.pv(x, pvlabel, by, data, 
export = FALSE, name = "output", folder = getwd(), std=FALSE)
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.}
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
pisa.reg.pv(pvlabel=paste0("PV",1:10,"MATH"), x="ST04Q01", by = "IDCNTRYL", data=pisa)
}
}
