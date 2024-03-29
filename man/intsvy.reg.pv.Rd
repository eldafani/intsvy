\name{intsvy.reg.pv}
\alias{intsvy.reg.pv}
\title{Regression analysis with plausible values}
\description{
intsvy.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
intsvy.reg.pv(x, pvnames, by, 
data, std=FALSE, export = FALSE, name = "output", folder = getwd(), config)
}

\arguments{
  \item{pvnames}{
The names of columns corresponding to the achievement plausible scores.
}
  \item{x}{
Data labels of independent variables.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other 
categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
  \item{std}{
A logical value. If TRUE standardised regression coefficients are calculated.
}
\item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that 
can be opened from LibreOffice or Excel. 
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
intsvy.reg.pv prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals, replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg". 
}

\seealso{
piaac.reg.pv, pirls.reg.pv, pisa.reg.pv, timss.reg.pv
}
\examples{
\dontrun{
intsvy.reg.pv(pvnames=paste0("PV",1:10,"MATH") , x="ST04Q01", 
by = "IDCNTRYL",data=pisa, config=pisa_conf)

intsvy.reg.pv(pvnames=paste0("PVLIT", 1:10), x="GENDER_R", by = "CNTRYID", 
data=piaac, config=piaac_conf)

intsvy.reg.pv(pvnames=paste0("BSMMAT0", 1:5), by="IDCNTRYL", x="ITSEX", 
data=timss8g, config=timss8_conf)

intsvy.reg.pv(pvnames=paste0("ASRREA0", 1:5), by="IDCNTRYL", x="ITSEX", 
data=pirls, config=pirls_conf)
}
}
