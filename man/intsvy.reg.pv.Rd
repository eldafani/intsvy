\name{intsvy.reg.pv}
\alias{intsvy.reg.pv}
\title{Regression analysis with plausible values}
\description{
intsvy.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
intsvy.reg.pv(x, pvlabel, by, 
data, std=FALSE, export = FALSE, name = "output", folder = getwd(), config)
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "BSMMAT", for overall math performance in Grade 8.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
}
  \item{std}{
A logical value. If TRUE standardised regression coefficients are calculated.
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
intsvy.reg.pv prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals, replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg". 
}

\seealso{
pirls.reg.pv, pisa.reg.pv
}
\examples{
\dontrun{
pisa.reg.pv(pvlabel="MATH", x="ST04Q01", by = "CNT", data=student2012)
intsvy.reg.pv(pvlabel="MATH", x="ST04Q01", by = "CNT", data=student2012, config=intsvy:::pisa_conf)
}
}
