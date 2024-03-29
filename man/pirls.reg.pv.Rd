\name{pirls.reg.pv}
\alias{pirls.reg.pv}
\title{Regression analysis with plausible values}
\description{
pirls.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
pirls.reg.pv(x, pvlabel, by, 
data, std=FALSE, export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{x}{
Data labels of independent variables (e.g., x = c("ASDHEHLA", "ITSEX") ).
}
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
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
}

\value{
pirls.reg.pv prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals, replicate coefficients, variance within and between, and the regression data.frame in a list object of class "intsvy.reg". 
}


\seealso{
timss.reg.pv, pisa.reg.pv
}
\examples{
\dontrun{
pirls$SEX[pirls$ITSEX=="BOY"]=1
pirls$SEX[pirls$ITSEX=="GIRL"]=0
pirls.reg.pv(pvlabel= paste0("ASRREA0", 1:5), by="IDCNTRYL", x="SEX", data=pirls)
}
}