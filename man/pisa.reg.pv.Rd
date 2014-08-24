\name{pisa.reg.pv}
\alias{pisa.reg.pv}
\title{Regression analysis with plausible values}
\description{
pisa.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
pisa.reg.pv(x, pvlabel = "READ", by, data, 
export = FALSE, name = "output", folder = getwd(), weight="W_FSTUWT")
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
  \item{weight}{
The variable with final weights.
}
}

\value{
pisa.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
If the "by" argument is set, then the returning object is of the class "intsvy.reg" with overloaded function plot().
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

