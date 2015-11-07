\name{piaac.reg.pv}
\alias{piaac.reg.pv}
\title{Regression analysis with plausible values for PIAAC}
\description{
piaac.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
piaac.reg.pv(x, pvlabel = "LIT", by, data, 
export = FALSE, name = "output", std=FALSE, folder = getwd())
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "LIT", for overall literacy performance, "NUM" for numeracy, "PSL" for problem solving.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="CNTRYID"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIAAC.
}
  \item{export}{
A logical value. If TRUE, the output is exported to a file in comma-separated value format (.csv) that can be opened from LibreOffice or Excel. 
}
  \item{name}{
The name of the exported file.
}
  \item{std}{
A logical value. If TRUE standardised regression coefficients are calculated.
}
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
piaac.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
If the "by" argument is set, then the returning object is of the class "intsvy.reg" with overloaded function plot().
}

\seealso{
pisa.reg.pv, timss.reg.pv, pirls.reg.pv
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
piaac.reg.pv(pvlabel="LIT", x="GENDER_R", by = "CNTRYID", data=piaac)
}
}

