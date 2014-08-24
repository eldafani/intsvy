\name{piaac.reg}
\alias{piaac.reg}
\title{Regression analysis for PIAAC}
\description{
piaac.reg performs linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values)
}
\usage{
piaac.reg(y, x, by, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{y}{
Label for dependent variable.
}
  \item{x}{
Data labels of independent variables.
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
  \item{folder}{
The folder where the exported file is located.
}
}

\value{
piaac.reg returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list.
If the "by" argument is set, then the returning object is of the class "intsvy.reg" with overloaded function plot().
}


\seealso{
pisa.reg, pirls.reg, timss.reg
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
piaac.reg(y="AGE_R", x="GENDER_R", by="CNTRYID", data=piaac)
}}
