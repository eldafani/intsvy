\name{timss.reg.pv}
\alias{timss.reg.pv}
\title{Regression analysis with plausible values}
\description{
timss.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
timss.reg.pv(x, pvlabel = "BSMMAT", weight = "TOTWGT", by, 
data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "BSMMAT", for overall math performance in Grade 8.
}
  \item{weight}{
The total weight, "TOTWGT" in the TIMSS dataset.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from TIMSS.
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
timss.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}

\seealso{
pirls.reg.pv, pisa.reg.pv
}
\examples{
\dontrun{
# TIMSS G8: Exhibit 2.11, User Guide TIMSS 2011, p.21
timss8g$SEX[timss8g$ITSEX=="BOY"]=1
timss8g$SEX[timss8g$ITSEX=="GIRL"]=0
timss.reg.pv(pvlabel="BSMMAT", by=c("IDCNTRYL"), x="SEX", data=timss8g)
}
}
