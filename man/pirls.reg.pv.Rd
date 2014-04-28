\name{pirls.reg.pv}
\alias{pirls.reg.pv}
\title{Regression analysis with plausible values}
\description{
pirls.reg.pv performs linear regression analysis (OLS) with plausible values and replicate weights.
}
\usage{
pirls.reg.pv(x, pvlabel = "ASRREA", weight = "TOTWGT", by, 
data, export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{x}{
Data labels of independent variables (e.g., x = c("ASDHEHLA", "ITSEX") ).
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
}
  \item{weight}{
The total weight, "TOTWGT" in the PIRLS dataset.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PIRLS.
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
pirls.reg.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}


\seealso{
timss.reg.pv, pisa.reg.pv
}
\examples{
\dontrun{

# PIRLS: Exhibit 2.11, User Guide PIRLS 2011, p.21
pirls$SEX[pirls$ITSEX=="BOY"]=1
pirls$SEX[pirls$ITSEX=="GIRL"]=0
pirls.reg.pv(pvlabel="ASRREA", by="IDCNTRYL", x="SEX", data=pirls)
}
}


