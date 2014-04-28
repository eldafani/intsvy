\name{timss.reg}
\alias{timss.reg}
\title{Regression analysis}
\description{
timss.reg performs linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values)
}
\usage{
timss.reg(y, x, by, weight = "TOTWGT", data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{y}{
Label for dependent variable.
}
  \item{x}{
Data labels of independent variables.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{weight}{
The total weight, "TOTWGT" in the TIMSS dataset.
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
timss.reg returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list.
}


\seealso{
pirls.reg
}
\examples{
\dontrun{
timss.reg(y="BSDAGE", x="ITSEX", by="IDCNTRYL", data=timss8g)
}}
