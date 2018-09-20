\name{timss.log.pv}
\alias{timss.log.pv}
\alias{timss2015.log.pv}
\title{Logistic regression analysis with plausible values}
\description{
timss.log.pv performs logistic regression with plausible values and replicate weights.
}
\usage{
timss.log.pv(pvlabel="BSMMAT", x, by, cutoff, 
           data, export=FALSE, name= "output", folder=getwd())
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "BSMMAT", for overall mathematics performance.
}
  \item{x}{
Data labels of independent variables.
}
  \item{cutoff}{
The cut-off point at which the dependent plausible values scores are dichotomised (1 is larger than the cut-off)
}
  \item{by}{
The label for the categorical grouping variable (i.e., by="IDCNTRYL") or variables (e.g., x= c("IDCNTRYL", "ITSEX")).   
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
timss.log.pv returns a data frame with coefficients, standard errors, t-values, and odds ratios. If "by" is specified, results are reported in a list. 
}

\seealso{
pisa.log.pv, pirls.log.pv
}

\examples{
\dontrun{
timss.log.pv(pvlabel="BSMMAT", cutoff= 550, x=c("ITSEX", "BSBGSLM"), by="IDCNTRYL", data=timss8g)
intsvy.log.pv(pvlabel="BSMMAT", cutoff= 550, x="ITSEX", by="IDCNTRYL", 
data=timss8g, config=timss8_conf)
}}