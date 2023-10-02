\name{pirls.log.pv}
\alias{pirls.log.pv}
\title{Logistic regression analysis with plausible values}
\description{
pirls.log.pv performs logistic regression with plausible values and replicate weights.
}
\usage{
pirls.log.pv(pvlabel, x, cutoff, by, 
           data, export=FALSE, name= "output", folder=getwd())
}

\arguments{
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.}
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
pirls.log.pv returns a data frame with coefficients, standard errors, t-values, and odds ratios. If "by" is specified, results are reported in a list. 
}

\seealso{
pisa.log.pv, timss.log.pv
}

\examples{
\dontrun{
timss.log.pv(pvlabel="paste0("BSMMAT0", 1:5), cutoff= 550, 
x=c("ITSEX", "BSBGSLM"), by="IDCNTRYL", data=timss8g)

intsvy.log.pv(pvlabel=paste0("BSMMAT0", 1:5), cutoff= 550, 
x="ITSEX", by="IDCNTRYL", data=timss8g, config=timss8_conf)
}}