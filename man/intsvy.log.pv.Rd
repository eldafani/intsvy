\name{intsvy.log.pv}
\alias{intsvy.log.pv}
\title{Logistic regression analysis with plausible values}
\description{
intsvy.log.pv performs logistic regression with plausible values and replicate weights.
}
\usage{
intsvy.log.pv(pvlabel, x, cutoff, by, data, export=FALSE, name= "output", 
folder=getwd(), config)
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
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
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.log.pv returns a data frame with coefficients, standard errors, t-values, and odds ratios. If "by" is specified, results are reported in a list. Weights, e.g. "TOTWGT" for PIRLS, are defined in the config argument.   
}

\seealso{
pisa.log.pv, pirls.log.pv, timss.log.pv
}

\examples{
\dontrun{
intsvy.log.pv(pvlabel="MATH", cutoff= 606.99, x="ESCS", by="IDCNTRYL", 
data=pisa, config=pisa_conf)
intsvy.log.pv(pvlabel="BSMMAT", cutoff= 550, x="ITSEX", by="IDCNTRYL", 
data=timss8g, config=timss8_conf)
}}