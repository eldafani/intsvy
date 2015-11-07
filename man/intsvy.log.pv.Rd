\name{intsvy.log.pv}
\alias{intsvy.log.pv}
\title{Logistic regression analysis with plausible values}
\description{
intsvy.log.pv performs logistic regression with plausible values and replicate weights.
}
\usage{
intsvy.log.pv(pvlabel="ASRREA", x, by, 
           data, export=FALSE, name= "output", folder=getwd(), config)
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
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
intsvy.log.pv returns a data frame with coefficients, standard errors, t-values, and odds ratios. If "by" is specified, results are reported in a list. 

Continuous variables are divided into a binary one with the use of cutoff2 that is in defined in the config argument.

Weight, e.g. "TOTWGT" for PIRLS, are defined in the config argument.   

}

\seealso{
pisa.log.pv, timss.log.pv
}
