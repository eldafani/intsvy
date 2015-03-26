\name{pisa.log.pv}
\alias{pisa.log.pv}
\title{Regression analysis with plausible values}
\description{
pisa.log.pv performs logistic regression with plausible values and replicate weights.
}
\usage{
pisa.log.pv(pvlabel="READ", cutoff= 606.99, x, by, weight="W_FSTUWT", 
           brr_weight="W_FSTR", data, export=FALSE, name= "output", folder=getwd())
}

\arguments{
  \item{x}{
Data labels of independent variables.
}
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "READ", for overall reading performance.
}
  \item{by}{
The label for the categorical grouping variable (i.e., by="IDCNTRYL") or variables (e.g., x= c("IDCNTRYL", "ST79Q03")).   
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
 \item{brr_weight}{
  The label of the brr weight prefix, "W_FSTR", PISA. 
  }
 \item{weight}{
    The label of the total weight, "W_FSTUWT", in PISA.   
 }
}

\value{
pisa.log.pv returns a data frame with coefficients, standard errors and t-values. If "by" is specified, results are reported in a list. 
}

\seealso{
timss.log.pv, pirls.log.pv
}

