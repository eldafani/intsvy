\name{timss.mean.pv}
\alias{timss.mean.pv}
\title{
Calculates mean achievement score
}
\description{
timss.mean.pv uses five plausible values to calculate the mean achievement score and its standard error
}
\usage{
timss.mean.pv(pvlabel, by, data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores, for example, paste0("BSMMAT0", 1:5).
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame.
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
timss.mean.pv returns a data frame with the mean values and standard errors.
}

\seealso{
pirls.mean.pv, pisa.mean.pv
}
\examples{\dontrun{
timss.mean.pv(pvlabel= paste0("BSMMAT0", 1:5), by= "IDCNTRYL", data=timss4g)
timss.mean.pv(pvlabel= paste0("BSMMAT0", 1:5), by= c("IDCNTRYL", "ITSEX"), data=timss8g)
}
}

