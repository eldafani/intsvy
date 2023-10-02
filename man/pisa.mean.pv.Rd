\name{pisa.mean.pv}
\alias{pisa.mean.pv}
\title{
Calculates mean achievement score
}
\description{
pisa.mean.pv uses five plausible values to calculate the mean achievement score and its standard error
}
\usage{
pisa.mean.pv(pvlabel, by, data, export = FALSE, name = "output", 
     folder = getwd())
}

\arguments{
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores, for example, paste0("PV",1:10,"MATH").
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
pisa.mean.pv returns a data frame with the mean values and standard errors.
}

\seealso{
timss.mean.pv, pirls.mean.pv, piaac.mean.pv
}
\examples{
\dontrun{
pisa.mean.pv(pvlabel = paste0("PV",1:10,"MATH"), by = "IDCNTRYL", data = pisa)
pisa.mean.pv(pvlabel = paste0("PV",1:10,"MATH"), by = c("IDCNTRYL", "ST04Q01"), data = pisa)
pisa.mean.pv(pvlabel = "paste0("PV",1:10,"MATH"), by = "IDCNTRYL", data = pisa)
}
}

