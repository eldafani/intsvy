\name{pirls.mean.pv}
\alias{pirls.mean.pv}
\alias{pirls2016.mean.pv}
\title{Calculates mean achievement score
}
\description{
pirls.mean.pv uses five plausible values to calculate the mean achievement score and its standard error
}
\usage{
pirls.mean.pv(pvlabel = "ASRREA", by,  
data, export = FALSE, name = "output", folder = getwd())
pirls2016.mean.pv(pvlabel = "ASRREA", by,  
data, export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
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
pirls.mean.pv returns a data frame with the mean values and standard errors.
}


\seealso{
timss.mean.pv, pisa.mean.pv
}
\examples{
\dontrun{
# PIRLS: Exhibit 2.5 User Guide PIRLS 2011, p. 15
pirls.mean.pv(pvlabel="ASRREA", by= "IDCNTRYL", data=pirls)
# PIRLS: Exhibit 2.8 User Guide PIRLS 2011, p. 18
pirls.mean.pv(pvlabel="ASRREA", by= c("IDCNTRYL", "ITSEX"), data=pirls)
}
}

