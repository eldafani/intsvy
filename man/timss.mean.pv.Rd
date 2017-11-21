\name{timss.mean.pv}
\alias{timss.mean.pv}
\alias{timss2015.mean.pv}
\title{
Calculates mean achievement score
}
\description{
timss.mean.pv uses five plausible values to calculate the mean achievement score and its standard error
}
\usage{
timss.mean.pv(pvlabel = "BSMMAT", by, data, 
export = FALSE, name = "output", folder = getwd())
timss2015.mean.pv(pvlabel = "BSMMAT", by, data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "BSMMAT", for overall math performance in Grade 8.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
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
timss.mean.pv returns a data frame with the mean values and standard errors.
}

\seealso{
pirls.mean.pv, pisa.mean.pv
}
\examples{\dontrun{
# TIMSS G4: Exhibit 1.1 International report TIMSS, p. 40
timss.mean.pv(pvlabel="ASMMAT", by= "IDCNTRYL", data=timss4g)

# TIMSS G8: Exhibit 2.8 User Guide TIMSS 2011, p. 18
timss.mean.pv(pvlabel="BSMMAT", by= c("IDCNTRYL", "ITSEX"), data=timss8g)
}
}

