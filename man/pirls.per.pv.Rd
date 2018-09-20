\name{pirls.per.pv}
\alias{pirls.per.pv}
\alias{pirls2016.per.pv}
\title{PIRLS percentiles}
\description{Calculates percentiles for plausible values}
\usage{
  pirls.per.pv(pvlabel="ASRREA", by, per, data, export=FALSE, 
  name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
    The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
  }
   \item{per}{
  User-defined percentiles (e.g., per = c(5, 10, 25, 75, 90, 95)).
  }
  \item{by}{
    The label of the categorical grouping variable (e.g., by="IDCNTRYL") or variables (e.g., by=c("IDCNTRYL", "ITSEX")).   
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
  pirls.per.pv returns a data frame with percentiles and associated standard errors. Default weights (e.g. "TOTWGT" in TIMSS) and percentiles are specified in the config parameter.    
}

\seealso{
  pisa.per.pv, timss.per.pv
}
\examples{
  \dontrun{
    # Appendix F.1, p. 286, PIRLS 2011 International Results in Reading
    pirls.per.pv(pvlabel="ASRREA", per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=pirls)
  }
}
