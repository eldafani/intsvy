\name{timss.per.pv}
\alias{timss.per.pv}
\title{TIMSS percentiles}
\description{Calculates percentiles for plausible values}
\usage{
  timss.per.pv(pvlabel, by, per, data, export=FALSE, name= "output", 
  folder=getwd())
}
\arguments{
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.
  }
   \item{per}{
  User-defined percentiles (e.g., per = c(5, 10, 25, 75, 90, 95)).
  }
  \item{by}{
    The label of the categorical grouping variable (e.g., by="IDCNTRYL") or variables (e.g., by=c("IDCNTRYL", "ITSEX")).   
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
  timss.per.pv returns a data frame with percentiles and associated standard errors. Default weights (e.g. "TOTWGT" in TIMSS) and percentiles are specified in the config parameter.    
}

\seealso{
  pisa.per.pv, pirls.per.pv
}
\examples{
  \dontrun{
timss.per.pv(pvlabel=paste0("BSMMAT0", 1:5), 
per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=timssg8)
  }
}
