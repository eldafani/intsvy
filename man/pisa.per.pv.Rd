\name{pisa.per.pv}
\alias{pisa.per.pv}
\title{PISA percentiles}
\description{Calculates percentiles for plausible values.
}
\usage{
  pisa.per.pv(pvlabel, by, per, data, export=FALSE, name= "output", 
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
    The label of the categorical grouping variable (e.g., by="IDCNTRYL") or variables (e.g., 
    by=c("IDCNTRYL", "ST79Q03")).   
  }
  \item{data}{
    An R object, normally a data frame, containing the data from PISA.
  }
  \item{export}{
    A logical value. If TRUE, the output is exported to a file in comma-separated value format 
    (.csv) that can be opened from LibreOffice or Excel. 
  }
  \item{name}{
    The name of the exported file.
  }
  \item{folder}{
    The folder where the exported file is located.
  }
}

\value{
  pisa.per.pv returns a data frame with percentiles and associated standard errors. Default weights (e.g. "TOTWGT" in TIMSS) and percentiles are specified in the config parameter.    
}

\seealso{
  timss.per.pv, pirls.per.pv
}
\examples{
  \dontrun{
pisa.per.pv(pvlabel=paste0("PV",1:10,"MATH"), per=c(10, 25, 75, 90), by="IDCNTRYL", data=pisa)
  }
}
