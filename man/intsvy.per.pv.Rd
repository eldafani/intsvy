\name{intsvy.per.pv}
\alias{intsvy.per.pv}
\title{Calculates percentiles}
\description{Calculates percentiles for plausible values}
\usage{
  intsvy.per.pv(pvlabel, by, per, data, export=FALSE, name= "output", 
  folder=getwd(), config)
}
\arguments{
  \item{pvlabel}{
  The label corresponding to the achievement variable, for example, "BSMMAT", for overall mathematics performance.
  }
  \item{per}{
  User-defined percentiles (e.g., per = c(5, 10, 25, 75, 90, 95)).
  }
  \item{by}{
    The label of the categorical grouping variable (e.g., by="IDCNTRYL") or variables (e.g., by=c("IDCNTRYL", "ITSEX")).   
  }
   \item{data}{
    An R object, normally a data frame, containing the data from intsvy studies.
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
  intsvy.per.pv returns a data frame with percentiles and associated standard errors. Default weights (e.g. "TOTWGT" in TIMSS) and percentiles are specified in the config parameter.    
}

\seealso{
  pisa.per.pv, pirls.per.pv, timss.per.pv
}
\examples{
  \dontrun{
    timss.per.pv(pvlabel="BSMMAT", per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=timss8)
    intsvy.per.pv(pvlabel="BSMMAT",  by="IDCNTRYL", data=timss8, config=timss8_conf)
    
    pirls.per.pv(pvlabel="ASRREA", by="IDCNTRYL", data=pirls)
    intsvy.per.pv(pvlabel="ASRREA", per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=pirls, 
    config=pirls_conf)
    
    pisa.per.pv(pvlabel="MATH", per=c(10, 25, 75, 90), by="CNT", data=student2012)
    intsvy.per.pv(pvlabel="MATH", by="CNT", data=student2012, config=pisa_conf)
  }
}
