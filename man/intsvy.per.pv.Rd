\name{intsvy.per.pv}
\alias{intsvy.per.pv}
\title{Calculates percentiles}
\description{Calculates percentiles for plausible values}
\usage{
  intsvy.per.pv(pvnames, by, per, data, export=FALSE, name= "output", 
  folder=getwd(), config)
}
\arguments{
  \item{pvnames}{
The names of columns corresponding to the achievement plausible scores.
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
    timss.per.pv(pvlabel= paste0("BSMMAT0", 1:5), 
    per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=timss8)
    
    intsvy.per.pv(pvnames= paste0("BSMMAT0", 1:5),  by="IDCNTRYL", 
    data=timss8, config=timss8_conf)
    
    pirls.per.pv(pvlabel= paste0("ASRREA0", 1:5), by="IDCNTRYL", data=pirls)
    
    intsvy.per.pv(pvnames= paste0("ASRREA0", 1:5), 
    per = c(5, 10, 25, 50, 75, 90, 95), by="IDCNTRYL", data=pirls, 
    config=pirls_conf)
    
    pisa.per.pv(pvlabel= paste0("PV",1:10,"MATH"), 
    per=c(10, 25, 75, 90), by="CNT", data=pisa)
    
    intsvy.per.pv(pvnames= paste0("PV",1:10,"MATH"), 
    by="CNT", data=pisa, config=pisa_conf)
  }
}
