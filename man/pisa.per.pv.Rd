\name{pisa.per.pv}
\alias{pisa.per.pv}
\title{PISA percentiles}
\description{Calculates percentiles for plausible values}
\usage{
  pisa.per.pv(pvlabel="READ", per = c(5, 10, 25, 75, 90, 95), by, weight="W_FSTUWT", 
  brr_weight="W_FSTR", data, export=FALSE, name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
    The label corresponding to the achievement variable, for example, "READ", for overall 
    reading performance.
  }
  \item{per}{
    User-defined percentiles (e.g., per = c(5, 10, 25, 75, 90, 95)).
  }
  \item{by}{
    The label of the categorical grouping variable (e.g., by="IDCNTRYL") or variables (e.g., 
    by=c("IDCNTRYL", "ST79Q03")).   
  }
  \item{weight}{
    The label of the total weight, "W_FSTUWT", in PISA. 	
  }
  \item{brr_weight}{
    The label of the brr weight prefix, "W_FSTR", PISA.   
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
  pisa.per.pv returns a data frame with percentiles and associated standard errors.
}

\seealso{
  timss.per.pv, pirls.per.pv
}
\examples{
  \dontrun{
    # Table I.2.3d, p. 309 International Report 2012 Volume I
    pisa.per.pv(pvlabel="MATH", by="IDCNTRYL", data=pisa)
  }
}
