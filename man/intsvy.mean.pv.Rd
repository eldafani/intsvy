\name{intsvy.mean.pv}
\alias{intsvy.mean.pv}
\title{Calculates mean achievement score}
\description{
The fucntion intsvy.mean.pv uses plausible values to calculate the mean achievement score and its standard error.
}
\usage{intsvy.mean.pv(pvnames, by, data, export=FALSE, name= "output", folder=getwd(), config)}

\arguments{
  \item{pvnames}{
The names of collumns corresponding to the achievement score, for example, paste0("PV",1:5,"MATH") for PISA.
}
  \item{by}{
The label for the grouping variable, usually the countries (e.g., by="CNTRYID"), but could be any other categorical variable. 	
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
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.mean.pv returns a data frame with means and standard errors.
}

\seealso{
pisa.mean.pv, timss.mean.pv, pirls.mean.pv
}
\examples{
\dontrun{
intsvy.mean.pv(pvnames = paste0("ASRREA0",1:5), by= "IDCNTRYL", 
    data=pirls, config=pirls_conf)

intsvy.mean.pv(pvnames = paste0("PV",1:5,"MATH"), by="CNT", data=student2012, 
    config=pisa_conf)
    
intsvy.mean.pv(pvnames = paste0("BSMMAT0",1:5), by= "IDCNTRYL", data=timss8g, 
    config=timss8_conf)
    
intsvy.mean.pv(pvnames = paste0("PVNUM", 1:10), by="CNTRYID", data=piaac, 
    config=piaac_conf)    
}
}
