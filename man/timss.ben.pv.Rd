\name{timss.ben.pv}
\alias{timss.ben.pv}
\alias{timss2015.ben.pv}
\title{
TIMSS international benchmarks
}
\description{
timss.ben.pv calculates the percentage of students performing at or above the cut-off points (scores) given by the useR. The default are the benchmarks established by PIRLS/TIMSS
}
\usage{
timss.ben.pv(pvlabel, by,  cutoff, data,
export = FALSE, name = "output", folder = getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "BSMMAT", for overall math performance.
}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff = c(400, 475, 550, 625)).
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
timss.ben.pv returns a data frame with the percentage of students at or above the benchmark and the corresponding standard error.

The total weight, "TOTWGT" and the cut-off points or benchmarks are defined in the config object.

}

\seealso{
pirls.ben.pv, pisa.ben.pv
}
\examples{
\dontrun{
# TIMSS G8: Exhibit 2.14 User Guide TIMSS 2011, p. 24
timss.ben.pv(pvlabel="BSMMAT", by="IDCNTRYL", cutoff = c(400, 475, 550, 625), data=timss8g)
# TIMSS G4: Exhibit 2.2 International Report TIMSS 2011, p. 90
timss.ben.pv(pvlabel="ASMMAT", by="IDCNTRYL", data=timss4g)
}
}


