\name{pirls.ben.pv}
\alias{pirls.ben.pv}
\alias{pirls2016.ben.pv}
\title{PIRLS international benchmarks}
\description{
pirls.ben.pv calculates the percentage of students performing at or above the cut-off points (scores) given by the useR. The default are the benchmarks established by PIRLS/TIMSS.
}
\usage{
pirls.ben.pv(pvlabel, by, cutoff, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "ASRREA", for overall reading performance.
}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff = c(400, 475, 550, 625). 
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
pirls.ben.pv returns a data frame with the percentage of students at or above the benchmark and the corresponding standard error.

The total weight, "TOTWGT" and the cut-off points or benchmarks are defined in the config object.
}

\seealso{
timss.ben.pv, pisa.ben.pv
}
\examples{
\dontrun{
# PIRLS: Exhibit 2.14 User Guide PIRLS 2011, p. 24
pirls.ben.pv(pvlabel="ASRREA", by="IDCNTRYL",  data=pirls)
}
}



