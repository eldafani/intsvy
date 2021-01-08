\name{timss.ben.pv}
\alias{timss.ben.pv}
\title{
TIMSS international benchmarks
}
\description{
timss.ben.pv calculates the percentage of students performing at or above the cut-off points (scores) given by the useR. The default are the benchmarks established by PIRLS/TIMSS
}
\usage{
timss.ben.pv(pvlabel, by,  cutoff, data, atlevel=FALSE,
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
 \item{atlevel}{
A logical value. If TRUE, percentages at each level are calculated. Otherwise (FALSE), percentages at or above levels are reported.
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
timss.ben.pv(pvlabel="BSMMAT", by="IDCNTRYL", cutoff = c(400, 475, 550, 625), data=timss8g)
timss.ben.pv(pvlabel="ASMMAT", by="IDCNTRYL", data=timss4g)
}
}