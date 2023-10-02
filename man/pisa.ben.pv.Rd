\name{pisa.ben.pv}
\alias{pisa.ben.pv}
\title{PISA proficiency levels}
\description{Calculates percentage of students at each proficiency level defined by PISA. Or at proficiency levels provided by the useR.
}
\usage{
pisa.ben.pv(pvlabel, by, cutoff, data, atlevel=FALSE, 
export=FALSE, name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
The names of columns corresponding to the achievement plausible scores.}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff= c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)). }
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
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
pisa.ben.pv returns a data frame with the percentage of students at each proficiency level and its corresponding standard error.

The total weight, "TOTWGT" and the cut-off points or benchmarks are defined in the config object.

}

\seealso{
timss.ben.pv, pirls.ben.pv
}
\examples{
\dontrun{
pisa.ben.pv(pvlabel= paste0("PV",1:10,"MATH"), by="IDCNTRYL", atlevel=TRUE, data=pisa)
}
}
