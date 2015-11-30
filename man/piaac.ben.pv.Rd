\name{piaac.ben.pv}
\alias{piaac.ben.pv}
\title{PIAAC proficiency levels}
\description{Calculates percentage of population at each proficiency level defined by PIAAC. Or at proficiency levels provided by the user.
}
\usage{
piaac.ben.pv(pvlabel, by, data, cutoff, export=FALSE, 
    name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "LIT", for overall reading performance.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="CNTRYID"), but could be any other categorical variable. 	
}
  \item{cutoff}{
The cut-off points for the assessment benchmarks (e.g., cutoff= c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)). }
  \item{data}{
An R object, normally a data frame, containing the data from PIAAC.
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
piaac.ben.pv returns a data frame with the percentage of students at each proficiency level and its corresponding standard error.

The total weight, "TOTWGT" and the cut-off points or benchmarks are defined in the config object.

}

\seealso{
timss.ben.pv, pirls.ben.pv, pisa.ben.pv
}
\examples{
\dontrun{
#Table A2.5
#Percentage of adults scoring at each proficiency level in numeracy
piaac.ben.pv(pvlabel="NUM", by="CNTRYID", data=piaac)
#Table A2.1
#Percentage of adults scoring at each proficiency level in literacy
piaac.ben.pv(pvlabel="LIT", by="CNTRYID", data=piaac)
}
}
