\name{pisa.ben.pv}
\alias{pisa.ben.pv}
\title{PISA proficiency levels}
\description{Calculates percentage of students at each proficiency level defined by PISA. Or at proficiency levels provided by the useR.
}
\usage{
pisa.ben.pv(pvlabel, cutoff = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30), 
by, weight="W_FSTUWT", data, export=FALSE, name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "READ", for overall reading performance.
}
  \item{cutoff}{
The cut-off points for calculating the percentage of students. The default are the proficiency levels in PISA
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
  \item{weight}{
The label for total weight, "W_FSTUWT", in PISA. 	
}
  \item{data}{
An R object, normally a data frame, containing the data from PISA.
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
}

\seealso{
timss.ben.pv, pirls.ben.pv
}
\examples{
\dontrun{
# Table I.2.1a, p. 298 International Report 2012 Volume I
pisa.ben.pv(pvlabel="MATH", by="IDCNTRYL", data=pisa)
}
}
