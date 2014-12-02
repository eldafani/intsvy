\name{piaac.ben.pv}
\alias{piaac.ben.pv}
\title{PIAAC proficiency levels}
\description{Calculates percentage of population at each proficiency level defined by PIAAC. Or at proficiency levels provided by the user.
}
\usage{
piaac.ben.pv(pvlabel, cutoff = c(175.99, 225.99, 275.99, 325.99, 375.99), 
    by, weight="SPFWT0", brr_weight="SPFWT", data, export=FALSE, 
    name= "output", folder=getwd())
}
\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "LIT", for overall reading performance.
}
  \item{cutoff}{
The cut-off points for calculating the percentage of students. The default are the proficiency levels in PIAAC.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="CNTRYID"), but could be any other categorical variable. 	
}
  \item{weight}{
The label for total weight, "SPFWT0", in PIAAC. 	
}
  \item{brr_weight}{
The label for brr_weights in PIAAC. 	
}
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
