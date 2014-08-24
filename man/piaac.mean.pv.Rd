\name{piaac.mean.pv}
\alias{piaac.mean.pv}
\title{
Calculates mean achievement score for PIAAC data
}
\description{
piaac.mean.pv uses ten plausible values to calculate the mean achievement score and its standard error
}
\usage{
piaac.mean.pv(pvlabel, by, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{pvlabel}{
The label corresponding to the achievement variable, for example, "LIT", for overall literacy performance, "NUM" for numeracy, "PSL" for problem solving.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="CNTRYID"), but could be any other categorical variable. 	
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
piaac.mean.pv returns a data frame with the mean values and standard errors.
}

\seealso{
pisa.mean.pv, timss.mean.pv, pirls.mean.pv
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
piaac.mean.pv(pvlabel = "LIT", by = "CNTRYID", data = piaac)
piaac.mean.pv(pvlabel = "NUM", by=c("CNTRYID", "GENDER_R"), data=piaac)
}
}

