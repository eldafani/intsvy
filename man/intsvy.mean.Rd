\name{piaac.mean}
\alias{piaac.mean}
\title{
Calculates mean of variable in PIAAC data
}
\description{
Calculates the mean of an observed variable (NOT one with plausible values) and its standard error.
}
\usage{
piaac.mean(variable, by, data, export = FALSE, 
name = "output", folder = getwd())
}

\arguments{
  \item{variable}{
The label corresponding to the observed variable, for example, "AGE_R" for age of respondent.
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
piaac.mean returns a data frame with means and standard errors.
}

\seealso{
pisa.mean, timss.mean, pirls.mean
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
piaac.mean(variable="AGE_R", by="CNTRYID", data=piaac)
}
}
