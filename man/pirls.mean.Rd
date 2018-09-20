\name{pirls.mean}
\alias{pirls.mean}
\alias{pirls2016.mean}
\title{
Calculates mean of variable
}
\description{
Calculates the mean of an observed variable (NOT one with plausible values) and its standard error.
}
\usage{
pirls.mean(variable, by, data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{variable}{
The label corresponding to the observed variable, for example, "ASDAGE", for the age of the student.
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
pirls.mean returns a data frame with means and standard errors.
}

\seealso{
timss.mean, pisa.mean
}
\examples{
\dontrun{
# PIRLS: Exhibit 2.17 User Guide PIRLS 2011, p. 28
pirls.mean(variable='ASBHELA', by= 'IDCNTRYL', data=pirls)
}
}
