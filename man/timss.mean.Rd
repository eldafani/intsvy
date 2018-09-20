\name{timss.mean}
\alias{timss.mean}
\alias{timss2015.mean}
\title{
Calculates mean of variable
}
\description{
Calculates the mean of an observed variable (NOT one with plausible values) and its standard error.
}
\usage{
timss.mean(variable, by, data, 
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
timss.mean returns a data frame with means and standard errors.
}

\seealso{
pirls.mean, pisa.mean
}
\examples{
\dontrun{
# TIMSS G4: Exhibit 8.1 International Report TIMSS 2011, p. 330
timss.mean(variable='ASBGSLM', by='IDCNTRYL', data=timss4g)

# TIMSS G8: Exhibit 2.17 User Guide TIMSS 2011, p. 27
timss.mean(variable='BSBGSLM', by='IDCNTRYL', data=timss8g)
}
}

