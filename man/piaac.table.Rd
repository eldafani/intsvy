\name{piaac.table}
\alias{piaac.table}
\title{
Frequency table
}
\description{
piaac.table produces a frequency table for a categorical variable printing percentages and standard errors.
}
\usage{
piaac.table(variable, by, data, export = FALSE, name = "output", folder = getwd())
}

\arguments{
  \item{variable}{
The data label with the variable to be analysed.
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
piaac.table returns a data frame with percentages and standard errors.
}

\seealso{
pisa.table, timss.table, pirls.table
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
piaac.table(variable="I_Q06A", by="CNTRYID", data=piaac)
piaac.table(variable="GENDER_R", by="CNTRYID", data=piaac)
}
}
