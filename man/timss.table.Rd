\name{timss.table}
\alias{timss.table}
\alias{timss2015.table}
\title{
Frequency table
}
\description{
timss.table produces a frequency table for a categorical variable printing percentages and standard errors. Information about weight is extracted from \code{intsvy:::pirls_conf}.
}
\usage{
timss.table(variable, by, data, 
export = FALSE, name = "output", folder = getwd())
}

\arguments{
    \item{variable}{
The data label with the variable to be analysed.
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
timss.table returns a data frame with percentages and standard errors.
}



\seealso{
pirls.table, pisa.table
}
\examples{
\dontrun{

# TIMSS G4: Exhibit 8.1 International Report 2011, p. 330
timss.table(variable="ASDGSLM", by="IDCNTRYL", data=timss4g)

# TIMSS G8: Exhibit 2.19 User Guide TIMSS 2011, p. 29
timss.table(variable="BSDGSLM", by="IDCNTRYL", data=timss8g)

}
}

