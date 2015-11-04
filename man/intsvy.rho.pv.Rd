\name{intsvy.rho.pv}
\alias{intsvy.rho.pv}
\title{Two-way weighted correlation with plausible values
}
\description{
intsvy.rho.pv calculates the correlation and standard error among two achievement variables each based on 5 plausible values or one achievement variable and an observed variable (i.e., with observed scores rather than plausible values).
}
\usage{
intsvy.rho.pv(variable, pvlabels, by, data, export=FALSE, 
name= "output", folder=getwd(), config)
}

\arguments{
  \item{variable}{
A data label for the observed variable 
}
  \item{pvlabels}{
One or two labels describing the achievement variables.
}
  \item{by}{
The label for the grouping variable, usually the countries (i.e., by="IDCNTRYL"), but could be any other categorical variable. 	
}
 \item{data}{
An R object, normally a data frame, containing the data.
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
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.rho returns a matrix including correlation and standard error values.
}

\seealso{
timss.rho, pirls.rho.pv, timss.rho.pv
}
\examples{
\dontrun{
timss.rho.pv(variable="BSDGEDUP", pvlabel="BSMMAT", by="IDCNTRYL", data=timss)
}
}
