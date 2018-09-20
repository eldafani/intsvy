\name{timss.log}
\alias{timss.log}
\alias{timss2015.log}
\title{Logistic regression analysis}
\description{timss.log performs logistic regression analysis for an observed depedent variable (NOT for plausible values)
}
\usage{
timss.log(y, x, by, data, export = FALSE, 
name = "output", folder = getwd())
}
\arguments{
  \item{y}{
Label for dependent variable
}
  \item{x}{
Data labels of independent variables (e.g., x = c("ASDHEHLA", "ITSEX") ).
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
timss.log prints a data frame with coefficients, standard errors, t-values, and odds ratios. Results are stored in a list object of class "intsvy.reg". 
}

\seealso{
pirls.log, pisa.log
}
\examples{
\dontrun{
# Table IV.5.14, p. 457 International Report 2012

pisa12$SKIP[!(pisa12$ST09Q01 =="None" & pisa12$ST115Q01 == "None")] <- 1
pisa12$SKIP[pisa12$ST09Q01 =="None" & pisa12$ST115Q01 == "None"] <- 0

pisa12$LATE[!pisa12$ST08Q01=="None"] <- 1
pisa12$LATE[pisa12$ST08Q01=="None"] <- 0

pisa.log(y="SKIP", x="LATE", by="IDCNTRYL", data = pisa12)

# from PISA2012lite

student2012$SKIP[!(student2012$ST09Q01 =="None  " & student2012$ST115Q01 == 1)] <- 1
student2012$SKIP[student2012$ST09Q01 =="None  " & student2012$ST115Q01 == 1] <- 0

student2012$LATE[!student2012$ST08Q01=="None  "] <- 1
student2012$LATE[student2012$ST08Q01=="None  "] <- 0

pisa.log(y="SKIP", x="LATE", by="CNT", data = student2012)

}}