\name{pirls.reg}
\alias{pirls.reg}
\alias{pirls2016.reg}
\title{Regression analysis}
\description{pirls.reg performs linear regression analysis (OLS) for an observed depedent variable (NOT for plausible values)
}
\usage{
pirls.reg(y, x, by, data, export = FALSE, 
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
pirls.reg prints a data.frame with regression results (i.e., coefficients, standard errors, t-values, R-squared) and stores  different regression output including residuals and replicate coefficients in a list object of class "intsvy.reg". 
}

\seealso{
timss.reg
}
\examples{
\dontrun{

# Exhibit 3.13 in User Guide 2006, p. 68

# Recode ASBGBOOK
table(as.numeric(pirls$ASBGBOOK), pirls$ASBGBOOK)
pirls$BOOK[as.numeric(pirls$ASBGBOOK)==1] <- 5
pirls$BOOK[as.numeric(pirls$ASBGBOOK)==2] <- 18
pirls$BOOK[as.numeric(pirls$ASBGBOOK)==3] <- 63
pirls$BOOK[as.numeric(pirls$ASBGBOOK)==4] <- 151
pirls$BOOK[as.numeric(pirls$ASBGBOOK)==5] <- 251
table(pirls$BOOK)

pirls.reg(y= "BOOK", x= "ITSEX", by="IDCNTRYL", data=pirls)
}
}