\name{plot.intsvy.reg}
\alias{plot.intsvy.reg}
\title{
Graphical representation of regression models in groups
}
\description{
Functions pisa.reg, pisa.reg.pv, piaac.reg and piaac.reg.pv produce object of the class intsvy.reg.
The function plot.intsvy.reg presents this list of regression models graphically.
}
\usage{
\method{plot}{intsvy.reg}(x, se = TRUE, sort = FALSE, ...)
}

\arguments{
  \item{x}{
An object of the class intsvy.reg returned by  pisa.reg, pisa.reg.pv, piaac.reg 
and piaac.reg.pv functions.
}
  \item{se}{
If TRUE add whiskers for standard errors.
}
  \item{sort}{
If TRUE groups are sorted along averages.
}
  \item{...}{
Not used. Required for cran-check.
}
}

\value{
Returns object of ggplot class with barplot. 
Works for one way, two-way and three-way contingency tables.
}

\seealso{
plot.intsvy.table, plot.intsvy.mean
}
\examples{
\dontrun{
#
# LITeracy explained by GENDER in different countries
rmodelLG <- piaac.reg.pv(pvlabel="LIT", x="GENDER_R", by = "CNTRYID", data=piaac, export=FALSE)
#
# add se intervals
plot(rmodelLG, se=TRUE)
# sorted along R square
plot(rmodelLG, se=TRUE, sort=TRUE)
#
# LITeracy explained by GENDER and level of trust in different countries
rmodelLGI <- piaac.reg.pv(pvlabel="LIT", x=c("GENDER_R", "I_Q06A"), by = "CNTRYID", 
                   data=piaac, export=FALSE) 
#
plot(rmodelLGI, se=TRUE)

}
}
