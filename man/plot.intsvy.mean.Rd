\name{plot.intsvy.mean}
\alias{plot.intsvy.mean}
\title{
Graphical representation of means in groups
}
\description{
Functions pisa.mean, pisa.mean.pv, piaac.mean, piaac.mean.pv produce object of 
the class intsvy.mean. 
The function plot.intsvy.mean presents these means graphically.
}
\usage{
\method{plot}{intsvy.mean}(x, se = TRUE, sort = FALSE, ...)
}

\arguments{
  \item{x}{
An object of the class intsvy.mean returned by pisa.mean, pisa.mean.pv, piaac.mean 
or piaac.mean.pv functions.
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
Returns object of ggplot class with dotplot. Works for one way, two-way and three-way effects.
}

\seealso{
plot.intsvy.table, plot.intsvy.reg
}
\examples{
\dontrun{
# Country averages
head(pmeansNC <- piaac.mean.pv(pvlabel="NUM", by="CNTRYID", data=piaac, export=FALSE))

# plotting country average NUM performance 
plot(pmeansNC) + ggtitle("Country performance in NUM")
# without se bars, not good idea
plot(pmeansNC, se=FALSE)
# sorted, thats better
plot(pmeansNC, sort=TRUE)

# Country averages for different age groups
head(pmeansNCA <- piaac.mean.pv(pvlabel="NUM", by=c("CNTRYID", "AGEG10LFS"), 
                        data=piaac, export=FALSE))
#
# plotting country average within 
# age groups NUM performance 
plot(pmeansNCA, sort=TRUE)

# Country averages for different age and gender groups (changed order)
head(pmeansNCGA <- piaac.mean.pv(pvlabel="NUM", by=c("CNTRYID", "GENDER_R", "AGEG10LFS"), 
                         data=piaac, export=FALSE))
#
# plotting country average within 
# age and gender groups NUM performance 
plot(na.omit(pmeansNCGA), sort=TRUE)

}
}
