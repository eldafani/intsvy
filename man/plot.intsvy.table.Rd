\name{plot.intsvy.table}
\alias{plot.intsvy.table}
\title{
Graphical representation of frequency tables
}
\description{
Functions pisa.table and piaac.table produce object of the class intsvy.table. 
The function plot.intsvy.table presents this table graphically.
}
\usage{
\method{plot}{intsvy.table}(x, se=FALSE, stacked=FALSE, centered = FALSE, midpoint = NA, ...)
}

\arguments{
  \item{x}{
An object of the class intsvy.table returned by pisa.table or piaac.table functions.
}
  \item{se}{
If TRUE add whiskers for standard errors  (only for stacked=FALSE).
}
  \item{stacked}{
If TRUE plot bars stacked one over another.
}
  \item{centered}{
If TRUE then bars will be centered around \code{midpoint}.
}
  \item{midpoint}{
A single number, which specifies the segment around which bars are centered. By default it's the middle segment calculated as \code{(n.levels + 1)/2}. If \code{n.levels} is odd then bars are centered around the beginning of the selected segment. If \code{n.levels} is even then bars are centered around the middle of the selected segment. 
}
  \item{...}{
Not used. Required for cran-check.
}
}

\value{
Returns object of ggplot class with barplot. Works for one way, two-way and three-way contingency tables.
}

\seealso{
plot.intsvy.mean, plot.intsvy.reg
}
\examples{
\dontrun{
# install pbiecek/PIAAC package from github to have access to piaac data
# age distribution in whole dataset
(ptable <- piaac.table(variable="AGEG10LFS", data=piaac))

# age distribution in whole dataset
plot(ptable)
plot(ptable, centered=TRUE)

# age distribution within countries
head(ptableC <- piaac.table(variable="AGEG10LFS", by="CNTRYID", data=piaac))

# age distribution within countries
plot(ptableC, stacked=TRUE)
plot(na.omit(ptableC), centered=TRUE)

# age distribution within countries and gender segments
head(ptableCA <- piaac.table(variable="AGEG10LFS", by=c("CNTRYID", "GENDER_R"), data=piaac))

# age distribution within countries and gender segments
plot(na.omit(ptableCA), stacked=TRUE)
plot(na.omit(ptableCA), centered=TRUE)

}
}
