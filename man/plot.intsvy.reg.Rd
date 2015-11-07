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
\method{plot}{intsvy.reg}(x, ..., vars, se = TRUE, sort = FALSE)
}

\arguments{
  \item{x}{
An object of the class intsvy.reg returned by  pisa.reg, pisa.reg.pv, piaac.reg 
and piaac.reg.pv functions.
}
  \item{...}{
Other arguments
}
  \item{vars}{
Variable labels of coefficients to be plotted. If none selected all coefficients are plotted including the R-squared 
}
  \item{se}{
If TRUE add whiskers for standard errors.
}
  \item{sort}{
If TRUE groups are sorted in alphabetical order.
}
}

\value{
Returns object of ggplot class with barplot. As with other ggplot objects the plus sign "+" can be used outside 
this function to modify graph parameters of the returning ggplot object. Works for one way, two-way and three-way 
contingency tables. 
}

\seealso{
plot.intsvy.table, plot.intsvy.mean
}
\examples{
\dontrun{
# Independent variables
x.vars <- c("ESCS", "COGACT", "TCHBEHTD", "TCHBEHSO")
# Model estimation
my.mod <- pisa.reg.pv(pvlabel="MATH", x=x.vars, by="IDCNTRYL", data=pisa12)
# Plot
plot(gen.mod, vars = c("COGACT", "TCHBEHTD", "TCHBEHSO"), sort=TRUE)
}
}
