\name{timssg8.select.merge}
\alias{timssg8.select.merge}
\title{
Select and merge data
}
\description{
timssg8.select.merge selects and merges data from TIMSS G8.
}
\usage{
timssg8.select.merge(folder = getwd(), countries, student = c(), school, 
math.teacher, science.teacher)
}

\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it should not be duplicated.
}
  \item{countries}{
The selected countries, supplied with the abbreviation (e.g., countries=c("AUT", "BGR") or codes (countries=c(40, 100)). If no countries are selected, all are selected.
}
  \item{student}{
The data labels for the selected student variables. 
}
  \item{school}{
The data labels for the selected school variables.
}
  \item{math.teacher}{
The data labels for the selected math teacher variables.
}
  \item{science.teacher}{
The data labels for the selected science teacher variables.
}
}
\value{
timssg8.select.merge returns a data frame with the selected data from TIMSS G8.
}

\seealso{
timssg4.select.merge, pirls.select.merge, pisa.select.merge
}
\examples{
\dontrun{
timss8g <- timssg8.select.merge(folder=getwd(),
                                 countries=c("AUS", "BHR", "ARM", "CHL"),
                                 student =c("BSDGEDUP", "ITSEX", "BSDAGE", "BSBGSLM", "BSDGSLM"), 
                                 school =c("BCBGDAS", "BCDG03"))
}
}
