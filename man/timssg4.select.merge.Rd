\name{timssg4.select.merge}
\alias{timssg4.select.merge}
\title{
Select and merge data
}
\description{
timssg4.select.merge selects and merges data from TIMSS G4. Achievement and weight variables 
(all of them) are selected by default.
}
\usage{
timssg4.select.merge(folder = getwd(), countries, student = c(), home, school, teacher)
}

\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it 
should not be duplicated.
}
  \item{countries}{
The selected countries, supplied with the abbreviation (e.g., countries=c("AUT", "BGR") or codes 
(countries=c(40, 100)). If no countries are selected, all are selected.
}
  \item{student}{
The data labels for the selected student variables. 
}
  \item{home}{
The data labels for the selected home background variables.
}
  \item{school}{
The data labels for the selected school variables.
}
  \item{teacher}{
The data labels for the selected teacher variables.
}
}

\value{
timssg4.select.merge returns a data frame with the selected data from TIMSS G4.
}

\seealso{
timssg8.select.merge, pirls.select.merge, pisa.select.merge
}
\examples{
\dontrun{
timss4g <- timssg4.select.merge(folder=getwd(),
          countries=c("AUS", "BHR", "ARM", "CHL"),
          student =c("ITSEX", "ASDAGE", "ASBGSLM", "ASDGSLM"),
          home = c("ASDHEDUP", "ASDHENA"),
          school =c("ACDG03", "ACDGENS"))
}
}

