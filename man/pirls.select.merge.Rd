\name{pirls.select.merge}
\alias{pirls.select.merge}
\title{
Select and merge data
}
\description{
pirls.select.merge selects and merges data from PIRLS. Achievement and weight variabels (all of them) 
are selected by default.
}
\usage{
pirls.select.merge(folder = getwd(), countries, student = c(), 
    home, school, teacher)
}
\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organized within folders but it should 
not be duplicated.
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
The data labels for the selected teacher data.
}
}

\value{
pirls.select.merge returns a data frame with the selected data from PIRLS.
}

\seealso{
timssg4.select.merge, timssg8.select.merge, pisa.select.merge
}
\examples{
\dontrun{
pirls <- pirls.select.merge(folder= getwd(),
                countries= c(36, 40, 31, 957),
                student= c("ITSEX", "ASDAGE", "ASBGSMR"),
                home= c("ASDHEDUP", "ASDHOCCP", "ASDHELA", "ASBHELA"),
                school= c("ACDGDAS", "ACDGCMP", "ACDG03"))
}
}
