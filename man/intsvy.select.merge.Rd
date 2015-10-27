\name{intsvy.select.merge}
\alias{intsvy.select.merge}
\title{
Select and merge data
}
\description{
intsvy.select.merge selects and merges data from different  International Association for the Evaluation of Educational Achievement (IEA) studies. Achievement and weight variabels (all of them) are selected by default.
}
\usage{
intsvy.select.merge(folder = getwd(), countries, student = c(), home, 
    school, teacher, use.labels=TRUE, config)
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
  \item{use.labels}{
logical: convert variables with value labels into R factors with those levels? 
}
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
intsvy.select.merge returns a data frame with the selected data from study defined in config file.
}

\seealso{
timssg4.select.merge, timssg8.select.merge, pisa.select.merge
}
\examples{
\dontrun{
pirls <- intsvy.select.merge(folder= getwd(),
                            countries= c(36, 40, 31, 957),
                            student= c("ITSEX", "ASDAGE", "ASBGSMR"),
                            home= c("ASDHEDUP", "ASDHOCCP", "ASDHELA", "ASBHELA"),
                            school= c("ACDGDAS", "ACDGCMP", "ACDG03"),
                            config = intsvy:::pirls_conf)
}
}