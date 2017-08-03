\name{intsvy.select.merge}
\alias{intsvy.select.merge}
\title{
Select and merge data
}
\description{
intsvy.select.merge selects and merges data from different international assessment studies. It was developed and it is particularly handy for importing IEA data since original files are organised by instrument, country, grade, etc., in a large number of files. Achievement and weight variabels (all of them) are selected by default.
}
\usage{
intsvy.select.merge(folder = getwd(), countries, student = c(), home, 
    school, teacher, config)
}
\arguments{
  \item{folder}{
Directory path where the data are located. The data could be organised within folders but duplicated files should be avoided.
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
         countries= c("AUS", "AUT", "AZE", "BFR"), 
         student= c("ITSEX", "ASDAGE", "ASBGSMR"), 
         home= c("ASDHEDUP", "ASDHOCCP", "ASDHELA", "ASBHELA"),
         school= c("ACDGDAS", "ACDGCMP", "ACDG03"), 
         config = pirls_conf)
         
pirls <- intsvy.select.merge(folder= getwd(),
         countries= c(36, 40, 31, 957),
         student= c("ITSEX", "ASDAGE", "ASBGSMR"),
         home= c("ASDHEDUP", "ASDHOCCP", "ASDHELA", "ASBHELA"),
         school= c("ACDGDAS", "ACDGCMP", "ACDG03"),
         config = pirls_conf)
                            
timss8g <- intsvy.select.merge(folder= getwd(), 
           countries=c("AUS", "BHR", "ARM", "CHL"), 
           student =c("BSDGEDUP", "ITSEX", "BSDAGE", "BSBGSLM", "BSDGSLM"),
           school=c("BCBGDAS", "BCDG03"), config = timss8_conf)
           
icils <- intsvy.select.merge(folder= getwd(),
         countries=c("AUS", "POL", "SVK"),
         student =c("S_SEX", "S_TLANG", "S_MISEI"), 
         school =c("IP1G02J", "IP1G03A"),
         config = icils_conf)
         
pisa <- pisa.select.merge(folder= getwd(),
        school.file="INT_SCQ12_DEC03.sav", 
        student.file="INT_STU12_DEC03.sav",
        student= c("ST01Q01", "ST04Q01", "ESCS", "PARED"), 
        school = c("CLSIZE", "TCSHORT"), 
        countries = c("HKG", "USA", "SWE", "POL", "PER"))         
}
}
