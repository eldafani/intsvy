\name{pisa.select.merge}
\alias{pisa.select.merge}
\title{
Select and merge data
}
\description{
pisa.select.merge selects and merges data from PISA. Achievement and weight variables (all of them) are selected by default. 
}
\usage{
pisa.select.merge(folder=getwd(), student.file, parent.file=c(), school.file=c(), 
countries, student=c(), parent, school)
}
\arguments{
  \item{folder}{
Directory path where the PISA data are located, if all the data are located in the same folder.
}
  \item{student.file}{
Student file name if 'folder' is provided, otherwise full path name of student dataset (required argument).
}
  \item{parent.file}{
Parent file name if 'folder' is provided, otherwise full path name of parent dataset.
}
  \item{school.file}{
School file name if 'folder' is provided, otherwise full path name of school dataset.
}
  \item{countries}{
The selected countries, supplied with the abbreviation (e.g., countries=c("DEU", "NOR") or codes. If no countries are selected, all are selected.
}
  \item{student}{
The data labels for the selected student variables. 
}
  \item{parent}{
The data labels for the selected parental variables.
}
  \item{school}{
The data labels for the selected school variables.
}
}

\value{
pisa.select.merge returns a data frame with the selected data from PISA.
}

\seealso{
timssg4.select.merge, timssg8.select.merge, pirls.select.merge
}
\examples{
\dontrun{
pisa <- pisa.select.merge(folder=getwd(),
        school.file="INT_SCQ12_DEC03.sav",
        student.file="INT_STU12_DEC03.sav",
        parent.file="INT_PAQ12_DEC03.sav",
        student= c("IMMIG", "ESCS", "ST04Q01", "ST61Q04", "ST62Q01", "ST08Q01"),
        parent = c("PARINVOL", "PARSUPP"),
        school = c("STRATIO", "SCHAUTON", "CLSIZE"),
        countries = c("HKG", "USA", "SWE", "POL", "PER"))
}
}

