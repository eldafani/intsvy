\name{timssg4.var.label}
\alias{timssg4.var.label}
\title{
Data labels
}
\description{
timssg4.var.labels prints and saves variable labels and names of participating countries in a text file
}
\usage{
timssg4.var.label(folder = getwd(), name = "Variable labels", output = getwd())
}

\arguments{
  \item{folder}{
Directory path where the TIMSS G4 data are located. The data could be organized within folders but it should not be duplicated.
}
   \item{name}{
Name of output file.
}
  \item{output}{
Folder where output file is located.
}
}

\value{
timssg4.var.label returns a list with variable labels for the student, home, school, and teacher data. 
}
\seealso{
timssg8.var.label, pirls.var.label, pisa.var.label
}
\examples{
\dontrun{
timssg4.var.label(folder= getwd())
}
}
