\name{timssg8.var.label}
\alias{timssg8.var.label}
\title{
Data labels
}
\description{
timssg8.var.labels prints and saves variable labels and names of participating countries in a text file
}
\usage{
timssg8.var.label(folder = getwd(), name = "Variable labels", output = getwd())
}
\arguments{
  \item{folder}{
Directory path where the TIMSS G8 data are located. The data could be organized within folders but it should not be duplicated.
}
  \item{name}{
Name of output file.
}
  \item{output}{
Folder where output file is located.
}
}

\value{
timssg8.var.label returns a list with variable labels for the student, home, school, and teacher data. 
}


\seealso{
timssg4.var.label, pirls.var.label, pisa.var.label
}
\examples{
\dontrun{
timssg8.var.label(folder= getwd())
}
}
