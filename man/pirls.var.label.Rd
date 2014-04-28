\name{pirls.var.label}
\alias{pirls.var.label}
\title{
Data labels
}
\description{
pirls.var.labels prints and saves variable labels and names of participating countries in a text file
}
\usage{
pirls.var.label(folder = getwd(), name = "Variable labels", output = getwd())
}

\arguments{
  \item{folder}{
Directory path where the PIRLS data are located. The data could be organized within folders but it should not be duplicated.
}
  \item{name}{
Name of output file.
}
  \item{output}{
Folder where output file is located.
}
}

\value{
pirls.var.label returns a list with variable labels for the student, home, school, and teacher data. 
}


\seealso{
timssg4.var.label, timssg8.var.label, pisa.var.label
}
\examples{
\dontrun{
pirls.var.label(folder= getwd())
}
}
