\name{pisa.var.label}
\alias{pisa.var.label}
\title{
Data labels
}
\description{
pisa.var.labels prints and saves variable labels and names of participating countries in a text file
}
\usage{
pisa.var.label(folder=getwd(), student.file, parent.file=c(), school.file=c(), 
name="Variable labels", output=getwd())
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
   \item{name}{
Name of output file.
}
  \item{output}{
Folder where output file is located.
}
}

\value{
pisa.var.label returns a list with variable labels for the student, parent, and school data. 
}


\seealso{
timssg4.var.label, timssg8.var.label, pirls.var.label
}
\examples{
\dontrun{
pisa.var.label(folder=getwd(), school.file="INT_SCQ12_DEC03.sav", 
student.file="INT_STU12_DEC03.sav", parent.file="INT_PAQ12_DEC03.sav")
}
}
