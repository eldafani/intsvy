\name{intsvy.var.label}
\alias{intsvy.var.label}
\title{
Data labels
}
\description{
\code{intsvy.var.labels} prints and saves variable labels and names of participating countries in a text file. The function is called by \code{timssg4.var.label}, \code{timssg8.var.label}, \code{pirls.var.label} and \code{pisa.var.label}.
}
\usage{
intsvy.var.label(folder = getwd(), name = "Variable labels", output = getwd(),
                 config)
}

\arguments{
  \item{folder}{
Directory path where the data files are located. The data could be organized within folders but duplicated files should be avoided. It is assumed that data is in `sav` files. For TIMSS, PIRLS and ICILS studies the data can be downloaded from \code{http://rms.iea-dpc.org/}. 
}
   \item{name}{
Name of the output file.
}
  \item{output}{
Folder where the output file is located.
}
  \item{config}{
Object with configuration of a given study. Should contain the slot `prefixes` with prefixes of filenames with the student, home, school, and teacher data.
}
}

\value{
\code{intsvy.var.label} returns a list with variable labels for the student, home, school, and teacher data (if applied). 
}
\seealso{
timssg4.var.label, timssg8.var.label, pirls.var.label, pisa.var.label
}
\examples{
\dontrun{
intsvy.var.label(folder= getwd(), config = pirls_conf)
intsvy.var.label(folder= getwd(), config = timss8_conf)
intsvy.var.label(folder= getwd(), config = icils_conf)
intsvy.var.label(folder= getwd(), config = piaac_conf)
}
}