\name{adj.measlev}
\alias{adj.measlev}
\title{
Utility function 
}
\description{
Removes missing data in as.data.frame(spss.system.file())
}
\usage{
adj.measlev(x, threshold = 0.5)
}
\arguments{
  \item{x}{
The data.set
}
  \item{threshold}{
User defined threshold
}
}
\details{
A utility function for PISA data importation
}
\value{
adj.measlev returns a data.set with corrected measurement levels for as.data.frame
}
\author{
Martin Elff
}

\seealso{
adj.input, is.vlabeled
}
\examples{\dontrun{
## Input of adj.measlev
adj.measlev(mydataset)
}
}
\keyword{internal}
