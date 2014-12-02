\name{adj.input}
\alias{adj.input}
\title{Utility function}
\description{Utility function}
\usage{
adj.input(x, threshold = 0.5)
}
\arguments{
  \item{x}{
A list element with labels
}
  \item{threshold}{
User defined threshold
}
}
\details{
A utility function for PISA data importation
}
\value{
adj.input returns data with corrected labels
}
\author{
Martin Elff
}

\seealso{
adj.measlev, is.vlabeled
}

\examples{
\dontrun{
## Input of adj.measlev
adj.input(data)
}
}
\keyword{internal}
