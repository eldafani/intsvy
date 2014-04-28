\name{file.names}
\alias{file.names}
\docType{data}
\title{Data file names}
\description{A table with file name abbreviations and associated instruments
}
\usage{data(file.names)}
\format{
  A data frame with 3 columns: test, instrument, and file abbreviation.
  \describe{
    \item{\code{Test}}{The assessment}
    \item{\code{Instrument}}{The specific instrument}
    \item{\code{Abv}}{The abbreviation}
  }
}

\source{
Data file names
}

\examples{
data(file.names)
str(file.names)
}
\keyword{datasets}
