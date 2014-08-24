\name{piaacReplicationScheme}
\alias{piaacReplicationScheme}
\docType{data}
\title{PIAAC country names and replication schemes
}
\description{Contains PIAAC participating countries and their replication schemes 
}
\usage{data(piaacReplicationScheme)}
\format{
  A data frame with 23 observations on the following 4 variables.
  \describe{
    \item{\code{CNTRYID}}{Country name}
    \item{\code{RepMethod}}{Replication method}
    \item{\code{NoReplications}}{Number of replications}
    \item{\code{c}}{Constant for scaling the variance due to replication scheme}
  }
}

\source{PIAAC Technical Report
}

\examples{
data(piaacReplicationScheme)
str(piaacReplicationScheme) 
}
\keyword{datasets}
