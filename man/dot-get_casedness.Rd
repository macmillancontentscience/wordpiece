% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenization.R
\name{.get_casedness}
\alias{.get_casedness}
\alias{.get_casedness.default}
\alias{.get_casedness.wordpiece_vocabulary}
\alias{.get_casedness.character}
\title{Determine Casedness of Vocabulary}
\usage{
.get_casedness(v)

\method{.get_casedness}{default}(v)

\method{.get_casedness}{wordpiece_vocabulary}(v)

\method{.get_casedness}{character}(v)
}
\arguments{
\item{v}{An object of class \code{wordpiece_vocabulary}, or a character vector.}
}
\value{
TRUE if the vocabulary is case-sensitive, FALSE otherwise.
}
\description{
Determine Casedness of Vocabulary
}
\keyword{internal}
