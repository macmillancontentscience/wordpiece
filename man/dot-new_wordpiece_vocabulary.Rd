% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vocab.R
\name{.new_wordpiece_vocabulary}
\alias{.new_wordpiece_vocabulary}
\title{Constructor for Class wordpiece_vocabulary}
\usage{
.new_wordpiece_vocabulary(vocab, is_cased)
}
\arguments{
\item{vocab}{Character vector of tokens.}

\item{is_cased}{Logical; whether the vocabulary is cased.}
}
\value{
The vocabulary with \code{is_cased} attached as an attribute, and the
class \code{wordpiece_vocabulary} applied.
}
\description{
Constructor for Class wordpiece_vocabulary
}
\keyword{internal}
