% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenization.R
\name{wordpiece_tokenize}
\alias{wordpiece_tokenize}
\title{Tokenize Sequence with Word Pieces}
\usage{
wordpiece_tokenize(
  text,
  vocab = wordpiece_vocab(),
  unk_token = "[UNK]",
  max_chars = 100
)
}
\arguments{
\item{text}{Character; text to tokenize.}

\item{vocab}{Character vector of vocabulary tokens. The tokens are assumed to
be in order of index, with the first index taken as zero to be compatible
with Python implementations.}

\item{unk_token}{Token to represent unknown words.}

\item{max_chars}{Maximum length of word recognized.}
}
\value{
A list of named integer vectors, giving the tokenization of the input
sequences. The integer values are the token ids, and the names are the
tokens.
}
\description{
Given a sequence of text and a wordpiece vocabulary, tokenizes the text.
}
\examples{
tokens <- wordpiece_tokenize(
  text = c(
    "I love tacos!",
    "I also kinda like apples."
  )
)
}
