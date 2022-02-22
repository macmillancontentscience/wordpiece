# Copyright 2021 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# load_vocab --------------------------------------------------------------


#' Load a vocabulary file
#'
#' @param vocab_file path to vocabulary file. File is assumed to be a text file,
#'   with one token per line, with the line number corresponding to the index of
#'   that token in the vocabulary.
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens,
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing (the order of the tokens), it would break any pre-trained models.
#'
#' @export
#' @examples
#' # Get path to sample vocabulary included with package.
#' vocab_path <- system.file("extdata", "tiny_vocab.txt", package = "wordpiece")
#' vocab <- load_vocab(vocab_file = vocab_path)
load_vocab <- function(vocab_file) {
  token_list <- readLines(vocab_file)
  return(prepare_vocab(token_list))
}

#' Format a Token List as a Vocabulary
#'
#' We use a special named integer vector with class wordpiece_vocabulary to
#' provide information about tokens used in \code{\link{wordpiece_tokenize}}.
#' This function takes a character vector of tokens and puts it into that
#' format.
#'
#' @param token_list A character vector of tokens.
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens,
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing (the order of the tokens), it would break any pre-trained models.
#' @export
#' @examples
#' my_vocab <- prepare_vocab(c("some", "example", "tokens"))
#' class(my_vocab)
#' attr(my_vocab, "is_cased")
prepare_vocab <- function(token_list) {
  token_list <- piecemaker::validate_utf8(trimws(token_list))

  is_cased <- .infer_case_from_vocab(token_list)
  vocab_all <- .new_wordpiece_vocabulary(
    vocab = token_list,
    is_cased = is_cased
  )
  return(.validate_wordpiece_vocabulary(vocab = vocab_all))
}


# load_or_retrieve_vocab ------------------------------------------------------


#' Load a vocabulary file, or retrieve from cache
#'
#' @inheritParams load_vocab
#'
#' @return The vocab as a character vector of tokens. The casedness of the
#'   vocabulary is inferred and attached as the "is_cased" attribute. The
#'   vocabulary indices are taken to be the positions of the tokens,
#'   *starting at zero* for historical consistency.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing (the order of the tokens), it would break any pre-trained models.
#'
#' @export
load_or_retrieve_vocab <- function(vocab_file) {
  return( # nocov start
    dlr::read_or_cache(
      source_path = vocab_file,
      appname = "wordpiece",
      process_f = load_vocab
    )
  ) # nocov end
}


# .infer_case_from_vocab --------------------------------------------------

#' Determine Vocabulary Casedness
#'
#' Determine whether or not a wordpiece vocabulary is case-sensitive.
#'
#' If none of the tokens in the vocabulary start with a capital letter, it will
#' be assumed to be uncased. Note that tokens like "\\[CLS\\]" contain uppercase
#' letters, but don't start with uppercase letters.
#'
#' @param vocab The vocabulary as a character vector.
#' @return TRUE if the vocabulary is cased, FALSE if uncased.
#'
#' @keywords internal
.infer_case_from_vocab <- function(vocab) {
  is_cased <- any(grepl(pattern = "^[A-Z]", vocab))
  return(is_cased)
}



# .new_wordpiece_vocabulary --------------------------------------------------

#' Constructor for Class wordpiece_vocabulary
#'
#' @param vocab Character vector of tokens.
#' @param is_cased Logical; whether the vocabulary is cased.
#' @return The vocabulary with `is_cased` attached as an attribute, and the
#'   class `wordpiece_vocabulary` applied.
#'
#' @keywords internal
.new_wordpiece_vocabulary <- function(vocab, is_cased) {
  return(
    structure(
      vocab,
      "is_cased" = is_cased,
      class = c("wordpiece_vocabulary", "character")
    )
  )
}



# .validate_wordpiece_vocabulary ----------------------------------------------

#' Validator for Objects of Class wordpiece_vocabulary
#'
#' @param vocab wordpiece_vocabulary object to validate
#' @return \code{vocab} if the object passes the checks. Otherwise, abort with
#'   message.
#'
#' @keywords internal
.validate_wordpiece_vocabulary <- function(vocab) {
  if (length(vocab) == 0) {
    stop("Empty vocabulary.")
  }
  if (anyDuplicated(vocab) > 0) {
    stop("Duplicate tokens found in vocabulary.")
  }
  if (any(grepl("\\s", vocab))) {
    stop("Whitespace found in vocabulary tokens.")
  }
  return(vocab)
}
