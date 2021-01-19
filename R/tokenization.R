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
#' @return The vocab as a named integer vector. Names are tokens in vocabulary,
#'   values are integer indices. The casedness of the vocabulary is inferred
#'   and attached as the "is_cased" attribute.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models. This is why the vocabulary
#'   is stored as a named integer vector, and why it starts with index zero.
#'
#' @export
#'
#' @examples
#' # Get path to sample vocabulary included with package.
#' vocab_path <- system.file("extdata", "tiny_vocab.txt", package = "wordpiece")
#' vocab <- load_vocab(vocab_file = vocab_path)
load_vocab <- function(vocab_file) {
  token_list <- readLines(vocab_file)
  token_list <- purrr::map(token_list, function(token) {
    .convert_to_unicode(trimws(token))})
  # The vocab is zero-indexed, and we need to preserve that indexing.
  index_list <- seq_along(token_list) - 1
  names(index_list) <- token_list
  # determine casedness of vocab
  is_cased <- .infer_case_from_vocab(index_list)
  vocab <- .new_wordpiece_vocabulary(index_list, is_cased)
  return(.validate_wordpiece_vocabulary(vocab))
}


# load_or_retrieve_vocab ------------------------------------------------------


#' Load a vocabulary file, or retrieve from cache
#'
#' @inheritParams load_vocab
#' @param use_cache Logical; if TRUE, will attempt to retrieve the vocabulary
#'   from the specified cache location, or, if not found there, will ask to save
#'   the vocabulary as an .rds file.
#' @param cache_dir Character; the path to a cache directory (defaults to
#'   location returned by `get_cache_dir()`).
#'
#' @return The vocab as a named integer vector. Names are tokens in vocabulary,
#'   values are integer indices. The casedness of the vocabulary is inferred
#'   and attached as the "is_cased" attribute.
#'
#'   Note that from the perspective of a neural net, the numeric indices *are*
#'   the tokens, and the mapping from token to index is fixed. If we changed the
#'   indexing, it would break any pre-trained models. This is why the vocabulary
#'   is stored as a named integer vector, and why it starts with index zero.
#'
#' @export
#'
#' @examples
#' # Get path to sample vocabulary included with package.
#' vocab_path <- system.file("extdata", "tiny_vocab.txt", package = "wordpiece")
#' vocab <- load_or_retrieve_vocab(vocab_file = vocab_path, use_cache = FALSE)
load_or_retrieve_vocab <- function(vocab_file,
                                   use_cache = TRUE,
                                   cache_dir = get_cache_dir()) {
  if (use_cache) {
    cache_filepath <- file.path(cache_dir, .make_cache_filename(vocab_file))
    if (file.exists(cache_filepath)) {
      return(readRDS(cache_filepath)) # nocov
    }
  }
  # Guess we have to load the vocab from text file.
  vocab <- load_vocab(vocab_file)

  if (use_cache) { # nocov start
    # ask for permission to write to cache
    if (interactive()) {
      if (isTRUE(utils::askYesNo(paste0("Cache vocabulary at ",
                                        cache_filepath, "?")))) {
        # make sure that the directory exists
        if (!dir.exists(cache_dir)) {
          dir.create(path = cache_dir, recursive = TRUE)
        }
        saveRDS(vocab, cache_filepath)
      }
    }
  } # nocov end
  return(vocab)
}

# wordpiece_tokenize ----------------------------------------------------

#' Tokenize Sequence with Word Pieces
#'
#' Given a single sequence of text and a wordpiece vocabulary, tokenizes the
#' text.
#'
#' @inheritParams .tokenize_word
#' @param text Character scalar; text to tokenize.
#'
#' @return A named integer vector, giving the tokenization of the input
#'   sequence. The integers values are the token ids, and the names are the
#'   tokens.
#' @export
#'
#' @examples
#' # Get path to sample vocabulary included with package.
#' vocab_path <- system.file("extdata", "tiny_vocab.txt", package = "wordpiece")
#' vocab <- load_or_retrieve_vocab(vocab_file = vocab_path, use_cache = FALSE)
#' tokens <- wordpiece_tokenize(
#'   text = "I love tacos!",
#'   vocab = vocab
#' )
wordpiece_tokenize <- function(text,
                               vocab,
                               unk_token = "[UNK]",
                               max_chars = 100) {
  is_cased <- attr(vocab, "is_cased")
  if (!is_cased) {
    text <- tolower(text)
  }

  text <- .convert_to_unicode(text)
  text <- .clean_text(text)
  text <- .tokenize_chinese_chars(text)
  text <- .strip_accents(text)
  text <- .split_on_punc(text)
  text <- purrr::map(.whitespace_tokenize(text),
                     .f = .tokenize_word,
                     vocab = vocab,
                     unk_token = unk_token,
                     max_chars = max_chars)
  text <- unlist(text)
  ids <- vocab[text]
  names(ids) <- text
  return(ids)
}


