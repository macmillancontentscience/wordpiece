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


# %||% ---------------------------------------------------------------------

#' Default value for `NULL`
#'
#' Mostly copied from rlang package.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns `x`.
#' @return Returns `x` if `x` is not NULL; otherwise returns `y`.
#' @keywords internal
#' @name op-null-default
`%||%` <- function(x, y) {
  if (is.null(x)) y else x # nocov
}

# .convert_to_unicode ------------------------------------------------------


#' Convert `text` to Unicode
#'
#' See documentation for `Encoding` for more information.
#' Assumes utf-8 input.
#'
#' @param text character scalar to convert to unicode
#'
#' @return input text, converted to unicode if applicable
#'
#' @keywords internal
.convert_to_unicode <- function(text) {
  if (validUTF8(text)) { # this seems to work for utf-8 and 'bytes' encodings
    Encoding(text) <- "UTF-8"
    return(text)
  } else {
    stop("Unsupported string type.") #nocov
  }
}


# .whitespace_tokenize -----------------------------------------------------


#' Run basic whitespace cleaning and splitting on a piece of text.
#'
#' @param text Character scalar to tokenize.
#'
#' @return Character vector of tokens, split on whitespace.
#' @keywords internal
.whitespace_tokenize <- function(text) {
  return(
    unlist(
      stringi::stri_split_regex(text, "\\s", omit_empty = TRUE)
    )
  )
}


# .strip_accents -----------------------------------------------------------


#' Strip accents from a piece of text.
#'
#' R implementation of BasicTokenizer._run_strip_accents from
#' BERT: tokenization.py.
#'
#' @param text A character scalar, encoded as utf-8.
#'
#' @return text with accents removed.
#'
#' @keywords internal
.strip_accents <- function(text) {
  # Break apart accented characters so that the accents are separate characters
  # from the base letter ("é" -> "e", "´")
  text <- stringi::stri_trans_nfd(text)

  return(
    .apply_to_chars(text,
                    function(char) {
                      # This is the charclass for accents...
                      if(stringi::stri_detect_charclass(char, "\\p{Mn}")) {
                        # If a char is an accent, discard it.
                        return("")
                      }
                      return(char)
                    })
  )
}


# .split_on_punc -----------------------------------------------------------


#' Split text on punctuation.
#'
#' (R implementation of BasicTokenizer._run_split_on_punc from
#' BERT: tokenization.py.)
#'
#' @param text A character scalar, encoded as utf-8.
#'
#' @return The input text as a character vector, split on punctuation
#' characters.
#'
#' @keywords internal
.split_on_punc <- function(text) {
  # this feels icky, but try to break it :-P
  # Put a unique marker around each punctuation char, then split on the
  # marker (since we want the punctuation to be included in split).
  sep_marker <- "a!b"
  output <- .apply_to_chars(text,
                            function(char) {
                              if(.is_punctuation(char)) {
                                return(paste0(sep_marker,
                                              char,
                                              sep_marker))
                              }
                              return(char)
                            })
  return(
    unlist(
      stringi::stri_split_fixed(output, sep_marker, omit_empty = TRUE)
    )
  )
}


# .tokenize_chinese_chars --------------------------------------------------


#' Add whitespace around any CJK character.
#'
#' R implementation of BasicTokenizer._tokenize_chinese_chars from
#' BERT: tokenization.py. This may result in doubled-up spaces,
#' but that's the behavior of the Python code.
#'
#' @param text A character scalar.
#'
#' @return Text with spaces around CJK characters.
#'
#' @keywords internal
.tokenize_chinese_chars <- function(text) {
  return(
    .apply_to_chars(text,
                    function(char) {
                      cp <- utf8ToInt(char)
                      if(.is_chinese_char_cp(cp)) {
                        return(paste0(" ", char, " ")) #nocov
                      }
                      return(char)
                    })
  )
}


# .is_chinese_char_cp ---------------------------------------------------------


#' Check whether cp is the codepoint of a CJK character.
#'
#' R implementation of BasicTokenizer._is_chinese_char from
#' BERT: tokenization.py. From that file:
#'  This defines a "chinese character" as anything in the CJK Unicode block:
#'   https://en.wikipedia.org/wiki/CJK_Unified_Ideographs_(Unicode_block)
#'
#' Note that the CJK Unicode block is NOT all Japanese and Korean characters,
#' despite its name. The modern Korean Hangul alphabet is a different block,
#' as is Japanese Hiragana and Katakana. Those alphabets are used to write
#' space-separated words, so they are not treated specially and are handled
#' like the alphabets of the other languages.
#'
#' @param cp A unicode codepoint, as an integer.
#'
#' @return Logical TRUE if cp is codepoint of a CJK character.
#'
#' @keywords internal
.is_chinese_char_cp <- function(cp) {
  if ((cp >= 0x4E00 & cp <= 0x9FFF) |
      (cp >= 0x3400 & cp <= 0x4DBF) |
      (cp >= 0x20000 & cp <= 0x2A6DF) |
      (cp >= 0x2A700 & cp <= 0x2B73F) |
      (cp >= 0x2B740 & cp <= 0x2B81F) |
      (cp >= 0x2B820 & cp <= 0x2CEAF) |
      (cp >= 0xF900 & cp <= 0xFAFF) |
      (cp >= 0x2F800 & cp <= 0x2FA1F)) {
    return(TRUE)
  }
  return(FALSE)
}


# .clean_text --------------------------------------------------------------


#' Perform invalid character removal and whitespace cleanup on text.
#'
#' (R implementation of BasicTokenizer._clean_text from
#' BERT: tokenization.py.)
#'
#' @param text A character scalar.
#'
#' @return Cleaned up text.
#'
#' @keywords internal
.clean_text <- function(text) {
  return(
    .apply_to_chars(text,
                    function(char) {
                      cp <- utf8ToInt(char)
                      if (cp == 0 | cp == 0xfffd | .is_control(char)) {
                        return("") #nocov
                      } else if (.is_whitespace(char)) {
                        return(" ")
                      }
                      return(char)
                    })
  )
}


# .tokenize_word -----------------------------------------------------------


#' Tokenize a Word
#'
#' Tokenize a single "word" (no whitespace). The word can technically contain
#' punctuation, but in BERT's tokenization, punctuation has been split out by
#' this point.
#'
#' @param word Word to tokenize.
#' @param vocab Named integer vector containing vocabulary words
#' @param unk_token Token to represent unknown words.
#' @param max_chars Maximum length of word recognized.
#'
#' @return Input word as a list of tokens.
#' @keywords internal
.tokenize_word <- function(word, vocab, unk_token = "[UNK]", max_chars = 100) {
  vocab <- names(vocab)
  if (stringi::stri_length(word) > max_chars) {
    return(unk_token)
  }
  if (word %in% vocab) {
    return(word)
  }

  is_bad  <- FALSE
  start <- 1
  sub_tokens <- character(0)
  while (start <= stringi::stri_length(word)) {
    end <- stringi::stri_length(word)

    cur_substr  <- NA_character_
    while (start <= end) {
      sub_str <- substr(word, start, end)   # inclusive on both ends
      if (start > 1) {  # means this substring is a suffix, so add '##'
        sub_str <- paste0("##", sub_str)
      }
      if (sub_str %in% vocab) {
        cur_substr <- sub_str
        break
      }
      end <- end - 1
    }
    if (is.na(cur_substr) ) {
      is_bad <-  TRUE #nocov
      break           #nocov
    }

    sub_tokens <- append(sub_tokens, cur_substr)
    start <- end + 1 # pick up where we left off
  }

  if (is_bad) {
    return(unk_token) #nocov
  }
  return(sub_tokens)
}


# .is_whitespace -----------------------------------------------------------


#' Check whether `char` is a whitespace character
#'
#' R implementation of _is_whitespace from BERT: tokenization.py.
#'
#' "\\t", "\\n", and "\\r" are technically control characters but we treat them
#' as whitespace since they are generally considered as such.
#'
#' @param char A character scalar, comprising a single unicode character.
#'
#' @return TRUE if `char` is a whitespace character.
#'
#' @keywords internal
.is_whitespace <- function(char) {
  # This is a way to check the unicode general category:
  # stringi::stri_detect_charclass(char, "\\p{Zs}")
  if (stringi::stri_length(char) != 1L) { # nocov start
    stop("Parameter passed to .is_whitespace should be exactly one character.")
  } # nocov end


  if (char %in% c(" ", "\t", "\n", "\r")) {
    return(TRUE)
  }
  return(stringi::stri_detect_charclass(char, "\\p{Zs}"))
}

# .is_control --------------------------------------------------------------


#' Check whether `char` is a control character.
#'
#' R implementation of _is_control from BERT: tokenization.py.
#'
#' "\\t", "\\n", and "\\r" are technically control characters but we treat them
#' as whitespace since they are generally considered as such.
#' @param char A character scalar, comprising a single unicode character.
#'
#' @return TRUE if `char` is a control character.
#'
#' @keywords internal
.is_control <- function(char) {
  if (stringi::stri_length(char) != 1L) { # nocov start
    stop("Parameter passed to .is_control should be exactly one character.")
  } # nocov end

  if (char %in% c(" ", "\t", "\n", "\r")) {
    return(FALSE)
  }
  return(stringi::stri_detect_charclass(char, "\\p{C}"))
}

# .is_punctuation ----------------------------------------------------------


#' Check whether `char` is a punctuation character.
#'
#' R implementation of _is_punctuation from BERT: tokenization.py.
#'
#' We treat all non-letter/number ASCII as punctuation.
#' Characters such as "^", "$", and "`" are not in the Unicode
#' Punctuation class but we treat them as punctuation anyway, for
#' consistency.
#' @param char A character scalar, comprising a single unicode character.
#'
#' @return TRUE if `char` is a punctuation character.
#'
#' @keywords internal
.is_punctuation <- function(char) {
  if (stringi::stri_length(char) != 1L) { # nocov start
    stop("Parameter passed to .is_punctuation should be exactly one character.")
  } # nocov end

  # Some punctuation-ish characters aren't actually in the Unicode "punctuation"
  # character class, so explicitly check the codepoint blocks containing them.
  # Also: https://www.regular-expressions.info/unicode.html
  cp <- utf8ToInt(char)
  if (
    (cp >= 33 & cp <= 47) |  # !\"#$%&'()*+,-./
    (cp >= 58 & cp <= 64) |  # :;<=>?@
    (cp >= 91 & cp <= 96) |  # [\\]^_`
    (cp >= 123 & cp <= 126)  # {|}~
  ) {
    return(TRUE)
  }
  return(stringi::stri_detect_charclass(char, "\\p{P}"))
}


# .apply_to_chars ----------------------------------------------------------


#' Apply a function to each character in a string.
#'
#' Utility function for something done a lot in this package.
#'
#' @param text A character scalar to process.
#' @param .f The function to apply to each character. Should return a character
#' scalar, given a single-character input.
#' @param ... Other arguments to pass to .f.
#'
#' @return The character scalar obtained by applying the given function to
#' each character of the input string, and concatenating the results.
#'
#' @keywords internal
.apply_to_chars <- function(text, .f, ...) {
  paste(
    purrr::map_chr(unlist(strsplit(text, "")), .f, ...),
    collapse = ""
  )
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
#' @param vocab The vocabulary as a named integer vector.
#' @return TRUE if the vocabulary is cased, FALSE if uncased.
#'
#' @keywords internal
.infer_case_from_vocab <- function(vocab) {
  is_cased <- any(grepl(pattern = "^[A-Z]", names(vocab)))
  return(is_cased)
}



# .new_wordpiece_vocabulary --------------------------------------------------

#' Constructor for Class wordpiece_vocabulary
#'
#' @param vocab Named integer vector; the "actual" vocabulary.
#' @param is_cased Logical; whether the vocabulary is cased.
#' @return The vocabulary with `is_cased` attached as an attribute, and the
#'   class `wordpiece_vocabulary` applied.
#'
#' @keywords internal
.new_wordpiece_vocabulary <- function(vocab, is_cased) {
  return(structure(vocab,
                   "is_cased" = is_cased,
                   class = c("wordpiece_vocabulary", "integer")))
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
  tokens <- names(vocab)
  if (anyDuplicated(tokens) > 0) {
    stop("Duplicate tokens found in vocabulary.")
  }
  if (any(grepl("\\s", tokens))) {
    stop("Whitespace found in vocabulary tokens.")
  }
  return(vocab)
}

# .make_cache_filename --------------------------------------------------

#' Construct Cache File Name
#'
#' Given the path to a vocabulary file, construct a unique filename using the
#' hash of the path.
#'
#' @inheritParams load_vocab
#' @return A unique filename to use for cacheing the vocabulary.
#'
#' @keywords internal
.make_cache_filename <- function(vocab_file) {
  just_name <- basename(vocab_file)
  dirpath <- normalizePath(dirname(vocab_file))
  path_hash <- digest::digest(dirpath, algo = "xxhash32")
  return(paste(just_name, path_hash, "rds", sep = "."))
}


# get_cache_dir --------------------------------------------------

#' Retrieve Directory for vocabulary Cache
#'
#' @return A unique filename to use for cacheing the vocabulary.
#' @export
get_cache_dir <- function() {
  return(
      getOption("wordpiece.dir") %||%
      rappdirs::user_cache_dir(appname = "wordpiece")
  )
}
