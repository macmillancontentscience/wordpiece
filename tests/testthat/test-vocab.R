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

test_that("bad vocabularies are flagged", {
  # badvocabfile <- "tests/testthat/badvocab_spaces.txt"
  badvocabfile <- "badvocab_spaces.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Whitespace")
  badvocabfile <- "badvocab_dupes.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Duplicate")
  badvocabfile <- "empty_vocab.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Empty")
})

test_that("good vocabularies load as expected", {
  # vocab_file <- "tests/testthat/vocab.txt"
  vocab_file <- "vocab.txt"
  vocab <- load_vocab(vocab_file = vocab_file)
  testthat::expect_false(attr(vocab, "is_cased"))
  testthat::expect_s3_class(vocab, "wordpiece_vocabulary")
  testthat::expect_snapshot(vocab)
})

test_that("various vocabulary formats work", {
  # vocab_file <- "tests/testthat/vocab.txt"
  vocab_file <- "vocab.txt"
  vocab <- load_vocab(vocab_file = vocab_file)

  # just a mockup
  vocab_char <- structure(
    names(vocab),
    "is_cased" = FALSE,
    class = c("wordpiece_vocabulary", "character")
  )
  char_vec <- names(vocab)

  text <- "I love tacos!"
  expected_result <- c(2L, 3L, 4L, 1L)
  names(expected_result) <- c("i", "love", "tacos", "!")
  expected_result <- list(expected_result)

  test_result <- wordpiece_tokenize(text = text, vocab = vocab_char)
  testthat::expect_identical(test_result, expected_result)

  test_result <- wordpiece_tokenize(text = text, vocab = char_vec)
  testthat::expect_identical(test_result, expected_result)
})
