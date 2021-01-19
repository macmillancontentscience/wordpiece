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


test_that(".whitespace_tokenize splits a string on whitespace", {
  test_string <- " some\ttext\nwith whitespace "
  test_result <- .whitespace_tokenize(test_string)
  expected_result <- c("some", "text", "with", "whitespace")
  testthat::expect_identical(test_result, expected_result)
})



test_that(".strip_accents replaces accented chars with nearest equivalents", {
  test_string <- "fa\u00E7ile"
  test_result <- .strip_accents(test_string)
  expected_result <- "facile"
  testthat::expect_identical(test_result, expected_result)
})


test_that(".split_on_punc splits a string before and after punctuation chars", {
  test_string <- "stop! don't touch that."
  test_result <- .split_on_punc(test_string)
  expected_result <- c("stop", "!", " don", "'", "t touch that", ".")
  testthat::expect_identical(test_result, expected_result)

  test_string <- "!"
  test_result <- .split_on_punc(test_string)
  expected_result <- c("!")
  testthat::expect_identical(test_result, expected_result)
})




test_that(".is_whitespace correctly classifies characters", {
  # tests from BERT: tokenization_test.py
  testthat::expect_true(.is_whitespace(" "))
  testthat::expect_true(.is_whitespace("\t"))
  testthat::expect_true(.is_whitespace("\r"))
  testthat::expect_true(.is_whitespace("\n"))
  testthat::expect_true(.is_whitespace("\u00A0")) # non-breaking space

  testthat::expect_false(.is_whitespace("A"))
  testthat::expect_false(.is_whitespace("-"))
})


test_that(".is_control correctly classifies characters", {
  # tests from BERT: tokenization_test.py
  testthat::expect_true(.is_control("\u0005")) # 'Enquiry' control character

  testthat::expect_false(.is_control("A"))
  testthat::expect_false(.is_control(" "))
  testthat::expect_false(.is_control("\t"))
  testthat::expect_false(.is_control("\r"))
})

test_that(".is_chinese_char_cp correctly classifies character codepoints", {
  testthat::expect_true(.is_chinese_char_cp(utf8ToInt("力")))

  testthat::expect_false(.is_chinese_char_cp(utf8ToInt("A")))
})

test_that(".is_punctuation correctly classifies characters", {
  # tests from BERT: tokenization_test.py
  testthat::expect_true(.is_punctuation("-"))
  testthat::expect_true(.is_punctuation("$"))
  testthat::expect_true(.is_punctuation("`"))
  testthat::expect_true(.is_punctuation("."))

  testthat::expect_false(.is_punctuation("A"))
  testthat::expect_false(.is_punctuation(" "))
})

test_that("bad vocabularies are flagged", {
  # badvocabfile <- "tests/testthat/badvocab_spaces.txt"
  badvocabfile <- "badvocab_spaces.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Whitespace")
  badvocabfile <- "badvocab_dupes.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Duplicate")
  badvocabfile <- "empty_vocab.txt"
  testthat::expect_error(load_or_retrieve_vocab(badvocabfile), "Empty")
})

test_that("wordpiece_tokenize works as expected.", {
  # vocab <- load_or_retrieve_vocab(vocab_file = "tests/testthat/vocab.txt")
  vocab <- load_or_retrieve_vocab(vocab_file = "vocab.txt")
  testthat::expect_false(attr(vocab, "is_cased"))
  testthat::expect_is(vocab, "wordpiece_vocabulary")

  text <- "I love tacos!"
  expected_result <- c(2, 3, 4, 1)
  names(expected_result) <- c("i", "love", "tacos", "!")
  test_result <- wordpiece_tokenize(text = text, vocab = vocab)
  testthat::expect_identical(test_result, expected_result)

  text <- "I love apples!"
  expected_result <- c(2, 3, 5, 6, 1)
  names(expected_result) <- c("i", "love", "app", "##les", "!")
  test_result <- wordpiece_tokenize(text = text, vocab = vocab)
  testthat::expect_identical(test_result, expected_result)

  text <- "I love oranges!"
  expected_result <- c(2, 3, 7, 1)
  names(expected_result) <- c("i", "love", "[UNK]", "!")
  test_result <- wordpiece_tokenize(text = text,
                                    vocab = vocab,
                                    max_chars = 5)
  testthat::expect_identical(test_result, expected_result)
})
