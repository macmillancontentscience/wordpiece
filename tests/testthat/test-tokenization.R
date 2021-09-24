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

test_that("wordpiece_tokenize works as expected.", {
  # vocab <- load_vocab(vocab_file = "tests/testthat/vocab.txt")
  vocab <- load_vocab(vocab_file = "vocab.txt")

  text <- "I love tacos!"
  expected_result <- c(2L, 3L, 4L, 1L)
  names(expected_result) <- c("i", "love", "tacos", "!")
  expected_result <- list(expected_result)
  test_result <- wordpiece_tokenize(text = text, vocab = vocab)
  testthat::expect_identical(test_result, expected_result)

  text <- "I love apples!"
  expected_result <- c(2L, 3L, 5L, 6L, 1L)
  names(expected_result) <- c("i", "love", "app", "##les", "!")
  expected_result <- list(expected_result)
  test_result <- wordpiece_tokenize(text = text, vocab = vocab)
  testthat::expect_identical(test_result, expected_result)

  text <- "I love oranges!"
  expected_result <- c(2L, 3L, 7L, 1L)
  names(expected_result) <- c("i", "love", "[UNK]", "!")
  expected_result <- list(expected_result)
  test_result <- wordpiece_tokenize(
    text = text,
    vocab = vocab,
    max_chars = 5
  )
  testthat::expect_identical(test_result, expected_result)

  text <- c(
    "I love tacos!",
    "I love apples!",
    "I love oranges!"
  )
  expected_result1 <- c(2L, 3L, 4L, 1L)
  names(expected_result1) <- c("i", "love", "tacos", "!")
  expected_result2 <- c(2L, 3L, 5L, 6L, 1L)
  names(expected_result2) <- c("i", "love", "app", "##les", "!")
  expected_result3 <- c(2L, 3L, 7L, 1L)
  names(expected_result3) <- c("i", "love", "[UNK]", "!")
  expected_result <- list(expected_result1, expected_result2, expected_result3)
  test_result <- wordpiece_tokenize(
    text = text,
    vocab = vocab
  )
  testthat::expect_identical(test_result, expected_result)
})
