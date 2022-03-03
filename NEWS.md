# wordpiece 2.1.3

* Implemented various speed improvements, improving speed and memory usage by over 1000x. (#27, @jonathanbratt)
* Removed purrr dependency. (#30, @jonthegeek)

# wordpiece 2.0.0

* Refactored `wordpiece_tokenize` to accept a character vector with length > 1. This makes the package more usable within a workflow, but will break scripts that used the previous version (the output is now a list of character vectors, instead of a single character vector). (@jonthegeek)
* Added a pair of default vocabularies via the {wordpiece.data} package. (@jonthegeek)

# wordpiece 1.0.0

* Initial CRAN release. (@jonathanbratt)

# wordpiece 0.0.6

* Oops, make tiny sample vocab compatible with RBERT (@jonathanbratt)

# wordpiece 0.0.5

* Added vocabulary class + validation. (#9, #10, @jonathanbratt)

# wordpiece 0.0.4

* Added basic usage vignette.

# wordpiece 0.0.3

* Enabled cache option to speed up vocabulary loading.

# wordpiece 0.0.1

* Added a `NEWS.md` file to track changes to the package.
