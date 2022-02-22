.onLoad <- function(libname, pkgname) { # nocov start
  .process_wp_vocab.integer <<- memoise::memoise(.process_wp_vocab.integer)
  .infer_case_from_vocab <<- memoise::memoise(.infer_case_from_vocab)
}  # nocov end
