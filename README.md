
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wordpiece

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of wordpiece is to allow for easy text tokenization using a
wordpiece vocabulary.

## Installation

You can install the released version of wordpiece from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wordpiece")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macmillancontentscience/wordpiece")
```

# Examples

This package can be used to tokenize text for modeling. A common usecase
would be to tokenize all text in a data.frame or other tibble.

``` r
library(wordpiece)
library(dplyr, warn.conflicts = FALSE)
df_tokenized <- tibble(
  text = c(
    "I like tacos.",
    "I like apples with cheese.",
    "The unaffable coder wrote incorrect examples."
  )
) %>% 
  mutate(
    tokens = wordpiece_tokenize(text)
  )

df_tokenized
#> # A tibble: 3 x 2
#>   text                                          tokens    
#>   <chr>                                         <list>    
#> 1 I like tacos.                                 <dbl [5]> 
#> 2 I like apples with cheese.                    <dbl [6]> 
#> 3 The unaffable coder wrote incorrect examples. <dbl [10]>
df_tokenized$tokens[[1]]
#>     i  like    ta ##cos     . 
#>  1045  2066 11937 13186  1012
```

## Code of Conduct

Please note that the wordpiece project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Disclaimer

This is not an officially supported Macmillan Learning product.

## Contact information

Questions or comments should be directed to Jonathan Bratt
(<jonathan.bratt@macmillan.com>) and Jon Harmon
(<jonthegeek@gmail.com>).
