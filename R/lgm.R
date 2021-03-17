#' Get race probabilities for a vector of names based on a language model
#'
#' @param names a character vector of names for which to retrieve probabilities
#' @param model the type of character-level language model to use. Currently
#' only trigram
#'
#' @examples
#' names <- c("alexander", "wesley", "smith", "johnson")
#' race_from_letters(names)

race_from_letters <- function(names, model = "trigram") {
  if (model == "trigram") {
    lgm <- trigram_language_models
    names <- get_ngrams(names, 3)
  }
  probabilities <- suppressMessages(names %>%
    dplyr::inner_join(lgm)) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(
      whi_prob = prod(whi_cond_prob),
      bla_prob = prod(bla_cond_prob),
      his_prob = prod(his_cond_prob),
      asi_prob = prod(asi_cond_prob),
      oth_prob = prod(oth_cond_prob)
    )
  probabilities <- probabilities %>%
    dplyr::mutate(
      total = whi_prob + bla_prob + his_prob + asi_prob + oth_prob,
    ) %>%
    dplyr::mutate(
      whi_prob = whi_prob / total,
      bla_prob = bla_prob / total,
      his_prob = his_prob / total,
      asi_prob = asi_prob / total,
      oth_prob = oth_prob / total
    ) %>%
    select(name, contains("prob"))
  return(probabilities)
}

#' Converts a character vector into a dataframe with n+1 columns. The first
#' n columns contain elements of trigrams, and the last contains the original
#' character vector
#'
#' @param names a character vector of names for conversion to letter-level
#' trigrams
#' @param n the n-gram size to use. Defaults to 3 (trigram)
#'
#' @import tidytext
names <- c("alexander", "wesley", "smith", "johnson")
n <- 3
get_ngrams <- function(names, n = 3) {
  names <- paste0("<<", names, ">>")
  ngrams <- names %>%
    tidyr::as_tibble() %>%
    tidytext::unnest_character_shingles(
      output = "ngram",
      input = value,
      drop = FALSE,
      n = n
    ) %>%
    dplyr::mutate(
      ngram = toupper(ngram),
      value = substr(value, 3, (str_length(value)-2))
    ) %>%
    tidyr::separate(
      ngram,
      into = c("discard", c(paste0("L", 1:n))),
      sep = ""
    ) %>%
    select(-discard) %>%
    rename("name" = "value")
  return(ngrams)
}
