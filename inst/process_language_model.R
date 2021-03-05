library(tidyverse)

# Process white cond probs
whi_model <- readr::read_csv("inst/whi_cond_probs.csv") %>%
  dplyr::rename("L1" = "X1",
                "L2" = "X2") %>%
  tidyr::pivot_longer(
    -one_of("L1", "L2"),
    names_to = "L3",
    values_to = "whi_cond_prob"
  )

# Process black cond probs
bla_pmodel <- read_csv("inst/bla_cond_probs.csv") %>%
  dplyr::rename("L1" = "X1",
         "L2" = "X2") %>%
  tidyr::pivot_longer(
    -one_of("L1", "L2"),
    names_to = "L3",
    values_to = "bla_cond_prob"
  )

# Process hispanic cond probs
his_model <- read_csv("inst/his_cond_probs.csv") %>%
  dplyr::rename("L1" = "X1",
                "L2" = "X2") %>%
  tidyr::pivot_longer(
    -one_of("L1", "L2"),
    names_to = "L3",
    values_to = "his_cond_prob"
  )

# Process asian cond probs
asi_model <- read_csv("inst/asi_cond_probs.csv") %>%
  dplyr::rename("L1" = "X1",
                "L2" = "X2") %>%
  tidyr::pivot_longer(
    -one_of("L1", "L2"),
    names_to = "L3",
    values_to = "asi_cond_prob"
  )

# Process other cond probs
oth_model <- read_csv("inst/oth_cond_probs.csv") %>%
  dplyr::rename("L1" = "X1",
                "L2" = "X2") %>%
  tidyr::pivot_longer(
    -one_of("L1", "L2"),
    names_to = "L3",
    values_to = "oth_cond_prob"
  )

trigram_language_models <- whi_model %>%
  inner_join(bla_pmodel) %>%
  inner_join(his_model) %>%
  inner_join(asi_model) %>%
  inner_join(oth_model)

usethis::use_data(trigram_language_models, overwrite = TRUE)
