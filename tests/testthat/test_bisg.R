context("Test main BISG function")

test_that("BISG probabilities are computed correctly with custom inputs", {

  # Basic lookups
  surname_counts <- data.frame(
    "surname" = c("x", "y"),
    "w" = c(1, 2),
    "b" = c(2, 3)
  )
  geo_counts <- data.frame(
    "fips" = c("a", "b"),
    "w" = c(1, 3),
    "b" = c(2, 3)
  )
  # Raw data
  data <- data.frame(
    "fips" = c("a"),
    "surname" = c("x")
  )

  # Do BISG manually
  p_r_cond_s <- surname_counts[,c("w","b")]/rowSums(surname_counts[,c("w","b")])
  p_s_cond_g <- t(t(geo_counts[,c("w", "b")])/colSums(geo_counts[,c("w", "b")]))
  probs <- p_r_cond_s[which(surname_counts$surname == "x"),]*
           p_s_cond_g[which(geo_counts$fips == "a"),]
  probs <- probs/rowSums(probs)

  # Now do it with the function
  probs_bisg <- bisg(
    voter_file = data,
    geo_col = "fips",
    geo_counts = geo_counts,
    geo_col_counts = "fips",
    surname_col = "surname",
    surname_counts = surname_counts,
    surname_col_counts = "surname",
    race_cols = c("w", "b")
  )

  expect_equal(probs, probs_bisg[1, c("w", "b")])
})
