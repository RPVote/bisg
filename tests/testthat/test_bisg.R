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
    "fips" = c("a", "b"),
    "surname" = c("x", "z")
  )

  # Do BISG manually for first row of data
  p_r_cond_s <- surname_counts[,c("w","b")]/rowSums(surname_counts[,c("w","b")])
  p_g_cond_r <- t(t(geo_counts[,c("w", "b")])/colSums(geo_counts[,c("w", "b")]))
  prob1 <- p_r_cond_s[which(surname_counts$surname == "x"),]*
    p_g_cond_r[which(geo_counts$fips == "a"),]
  prob1 <- as.numeric(prob1/rowSums(prob1))

  # Now for second row with NA
  prob2 <- colMeans(p_r_cond_s)*p_g_cond_r[which(geo_counts$fips == "b"),]
  prob2 <- as.numeric(prob2/sum(prob2))


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

  expect_equal(prob1, as.numeric(probs_bisg[1, c("w", "b")]))
  expect_equal(prob2, as.numeric(probs_bisg[2, c("w", "b")]))
})
