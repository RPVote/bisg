#' Gets identifier for geographic unit at the level above the provided unit.
#'
#' This function accept a Census geographic identifier and returns the
#' identifier one level above the provided one, in terms of spatial coverage.
#'
#' @param census_geo A string indicating the geographic level.
#' @return A string indicating the geographic unit one level above the provided
#'  unit.
#'
#' @export swap_census_geography
swap_census_geography <- function(census_geo) {
  if (census_geo == "block") {
    new_geo <- "block group"
  } else if (census_geo == "block group") {
    new_geo <- "tract"
  } else if (census_geo == "tract") {
    new_geo <- "county"
  } else if (census_geo == "county") {
    new_geo <- "state"
  } else {
    stop("Incorrect Census geographic identifier.")
  }
  return(new_geo)
}