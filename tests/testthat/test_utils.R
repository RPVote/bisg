context("Test BISG utility functions.")

test_that("swap_census_geography operates correctly", {
    expect_equal("block group", bisg::swap_census_geography("block"))
    expect_equal("tract", bisg::swap_census_geography("block group"))
    expect_equal("county", bisg::swap_census_geography("tract"))
    expect_equal("state", bisg::swap_census_geography("county"))
    expect_equal("block group", bisg::swap_census_geography("block"))
    expect_error(bisg::swap_census_geography("test"))
})
