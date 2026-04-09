test_that("utility helpers normalize flags, years, reviewer values, and map aliases", {
  expect_identical(
    normalize_flag(c("Y", " yes ", "N", "maybe later", "", NA_character_)),
    c("Y", "Y", "N", "M", NA_character_, NA_character_)
  )

  expect_identical(
    parse_publication_year(c("smith_1999", "jones-2005", "paper_2001_rev2", "no_year")),
    c(1999L, 2005L, 2001L, NA_integer_)
  )

  expect_identical(
    combine_reviewer_values(c(" Coho ", NA_character_, "Same"), c("Coho", "Chinook", "Same")),
    c("Coho", "Chinook", "Same")
  )

  expect_identical(
    normalize_map_unit_name(c("USA", "UK", "Québec")),
    c("united states of america", "united kingdom", "quebec")
  )
})
