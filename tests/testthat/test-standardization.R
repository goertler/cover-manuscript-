test_that("species standardization preserves specific and unresolved mappings", {
  species <- standardize_species_for_text("spring-run Chinook; coho")

  expect_setequal(
    species,
    c("Oncorhynchus tshawytscha (spring-run)", "Oncorhynchus kisutch")
  )
  expect_false("Oncorhynchus tshawytscha" %in% species)

  expect_setequal(
    standardize_species_for_text("salmon and trout"),
    c("unresolved salmon", "unresolved trout")
  )
})

test_that("lifestage standardization maps controlled vocabulary values", {
  expect_identical(
    standardize_lifestage_for_text("egg to fry"),
    c("egg", "fry")
  )

  expect_identical(
    standardize_lifestage_for_text("juvenile rearing"),
    "juvenile_unspecified"
  )

  expect_identical(
    standardize_lifestage_for_text("spawning adults"),
    "adult"
  )
})

test_that("location standardization handles explicit and fallback mappings", {
  coyote_record <- standardize_location_record(
    id = 1L,
    full_text_file = "example_2001",
    location_raw_la = "Coyote Creek, Stevens Creek, Guadalupe River",
    location_raw_pg = "three creeks watershed"
  )

  expect_equal(nrow(coyote_record), 3L)
  expect_setequal(
    coyote_record$location_standardized,
    c(
      "Coyote Creek, California, USA",
      "Stevens Creek, California, USA",
      "Guadalupe River, California, USA"
    )
  )

  fallback_record <- standardize_location_record(
    id = 2L,
    full_text_file = "example_2002",
    location_raw_la = "Unknown Tributary",
    location_raw_pg = NA_character_
  )

  expect_identical(fallback_record$location_type, "multi_site")
  expect_identical(fallback_record$location_standardized, "Unknown Tributary")
})
