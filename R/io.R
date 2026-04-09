rename_review_columns <- function(data, reviewer_label) {
  names(data) <- clean_names(names(data))

  rename_map <- c(
    what_species_is_the_focus = "species_raw",
    what_lifestage_is_the_focus = "lifestage_raw",
    where_is_the_study_located = "location_raw",
    what_kinds_of_cover_are_described = "cover_raw",
    how_is_cover_quantified = "cover_quantified_raw",
    how_is_habitat_use_by_fish_measured_e_g_field_method_and_metric = "habitat_use_raw",
    how_does_cover_influence_juvenile_rearing = "cover_influence_raw",
    what_are_the_primary_drivers_of_juvenile_rearing_presence_recovery = "drivers_raw",
    should_this_article_be_included_in_the_systematic_review = "include_raw",
    should_this_article_be_included_in_the_final_analysis = "include_raw",
    why_use_categories_from_categories_doc = "reason_raw",
    why_use_categories_from_sharepoint = "reason_raw",
    updated_why = "updated_reason_raw",
    new = "is_new"
  )

  matching_names <- intersect(names(rename_map), names(data))
  names(data)[match(matching_names, names(data))] <- unname(rename_map[matching_names])

  required_columns <- c(
    "reviewer", "id", "full_text_file", "doi", "title", "species_raw",
    "lifestage_raw", "location_raw", "cover_raw", "cover_quantified_raw",
    "habitat_use_raw", "cover_influence_raw", "drivers_raw", "include_raw",
    "reason_raw", "updated_reason_raw", "is_new"
  )

  for (column in setdiff(required_columns, names(data))) {
    data[[column]] <- NA
  }

  data <- data[, required_columns]
  data$id <- as.integer(data$id)
  data$reviewer_source <- reviewer_label

  character_columns <- names(data)[vapply(data, is.character, logical(1))]
  data[character_columns] <- lapply(data[character_columns], normalize_string)
  data$include_flag <- normalize_flag(data$include_raw)

  data
}

read_review_file <- function(path, reviewer_label, encoding) {
  readr::read_csv(
    project_path(path),
    locale = readr::locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    rename_review_columns(reviewer_label = reviewer_label)
}

load_review_data <- function() {
  list(
    la = read_review_file("data-raw/full_text_review_LA.csv", reviewer_label = "LA", encoding = "ISO-8859-1"),
    pg = read_review_file("data-raw/full_text_review_PG.csv", reviewer_label = "PG", encoding = "UTF-8")
  )
}

build_reviewer_comparison <- function(la, pg) {
  la_side <- la |>
    dplyr::rename_with(~ paste0(.x, "_la"), -id)

  pg_side <- pg |>
    dplyr::rename_with(~ paste0(.x, "_pg"), -id)

  dplyr::full_join(la_side, pg_side, by = "id") |>
    dplyr::mutate(
      full_text_file = first_non_missing(full_text_file_la, full_text_file_pg),
      doi = first_non_missing(doi_la, doi_pg),
      title = first_non_missing(title_la, title_pg),
      publication_year = parse_publication_year(full_text_file),
      doi_match = field_match(doi_la, doi_pg),
      title_match = field_match(title_la, title_pg),
      full_text_file_match = field_match(full_text_file_la, full_text_file_pg),
      species_raw_combined = combine_reviewer_values(species_raw_la, species_raw_pg),
      lifestage_raw_combined = combine_reviewer_values(lifestage_raw_la, lifestage_raw_pg),
      location_raw_combined = combine_reviewer_values(location_raw_la, location_raw_pg)
    ) |>
    dplyr::arrange(id)
}

build_included_papers <- function(reviewer_comparison) {
  reviewer_comparison |>
    dplyr::filter(include_flag_pg == "Y") |>
    dplyr::transmute(
      id,
      full_text_file,
      publication_year,
      doi,
      title,
      include_flag_pg,
      include_raw_pg,
      reason_raw_pg,
      include_flag_la,
      include_raw_la,
      reason_raw_la,
      updated_reason_raw_la,
      species_raw_la,
      species_raw_pg,
      species_raw_combined,
      lifestage_raw_la,
      lifestage_raw_pg,
      lifestage_raw_combined,
      location_raw_la,
      location_raw_pg,
      location_raw_combined
    ) |>
    dplyr::arrange(publication_year, id)
}
