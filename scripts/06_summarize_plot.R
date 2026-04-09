lifestage_display_order <- c(
  "adult",
  "unspecified juvenile",
  "smolt",
  "yearling",
  "subyearling",
  "parr",
  "fry",
  "egg"
)

year_summary <- papers_included |>
  dplyr::count(publication_year, name = "n_papers") |>
  dplyr::filter(!is.na(publication_year)) |>
  tidyr::complete(
    publication_year = seq(min(publication_year), max(publication_year), by = 1),
    fill = list(n_papers = 0L)
  ) |>
  dplyr::arrange(publication_year)

species_summary <- paper_species |>
  dplyr::count(species_standardized, name = "n_papers") |>
  dplyr::arrange(dplyr::desc(n_papers), species_standardized)

lifestage_summary <- paper_lifestage |>
  dplyr::count(lifestage, name = "n_papers") |>
  dplyr::mutate(
    lifestage = dplyr::if_else(
      lifestage == "juvenile_unspecified",
      "unspecified juvenile",
      lifestage
    ),
    lifestage_order = match(lifestage, lifestage_display_order)
  ) |>
  dplyr::arrange(lifestage_order) |>
  dplyr::select(-lifestage_order)

location_summary <- paper_locations |>
  dplyr::count(location_standardized, location_type, name = "n_papers") |>
  dplyr::arrange(dplyr::desc(n_papers), location_standardized)

location_map_summary <- build_location_map_summary(paper_locations)

location_type_summary <- paper_locations |>
  dplyr::count(location_type, name = "n_papers") |>
  dplyr::arrange(dplyr::desc(n_papers), location_type)

species_by_paper <- paper_species |>
  dplyr::group_by(id) |>
  dplyr::summarise(species_standardized = collapse_unique(species_standardized), .groups = "drop")

lifestage_by_paper <- paper_lifestage |>
  dplyr::group_by(id) |>
  dplyr::summarise(lifestage_standardized = collapse_unique(lifestage), .groups = "drop")

location_by_paper <- paper_locations |>
  dplyr::group_by(id) |>
  dplyr::summarise(location_standardized = collapse_unique(location_standardized), .groups = "drop")

papers_included <- papers_included |>
  dplyr::left_join(species_by_paper, by = "id") |>
  dplyr::left_join(lifestage_by_paper, by = "id") |>
  dplyr::left_join(location_by_paper, by = "id")

write_derived_csv(papers_included, "data-derived/papers_included.csv")
write_derived_csv(papers_included, "data-derived/papers_clean.csv")
write_derived_csv(year_summary, "data-derived/summary_year.csv")
write_derived_csv(species_summary, "data-derived/summary_species.csv")
write_derived_csv(lifestage_summary, "data-derived/summary_lifestage.csv")
write_derived_csv(location_summary, "data-derived/summary_location.csv")
write_derived_csv(location_map_summary, "data-derived/summary_location_map.csv")
write_derived_csv(location_type_summary, "data-derived/summary_location_type.csv")

save_year_time_series_plot(
  data = year_summary,
  year_col = "publication_year",
  count_col = "n_papers",
  title = "Papers by Publication Year",
  path = "figures/year_published.png",
  fill = "#000000"
)

save_count_plot(
  data = species_summary,
  category_col = "species_standardized",
  count_col = "n_papers",
  title = "Papers by Species",
  path = "figures/species.png",
  fill = "#000000"
)

save_count_plot(
  data = lifestage_summary,
  category_col = "lifestage",
  count_col = "n_papers",
  title = "Papers by Lifestage",
  path = "figures/lifestage.png",
  fill = "#000000"
)

save_location_map(
  data = location_map_summary,
  title = "Papers by Study Location",
  path = "figures/location.png"
)
