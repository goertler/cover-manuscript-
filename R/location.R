location_row <- function(
  id,
  full_text_file,
  location_raw_la,
  location_raw_pg,
  location_standardized,
  location_type,
  country,
  state_province = NA_character_,
  geometry_name = location_standardized,
  geometry_type = location_type,
  geometry_source = "manual_standardization_from_review_text",
  mapping_note = "Reconciled LA and PG location fields to a common analysis-ready feature."
) {
  data.frame(
    id = id,
    full_text_file = full_text_file,
    location_raw_la = location_raw_la,
    location_raw_pg = location_raw_pg,
    location_standardized = location_standardized,
    location_type = location_type,
    country = country,
    state_province = state_province,
    geometry_name = geometry_name,
    geometry_type = geometry_type,
    geometry_source = geometry_source,
    mapping_note = mapping_note,
    stringsAsFactors = FALSE
  )
}

standardize_location_record <- function(id, full_text_file, location_raw_la, location_raw_pg) {
  key <- normalize_match_key(combine_reviewer_values(location_raw_la, location_raw_pg))

  if (is.na(key) || stringr::str_detect(key, "review paper")) {
    return(location_row(
      id = id,
      full_text_file = full_text_file,
      location_raw_la = location_raw_la,
      location_raw_pg = location_raw_pg,
      location_standardized = "not_applicable",
      location_type = "not_applicable",
      country = NA_character_,
      geometry_name = NA_character_,
      geometry_type = NA_character_,
      mapping_note = "Location not applicable for review or synthesis paper."
    ))
  }

  if (stringr::str_detect(key, "coyote creek|stevens creek|guadalupe river|three creeks watershed")) {
    return(dplyr::bind_rows(
      location_row(id, full_text_file, location_raw_la, location_raw_pg, "Coyote Creek, California, USA", "river", "United States", "California"),
      location_row(id, full_text_file, location_raw_la, location_raw_pg, "Stevens Creek, California, USA", "river", "United States", "California"),
      location_row(id, full_text_file, location_raw_la, location_raw_pg, "Guadalupe River, California, USA", "river", "United States", "California")
    ))
  }

  if (stringr::str_detect(key, "big jonathan brook")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Big Jonathan Brook, Quebec, Canada", "river", "Canada", "Quebec"))
  }

  if (stringr::str_detect(key, "romaine river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Romaine River, Quebec, Canada", "river", "Canada", "Quebec"))
  }

  if (stringr::str_detect(key, "cascapedia")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Cascapedia River basin, Quebec, Canada", "watershed", "Canada", "Quebec",
      geometry_name = "Cascapedia River basin",
      geometry_type = "watershed"
    ))
  }

  if (stringr::str_detect(key, "skagit river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Skagit River, British Columbia, Canada", "river", "Canada", "British Columbia"))
  }

  if (stringr::str_detect(key, "chilliwack river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Chilliwack River, British Columbia, Canada", "river", "Canada", "British Columbia"))
  }

  if (stringr::str_detect(key, "denmark")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Denmark", "country", "Denmark",
      geometry_name = "Denmark",
      geometry_type = "country"
    ))
  }

  if (stringr::str_detect(key, "oregon hatchery research center|experimental channels|alsea")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Oregon Hatchery Research Center, Oregon, USA", "admin_area", "United States", "Oregon",
      geometry_name = "Oregon Hatchery Research Center",
      geometry_type = "facility",
      mapping_note = "Experimental-channel study reconciled to the named research facility."
    ))
  }

  if (stringr::str_detect(key, "hart brook")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Hart Brook, New York, USA", "river", "United States", "New York"))
  }

  if (stringr::str_detect(key, "east stoke millstream|river frome")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "East Stoke Millstream (River Frome), England, UK", "river", "United Kingdom", "England"))
  }

  if (stringr::str_detect(key, "mill creek")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Mill Creek, Oregon, USA", "river", "United States", "Oregon"))
  }

  if (stringr::str_detect(key, "trout brook")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Trout Brook, New York, USA", "river", "United States", "New York"))
  }

  if (stringr::str_detect(key, "eel river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Eel River, California, USA", "river", "United States", "California"))
  }

  if (stringr::str_detect(key, "touchet river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Touchet River, Washington, USA", "river", "United States", "Washington"))
  }

  if (stringr::str_detect(key, "russian river")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Russian River tributaries, California, USA", "watershed", "United States", "California",
      geometry_name = "Russian River tributaries",
      geometry_type = "watershed"
    ))
  }

  if (stringr::str_detect(key, "western washington|satsop river")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Western Washington study streams, Washington, USA", "multi_site", "United States", "Washington",
      geometry_name = "Western Washington study streams",
      geometry_type = "multi_site",
      mapping_note = "Reviewer fields indicate a multi-stream western Washington study rather than a single river."
    ))
  }

  if (stringr::str_detect(key, "upper grande ronde")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Upper Grande Ronde River, Oregon, USA", "river", "United States", "Oregon"))
  }

  if (stringr::str_detect(key, "grande ronde")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Grande Ronde River, Oregon, USA", "river", "United States", "Oregon"))
  }

  if (stringr::str_detect(key, "yakima river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Yakima River, Washington, USA", "river", "United States", "Washington"))
  }

  if (stringr::str_detect(key, "yuba river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Yuba River, California, USA", "river", "United States", "California"))
  }

  if (stringr::str_detect(key, "coquitlam river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Coquitlam River, British Columbia, Canada", "river", "Canada", "British Columbia"))
  }

  if (stringr::str_detect(key, "cains river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Cains River, New Brunswick, Canada", "river", "Canada", "New Brunswick"))
  }

  if (stringr::str_detect(key, "new brunswick")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "New Brunswick, Canada", "admin_area", "Canada", "New Brunswick",
      geometry_name = "New Brunswick",
      geometry_type = "admin_area"
    ))
  }

  if (stringr::str_detect(key, "river main")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "River Main, Northern Ireland, UK", "river", "United Kingdom", "Northern Ireland"))
  }

  if (stringr::str_detect(key, "northern ireland")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Northern Ireland, UK", "admin_area", "United Kingdom", "Northern Ireland",
      geometry_name = "Northern Ireland",
      geometry_type = "admin_area"
    ))
  }

  if (stringr::str_detect(key, "teno")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Teno catchment, Finland and Norway", "watershed", "Finland; Norway",
      geometry_name = "Teno catchment",
      geometry_type = "watershed",
      mapping_note = "LA river-level location and PG catchment-level location reconciled to the shared catchment."
    ))
  }

  if (stringr::str_detect(key, "gjengedalselva|gjengedalselva river")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Gjengedalselva River, Norway", "river", "Norway"))
  }

  if (stringr::str_detect(key, "bolstadbekken")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Bolstadbekken, Norway", "river", "Norway"))
  }

  if (stringr::str_detect(key, "\\bnorway\\b")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Norway", "country", "Norway",
      geometry_name = "Norway",
      geometry_type = "country"
    ))
  }

  if (stringr::str_detect(key, "catherine creek")) {
    return(location_row(id, full_text_file, location_raw_la, location_raw_pg, "Catherine Creek, Oregon, USA", "river", "United States", "Oregon"))
  }

  if (stringr::str_detect(key, "quebec")) {
    return(location_row(
      id, full_text_file, location_raw_la, location_raw_pg,
      "Quebec, Canada", "admin_area", "Canada", "Quebec",
      geometry_name = "Quebec",
      geometry_type = "admin_area"
    ))
  }

  location_row(
    id = id,
    full_text_file = full_text_file,
    location_raw_la = location_raw_la,
    location_raw_pg = location_raw_pg,
    location_standardized = collapse_unique(c(location_raw_la, location_raw_pg), sep = " || "),
    location_type = "multi_site",
    country = NA_character_,
    geometry_name = collapse_unique(c(location_raw_la, location_raw_pg), sep = " || "),
    geometry_type = "multi_site",
    mapping_note = "No rule-based standardization matched; retained the reconciled reviewer text."
  )
}

build_location_outputs <- function(papers_included) {
  location_crosswalk <- dplyr::bind_rows(lapply(seq_len(nrow(papers_included)), function(i) {
    standardize_location_record(
      id = papers_included$id[i],
      full_text_file = papers_included$full_text_file[i],
      location_raw_la = papers_included$location_raw_la[i],
      location_raw_pg = papers_included$location_raw_pg[i]
    )
  })) |>
    dplyr::arrange(id, location_standardized)

  paper_locations <- location_crosswalk |>
    dplyr::left_join(
      papers_included |>
        dplyr::select(id, publication_year, doi, title),
      by = "id"
    ) |>
    dplyr::select(
      id, full_text_file, publication_year, doi, title, location_standardized,
      location_type, country, state_province, geometry_name, geometry_type, geometry_source
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(id, location_standardized)

  list(
    crosswalk = location_crosswalk,
    paper = paper_locations
  )
}

build_location_map_summary <- function(paper_locations) {
  subnational_units <- paper_locations |>
    dplyr::filter(!is.na(state_province) & state_province != "") |>
    dplyr::transmute(
      id,
      map_unit_level = "subnational",
      map_unit_name = normalize_string(state_province),
      map_unit_key = normalize_map_unit_name(state_province)
    )

  country_units <- paper_locations |>
    dplyr::filter(is.na(state_province) | state_province == "") |>
    dplyr::transmute(id, map_unit_name = normalize_string(country)) |>
    tidyr::separate_rows(map_unit_name, sep = ";\\s*") |>
    dplyr::transmute(
      id,
      map_unit_level = "country",
      map_unit_name,
      map_unit_key = normalize_map_unit_name(map_unit_name)
    )

  dplyr::bind_rows(subnational_units, country_units) |>
    dplyr::filter(!is.na(map_unit_key) & map_unit_key != "") |>
    dplyr::distinct(id, map_unit_level, map_unit_key, .keep_all = TRUE) |>
    dplyr::group_by(map_unit_level, map_unit_key) |>
    dplyr::summarise(
      map_unit_name = dplyr::first(map_unit_name),
      n_papers = dplyr::n_distinct(id),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_papers), map_unit_level, map_unit_name)
}
