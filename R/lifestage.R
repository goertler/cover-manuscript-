standardize_lifestage_for_text <- function(...) {
  raw_text <- collapse_unique(c(...), sep = " || ")
  key <- normalize_match_key(raw_text)
  raw_lower <- tolower(normalize_utf8(raw_text))

  if (is.na(key)) {
    return(character())
  }

  stages <- character()
  add_stage <- function(condition, label) {
    if (isTRUE(condition)) {
      stages <<- c(stages, label)
    }
  }

  add_stage(stringr::str_detect(key, "\\begg\\b|incubat"), "egg")
  add_stage(stringr::str_detect(key, "\\bfry\\b|egg to fry|emergence"), "fry")
  add_stage(stringr::str_detect(key, "\\bparr\\b"), "parr")
  add_stage(stringr::str_detect(raw_lower, "\\bsmolt(s)?\\b"), "smolt")
  add_stage(
    stringr::str_detect(raw_lower, "subyearling|young[- ]of[- ]the[- ]year|young[- ]of[- ]year|\\b0\\+\\b"),
    "subyearling"
  )
  add_stage(
    stringr::str_detect(raw_lower, "yearling|overyearling|\\b1\\+\\b"),
    "yearling"
  )
  add_stage(stringr::str_detect(key, "adult|spawning"), "adult")

  specific_juvenile <- any(c("fry", "parr", "smolt", "subyearling", "yearling") %in% stages)
  juvenile_signal <- stringr::str_detect(key, "juvenile|overwintering juvenile|subadult|\\byoung\\b")
  generic_rearing_only <- stringr::str_detect(key, "\\brearing\\b") && !specific_juvenile

  if (juvenile_signal || generic_rearing_only) {
    stages <- c(stages, "juvenile_unspecified")
  }

  if (
    length(stages) == 0 &&
    stringr::str_detect(key, "no life stage information|not specific to life stage|undescribed|unclear")
  ) {
    return(character())
  }

  unique_order <- c(
    "egg", "fry", "parr", "smolt",
    "subyearling", "yearling", "juvenile_unspecified", "adult"
  )

  unique_order[unique_order %in% unique(stages)]
}

build_lifestage_outputs <- function(papers_included) {
  crosswalk_rows <- lapply(seq_len(nrow(papers_included)), function(i) {
    standardized <- standardize_lifestage_for_text(
      papers_included$lifestage_raw_la[i],
      papers_included$lifestage_raw_pg[i]
    )

    if (length(standardized) == 0) {
      standardized <- NA_character_
    }

    data.frame(
      id = papers_included$id[i],
      full_text_file = papers_included$full_text_file[i],
      lifestage_raw_la = papers_included$lifestage_raw_la[i],
      lifestage_raw_pg = papers_included$lifestage_raw_pg[i],
      lifestage = standardized,
      mapping_note = "Union of reviewer lifestage fields; only controlled-vocabulary life stages retained, with behavior terms ignored except that spawning maps to adult.",
      stringsAsFactors = FALSE
    )
  })

  lifestage_crosswalk <- dplyr::bind_rows(crosswalk_rows) |>
    dplyr::arrange(id, lifestage)

  paper_lifestage <- lifestage_crosswalk |>
    dplyr::filter(!is.na(lifestage)) |>
    dplyr::left_join(
      papers_included |>
        dplyr::select(id, publication_year, doi, title),
      by = "id"
    ) |>
    dplyr::select(id, full_text_file, publication_year, doi, title, lifestage) |>
    dplyr::distinct() |>
    dplyr::arrange(id, lifestage)

  list(
    crosswalk = lifestage_crosswalk,
    paper = paper_lifestage
  )
}
