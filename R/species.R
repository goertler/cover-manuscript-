standardize_species_for_text <- function(...) {
  raw_text <- collapse_unique(c(...), sep = " || ")
  key <- normalize_match_key(raw_text)

  if (is.na(key) || stringr::str_detect(key, "no species information")) {
    return(character())
  }

  species <- character()
  add_species <- function(condition, label) {
    if (isTRUE(condition)) {
      species <<- c(species, label)
    }
  }

  add_species(stringr::str_detect(key, "spring run") && stringr::str_detect(key, "\\bchinook\\b"), "Oncorhynchus tshawytscha (spring-run)")
  add_species(stringr::str_detect(key, "fall run") && stringr::str_detect(key, "\\bchinook\\b"), "Oncorhynchus tshawytscha (fall-run)")
  add_species(stringr::str_detect(key, "\\bchinook\\b"), "Oncorhynchus tshawytscha")
  add_species(stringr::str_detect(key, "\\bcoho\\b"), "Oncorhynchus kisutch")
  add_species(stringr::str_detect(key, "\\bchum\\b"), "Oncorhynchus keta")
  add_species(stringr::str_detect(key, "\\bsockeye\\b"), "Oncorhynchus nerka")
  add_species(stringr::str_detect(key, "\\bpink\\b"), "Oncorhynchus gorbuscha")
  add_species(stringr::str_detect(key, "\\bsteelhead\\b"), "Oncorhynchus mykiss (steelhead)")
  add_species(stringr::str_detect(key, "\\brainbow trout\\b|\\brbt\\b"), "Oncorhynchus mykiss (rainbow trout)")
  add_species(
    stringr::str_detect(key, "\\bcutthroat\\b"),
    if (stringr::str_detect(key, "coastal")) {
      "Oncorhynchus clarkii (coastal cutthroat trout)"
    } else {
      "Oncorhynchus clarkii"
    }
  )
  add_species(stringr::str_detect(key, "atlantic salmon"), "Salmo salar")
  add_species(stringr::str_detect(key, "brook trout"), "Salvelinus fontinalis")
  add_species(stringr::str_detect(key, "brown trout"), "Salmo trutta")
  add_species(stringr::str_detect(key, "bull trout"), "Salvelinus confluentus")
  add_species(stringr::str_detect(key, "lake trout"), "Salvelinus namaycush")
  add_species(stringr::str_detect(key, "mountain whitefish"), "Prosopium williamsoni")
  add_species(stringr::str_detect(key, "mountain sucker"), "Catostomus platyrhynchus")
  add_species(stringr::str_detect(key, "dolly varden"), "Salvelinus malma")
  add_species(stringr::str_detect(key, "torrentfish"), "Cheimarrichthys fosteri")
  add_species(stringr::str_detect(key, "bluegilled bully"), "Gobiomorphus hubbsi")
  add_species(stringr::str_detect(key, "upland bully|uplandy bully"), "Gobiomorphus breviceps")
  add_species(stringr::str_detect(key, "longfinned eel"), "Anguilla dieffenbachii")

  if (stringr::str_detect(key, "\\bsalmon\\b") && !any(stringr::str_detect(species, "^Oncorhynchus|^Salmo salar"))) {
    species <- c(species, "unresolved salmon")
  }

  generic_trout_key <- stringr::str_remove_all(
    key,
    "brook trout|brown trout|bull trout|lake trout|rainbow trout|cutthroat trout|steelhead trout"
  )

  if (stringr::str_detect(generic_trout_key, "\\btrout\\b")) {
    species <- c(species, "unresolved trout")
  }

  if (stringr::str_detect(key, "\\bchar\\b") && !any(stringr::str_detect(species, "^Salvelinus"))) {
    species <- c(species, "unresolved char")
  }

  if (stringr::str_detect(key, "\\bgrayling\\b")) {
    species <- c(species, "unresolved grayling")
  }

  if (stringr::str_detect(key, "\\bdace\\b")) {
    species <- c(species, "unresolved dace")
  }

  if (stringr::str_detect(key, "\\broach\\b")) {
    species <- c(species, "unresolved roach")
  }

  if (stringr::str_detect(key, "\\bpike\\b")) {
    species <- c(species, "unresolved pike")
  }

  if (stringr::str_detect(key, "\\beel\\b") && !stringr::str_detect(key, "longfinned eel")) {
    species <- c(species, "unresolved eel")
  }

  species <- unique(species)

  if ("Oncorhynchus tshawytscha (spring-run)" %in% species || "Oncorhynchus tshawytscha (fall-run)" %in% species) {
    species <- setdiff(species, "Oncorhynchus tshawytscha")
  }

  sort(species)
}

build_species_outputs <- function(papers_included) {
  crosswalk_rows <- lapply(seq_len(nrow(papers_included)), function(i) {
    standardized <- standardize_species_for_text(
      papers_included$species_raw_la[i],
      papers_included$species_raw_pg[i]
    )

    if (length(standardized) == 0) {
      standardized <- NA_character_
    }

    data.frame(
      id = papers_included$id[i],
      full_text_file = papers_included$full_text_file[i],
      species_raw_la = papers_included$species_raw_la[i],
      species_raw_pg = papers_included$species_raw_pg[i],
      species_standardized = standardized,
      mapping_note = "Union of reviewer species fields; exact species retained when explicit and unresolved buckets used only when the text stayed ambiguous.",
      stringsAsFactors = FALSE
    )
  })

  species_crosswalk <- dplyr::bind_rows(crosswalk_rows) |>
    dplyr::arrange(id, species_standardized)

  paper_species <- species_crosswalk |>
    dplyr::filter(!is.na(species_standardized)) |>
    dplyr::left_join(
      papers_included |>
        dplyr::select(id, publication_year, doi, title),
      by = "id"
    ) |>
    dplyr::select(id, full_text_file, publication_year, doi, title, species_standardized) |>
    dplyr::distinct() |>
    dplyr::arrange(id, species_standardized)

  list(
    crosswalk = species_crosswalk,
    paper = paper_species
  )
}
