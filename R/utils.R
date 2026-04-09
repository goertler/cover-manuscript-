ensure_directories <- function(paths) {
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
}

project_path <- function(...) {
  parts <- list(...)

  if (
    length(parts) == 1 &&
    is.character(parts[[1]]) &&
    length(parts[[1]]) > 1
  ) {
    return(vapply(parts[[1]], here::here, character(1), USE.NAMES = FALSE))
  }

  do.call(here::here, parts)
}

normalize_utf8 <- function(x) {
  x <- as.character(x)
  out <- iconv(x, from = "", to = "UTF-8")
  fallback <- is.na(out) & !is.na(x)

  if (any(fallback)) {
    out[fallback] <- iconv(x[fallback], from = "latin1", to = "UTF-8")
  }

  out
}

normalize_string <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- normalize_utf8(x)
  x <- stringr::str_replace_all(x, stringr::fixed("\u00A0"), " ")
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

clean_names <- function(x) {
  x |>
    normalize_string() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

normalize_match_key <- function(x) {
  x <- normalize_string(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  x <- stringr::str_squish(x)
  x
}

normalize_map_unit_name <- function(x) {
  key <- normalize_match_key(x)

  dplyr::case_when(
    is.na(key) ~ NA_character_,
    key == "qu ebec" ~ "quebec",
    key %in% c("united states", "usa") ~ "united states of america",
    key == "uk" ~ "united kingdom",
    TRUE ~ key
  )
}

first_non_missing <- function(...) {
  candidates <- list(...)
  n <- length(candidates[[1]])
  out <- rep(NA_character_, n)

  for (candidate in candidates) {
    candidate <- normalize_string(as.character(candidate))
    take <- is.na(out) & !is.na(candidate)
    out[take] <- candidate[take]
  }

  out
}

collapse_unique <- function(x, sep = "; ") {
  x <- normalize_string(x)
  x <- unique(x[!is.na(x)])

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(x, collapse = sep)
}

combine_reviewer_values <- function(a, b) {
  mapply(
    FUN = function(left, right) {
      values <- normalize_string(c(left, right))
      values <- unique(values[!is.na(values)])

      if (length(values) == 0) {
        return(NA_character_)
      }

      paste(values, collapse = " || ")
    },
    a,
    b,
    USE.NAMES = FALSE
  )
}

normalize_flag <- function(x) {
  key <- normalize_match_key(x)

  dplyr::case_when(
    is.na(key) ~ NA_character_,
    key %in% c("y", "yes") ~ "Y",
    key %in% c("n", "no") ~ "N",
    stringr::str_detect(key, "^y(es)?\\b") ~ "Y",
    stringr::str_detect(key, "^n(o)?\\b") ~ "N",
    stringr::str_detect(key, "^maybe\\b") ~ "M",
    TRUE ~ NA_character_
  )
}

field_match <- function(a, b) {
  left <- normalize_match_key(a)
  right <- normalize_match_key(b)

  dplyr::case_when(
    is.na(left) | is.na(right) ~ NA,
    left == right ~ TRUE,
    TRUE ~ FALSE
  )
}

parse_publication_year <- function(x) {
  year <- stringr::str_extract(x, "(?<!\\d)\\d{4}(?!\\d)")
  as.integer(year)
}

write_derived_csv <- function(data, path) {
  readr::write_csv(data, project_path(path), na = "")
}
