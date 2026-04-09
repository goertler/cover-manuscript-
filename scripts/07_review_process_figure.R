review_process_summary <- data.frame(
  stage = c(
    "title_abstract_screening",
    "title_abstract_excluded",
    "full_text_review",
    "full_text_excluded",
    "included_in_analysis"
  ),
  n = c(
    1464L,
    1413L,
    51L,
    24L,
    27L
  ),
  stringsAsFactors = FALSE
)

search_details <- paste(
  "Web of Science: all collections, including BIOSIS Previews and Zoological Record",
  "ProQuest Agricultural & Environmental Science Collection: peer-reviewed only",
  "ProQuest GeoRef: peer-reviewed only",
  'Query: "habitat suitability" AND ("salmon" OR "salmonid" OR "salmonids")',
  sep = "\n"
)

title_abstract_note <- paste(
  "Required mention of both:",
  "cover terms",
  "juvenile salmonid rearing terms",
  sep = "\n"
)

full_text_note <- paste(
  "Excluded if coded as 1a, 4, or 5:",
  "1a = cover not evaluated / not included",
  "4 = juvenile salmon not evaluated",
  "5 = not a traditional or standard study",
  sep = "\n"
)

write_derived_csv(review_process_summary, "data-derived/summary_review_process.csv")

save_review_process_flow(
  path = "figures/review_process.png",
  records_screened = 1464L,
  full_text_review = 51L,
  included_in_analysis = 27L,
  search_details = search_details,
  title_abstract_note = title_abstract_note,
  full_text_note = full_text_note
)
