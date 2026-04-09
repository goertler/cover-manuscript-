location_outputs <- build_location_outputs(papers_included)
paper_locations <- location_outputs$paper
crosswalk_location <- location_outputs$crosswalk

write_derived_csv(paper_locations, "data-derived/paper_locations.csv")
write_derived_csv(crosswalk_location, "data-derived/crosswalk_location.csv")
