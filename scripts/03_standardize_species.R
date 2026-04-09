species_outputs <- build_species_outputs(papers_included)
paper_species <- species_outputs$paper
crosswalk_species <- species_outputs$crosswalk

write_derived_csv(paper_species, "data-derived/paper_species.csv")
write_derived_csv(crosswalk_species, "data-derived/crosswalk_species.csv")
