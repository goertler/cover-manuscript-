lifestage_outputs <- build_lifestage_outputs(papers_included)
paper_lifestage <- lifestage_outputs$paper
crosswalk_lifestage <- lifestage_outputs$crosswalk

write_derived_csv(paper_lifestage, "data-derived/paper_lifestage.csv")
write_derived_csv(crosswalk_lifestage, "data-derived/crosswalk_lifestage.csv")
