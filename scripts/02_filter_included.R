papers_included <- build_included_papers(reviewer_comparison)

write_derived_csv(papers_included, "data-derived/papers_included.csv")
write_derived_csv(papers_included, "data-derived/papers_clean.csv")
