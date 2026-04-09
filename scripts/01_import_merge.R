reviews <- load_review_data()

reviewer_comparison <- build_reviewer_comparison(reviews$la, reviews$pg)

write_derived_csv(reviewer_comparison, "data-derived/reviewer_comparison.csv")
