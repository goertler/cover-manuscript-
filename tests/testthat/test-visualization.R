test_that("review-process plotting writes files and rejects invalid stage counts", {
  plot_path <- tempfile(fileext = ".png")

  save_review_process_flow(
    path = plot_path,
    records_screened = 20,
    full_text_review = 5,
    included_in_analysis = 3,
    search_details = "Database search details",
    title_abstract_note = "Title/abstract exclusions",
    full_text_note = "Full-text exclusions"
  )

  expect_true(file.exists(plot_path))

  expect_error(
    save_review_process_flow(
      path = tempfile(fileext = ".png"),
      records_screened = 4,
      full_text_review = 5,
      included_in_analysis = 3,
      search_details = "Database search details",
      title_abstract_note = "Title/abstract exclusions",
      full_text_note = "Full-text exclusions"
    ),
    "counts must decrease"
  )
})

test_that("year time-series plotting writes files with a continuous year axis input", {
  plot_path <- tempfile(fileext = ".png")

  save_year_time_series_plot(
    data = data.frame(
      publication_year = 2000:2003,
      n_papers = c(1L, 0L, 2L, 0L)
    ),
    year_col = "publication_year",
    count_col = "n_papers",
    title = "Papers by Publication Year",
    path = plot_path,
    fill = "#000000"
  )

  expect_true(file.exists(plot_path))
})
