require(quarto)


villages = clean.catch.combined |>
  distinct(village) |>
  pull(village) |>
  as.character()|>
  discard(~ is.na(.x) || .x == "")

report = tibble(
  input = "code/reef_fisheries_poster.qmd",
  output_file = str_glue("{villages}.html"),
  execute_params = map(villages, ~list(village = .x))
)

pwalk(report, quarto_render)
