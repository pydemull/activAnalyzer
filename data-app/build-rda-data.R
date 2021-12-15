equations_mets <- readr::read_csv2("data-app/equations_mets.csv")
mvpa_cutpoints <- readr::read_csv2("data-app/mvpa_cutpoints.csv")
mvpa_lines <- readr::read_csv2("data-app/mvpa_lines.csv")
ratio_lines <- readr::read_csv2("data-app/ratio_lines.csv")
sed_cutpoints <- readr::read_csv2("data-app/sed_cutpoints.csv")
sed_lines <- readr::read_csv2("data-app/sed_lines.csv")

usethis::use_data(
  equations_mets,
  mvpa_cutpoints,
  mvpa_lines,
  ratio_lines,
  sed_cutpoints,
  sed_lines,
  internal = TRUE,
  overwrite = TRUE
)