icd10us::icd10pcs |>
  dplyr::select(icd10pcs_long_description) |>
  dplyr::slice_sample(n = 2000) |>
  dplyr::mutate(approach = strex::str_after_last(icd10pcs_long_description, ", ")) |>
  # dplyr::mutate(approach = strex::str_after_last(icd10pcs_long_description, ", "),
  #               approach = dplyr::if_else(approach %nin% approaches, NA_character_, approach)) |>
  dplyr::count(approach, sort = TRUE) |>
  print(n = 100)
