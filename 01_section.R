#' @param pcs ICD-10-PCS code, a __7-character__ alphanumeric code |> 
#' @return description
#' @autoglobal
#' @noRd
pcs <- function(pcs,
                arg = rlang::caller_arg(pcs),
                call = rlang::caller_env()) {
  
  if (nchar(pcs) > 7L) {
    cli::cli_abort(c(
    "A {.strong PCS} code cannot be more than {.emph 7 characters long}.",
    "x" = "{.val {pcs}} is {.val {nchar(pcs)}} characters long."),
    call = call)
  }
  
  if (grepl("[0-9A-HJ-NP-Z]", pcs) == FALSE) {
    cli::cli_abort(c(
      "x" = "{.strong {.val {pcs}}} contains {.emph non-valid} characters."),
      call = call)
  }
  
  if(is.numeric(pcs)) x <- as.character(pcs)

  x <- unlist(strsplit(pcs, ""), use.names = FALSE)
  
  # 1: Section
  .section <- section(x)
  
  # 2: Body System
  .system <- system(x)
  
  # 3: Root Operation
  .root <- root(x)
  
  # 4: Body Part
  .part <- part(x)
  
  # 5: Approach
  .approach <- approach(x)
  
  # 6: Device
  .device <- device(x)
  
  # 7: Qualifier
  .qualifier <- qualifier(x)
  
  results <- list(
    Code             = pcs,
    Section          = .section,
    'Body System'    = .system,
    'Root Operation' = .root,
    'Body Part'      = .part,
    Approach         = .approach,
    Device           = .device,
    Qualifier        = .qualifier
  )
  return(results)
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
section <- function(x) {
  
  if(is.numeric(x)) x <- as.character(x)
  
  if (length(x) > 1) x <- paste0(x[1], collapse = "")
  
  section <- vctrs::vec_c(
    '0' = 'Medical and Surgical',
    '1' = 'Obstetrics',
    '2' = 'Placement',
    '3' = 'Administration',
    '4' = 'Measurement and Monitoring',
    '5' = 'Extracorporeal or Systemic Assistance and Performance',
    '6' = 'Extracorporeal or Systemic Therapies',
    '7' = 'Osteopathic',
    '8' = 'Other Procedures',
    '9' = 'Chiropractic',
    'B' = 'Imaging',
    'C' = 'Nuclear Medicine',
    'D' = "Radiation Therapy",
    'F' = "Physical Rehabilitation and Diagnostic Audiology",
    'G' = 'Mental Health',
    'H' = 'Substance Abuse Treatment',
    'X' = 'New Technology')
  
  section <- unname(section[x])
  return(section)
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
system <- function(x) {
  
  if(is.numeric(x)) x <- as.character(x)
  
  if (length(x) > 1) x <- x[1]; y <- x[2]
  
  system <- switch(x,
    '0' = sys.0(),
    '1' = sys.1(),
    '2' = 'Placement',
    '3' = 'Administration',
    '4' = 'Measurement and Monitoring',
    '5' = 'Extracorporeal or Systemic Assistance and Performance',
    '6' = 'Extracorporeal or Systemic Therapies',
    '7' = 'Osteopathic',
    '8' = 'Other Procedures',
    '9' = 'Chiropractic',
    'B' = 'Imaging',
    'C' = 'Nuclear Medicine',
    'D' = "Radiation Therapy",
    'F' = "Physical Rehabilitation and Diagnostic Audiology",
    'G' = 'Mental Health',
    'H' = 'Substance Abuse Treatment',
    'X' = 'New Technology')
  
  system <- unname(system[y])
  return(system)
}


