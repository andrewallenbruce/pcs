#' @param pcs ICD-10-PCS code, a __7-character__ alphanumeric code |> 
#' @return description
#' @autoglobal
#' @noRd
pcs <- function(pcs,
                arg = rlang::caller_arg(pcs),
                call = rlang::caller_env()) {
  
  if (!nzchar(pcs)) {
    cli::cli_abort("x" = "{.strong pcs} cannot be an {.emph empty} string.", 
                   call = call)
    }
  
  if (nchar(pcs) > 7L) {
    cli::cli_abort(c(
    "A {.strong PCS} code cannot be more than {.emph 7 characters long}.",
    "x" = "{.val {pcs}} is {.val {nchar(pcs)}} characters long."),
    call = call)
  }
  
  # If any chars are lowercase, capitalize
  if (grepl("[[:lower:]]*", pcs)) pcs <- toupper(pcs)
  
  if (grepl("[^[0-9A-HJ-NP-Z]]*", pcs)) {
    cli::cli_abort(c(
      "x" = "{.strong {.val {pcs}}} contains {.emph non-valid} characters."),
      call = call)
  }
  
  # A: Prepare
  .prep <- prep(pcs)
  
  # B: Axes
  .axis <- axes(.prep)
  
  # 1: Section
  .section <- section(.axis)
  
  # 2: Body System
  .system <- system(.section)
  
  # 3: Root Operation
  .root <- root(.system)
  
  # 4: Body Part
  .part <- part(.root)
  
  # 5: Approach
  .approach <- approach(.part)
  
  # 6: Device
  .device <- device(.approach)
  
  # 7: Qualifier
  .qualifier <- qualifier(.device)
  
  return(.qualifier)
}

#' Prepare the PCS code for processing
#' @param x ICD-10-PCS code
#' @return list of PCS code and the code split into individual characters
#' @autoglobal
#' @noRd
prep <- function(x) {
  
  if (is.numeric(x)) x <- as.character(x)
  
  sp <- unlist(strsplit(x, ""), use.names = FALSE)
  
  return(list(code = x,
              split = sp))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
axes <- function(x) {
  
  base <- vctrs::vec_c(
    '1' = 'Section',
    '2' = 'Body System',
    '3' = 'Root Operation',
    '4' = 'Body Part',
    '5' = 'Approach',
    '6' = 'Device',
    '7' = 'Qualifier'
  )
  
  axes <- switch(x$split[1],
                 '0' = base,
                 '1' = base,
                 '2' = { 
                   base[4] <- "Body Region" 
                 },
                 '3' = { 
                   base[4] <- "Body System/Region"
                   base[6] <- "Substance"
                 },
                 '4' = {
                   base[4] <- "Body System"
                   base[6] <- "Function/Device"
                 },
                 '5' = {
                   base[4] <- "Body System"
                   base[5] <- "Duration"
                   base[6] <- "Function"
                 },
                 '6' = {
                   base[4] <- "Body System"
                   base[5] <- "Duration"
                   base[6] <- "Qualifier"
                 },
                 '7' = {
                   base[4] <- "Body Region"
                   base[6] <- "Method"
                 },
                 '8' = {
                   base[4] <- "Body Region"
                   base[6] <- "Method"
                 },
                 '9' = {
                   base[4] <- "Body Region"
                   base[6] <- "Method"
                 },
                 'B' = {
                   base[3] <- "Root Type"
                   base[5] <- "Contrast"
                   base[6] <- "Qualifier"
                 },
                 'C' = {
                   base[3] <- "Root Type"
                   base[5] <- "Radionuclide"
                   base[6] <- "Qualifier"
                 },
                 'D' = {
                   base[3] <- "Modality"
                   base[4] <- "Treatment Site"
                   base[5] <- "Modality Qualifier"
                   base[6] <- "Isotope"
                 },
                 'F' = {
                   base[2] <- "Section Qualifier"
                   base[3] <- "Root Type"
                   base[4] <- "Body System/Region"
                   base[5] <- "Type Qualifier"
                   base[6] <- "Equipment"
                 },
                 'G' = {
                   base[3] <- "Root Type"
                   base[4] <- "Qualifier"
                   base[5] <- "Qualifier"
                   base[6] <- "Qualifier"
                 },
                 'H' = {
                   base[3] <- "Root Type"
                   base[4] <- "Qualifier"
                   base[5] <- "Qualifier"
                   base[6] <- "Qualifier"
                 },
                 'X' = {
                   base[6] <- "Device/Substance/Technology"
                 })

  return(append(x, list(axes = unname(axes))))
}

#' Return the PCS section
#' @param x list from prep()
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
section <- function(x) {
  
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
  
  section <- section[x$split[1]]
  
  section <- list(
    axis = names(section),
    title = unname(section))
  
  return(append(x, list(section = section)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
system <- function(x) {
  
  system <- switch(
    x$section$axis,
    '0' = sys.0(),
    '1' = sys.1(),
    '2' = 'Placement', # sys.3(),
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
  
  system <- system[x$split[2]]
  
  system <- list(
    axis = names(system),
    title = unname(system))
  
  return(append(x, list(system = system)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
root <- function(x) {
  
  root <- switch(
    x$section$axis,
    '0' = root.0(),
    '1' = 'Obstetrics', # root.1(),
    '2' = 'Placement', # root.2(),
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
  
  root <- root[x$split[3]]
  
  root <- list(
    axis = names(root),
    title = unname(root))
  
  return(append(x, list(root = root)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
part <- function(x) {
  
  part <- switch(
    x$section$axis,
    '0' = part.0(),
    '1' = 'Obstetrics', # root.1(),
    '2' = 'Placement', # root.2(),
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
  
  part <- part[x$split[4]]
  
  part <- list(
    axis = names(part),
    title = unname(part))
  
  return(append(x, list(part = part)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
approach <- function(x) {
  
  approach <- switch(
    x$section$axis,
    '0' = app.0(),
    '1' = 'Obstetrics', # root.1(),
    '2' = 'Placement', # root.2(),
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
  
  approach <- approach[x$split[5]]
  
  approach <- list(
    axis = names(approach),
    title = unname(approach))
  
  return(append(x, list(approach = approach)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
device <- function(x) {
  
  device <- switch(
    x$section$axis,
    '0' = dev.0(),
    '1' = 'Obstetrics', # root.1(),
    '2' = 'Placement', # root.2(),
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
  
  device <- device[x$split[6]]
  
  device <- list(
    axis = names(device),
    title = unname(device))
  
  return(append(x, list(device = device)))
}

#' Return the PCS section
#' @param x ICD-10-PCS code, the first character represents the section.
#' @return character vector of a valid PCS section or `NA`
#' @autoglobal
#' @noRd
qualifier <- function(x) {
  
  qualifier <- switch(
    x$section$axis,
    '0' = qual.0(),
    '1' = 'Obstetrics', # root.1(),
    '2' = 'Placement', # root.2(),
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
  
  qualifier <- qualifier[x$split[7]]
  
  qualifier <- list(
    axis = names(qualifier),
    title = unname(qualifier))
  
  return(append(x, list(qualifier = qualifier)))
}
