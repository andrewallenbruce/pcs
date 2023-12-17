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

prep <- function(x) {
  
  if (is.numeric(x)) x <- as.character(x)
  
  # If any chars are lowercase, capitalize
  if (grepl("[[:lower:]]*", x)) x <- toupper(x)
  
  sp <- unlist(strsplit(x, ""), use.names = FALSE)
  
  return(list(code = x,
              split = sp))
}

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
  
  axes <- switch(
    x$split[1],
    '0' = base,
    '1' = base,
    '2' = { base[4] <- "Body Region" },
    '3' = { c(base[4], base[6]) %<-% c("Body System/Region", "Substance") },
    '4' = { c(base[4], base[6]) %<-% c("Body System/Region", "Function/Device") },
    '5' = { c(base[4], base[5], base[6]) %<-% c("Body System", "Duration", "Function") },
    '6' = { c(base[4], base[5], base[6]) %<-% c("Body System", "Duration", "Qualifier") },
    '7' = ,
    '8' = ,
    '9' = { c(base[4], base[6]) %<-% c("Body Region", "Method") },
    'B' = { c(base[3], base[5], base[6]) %<-% c("Root Type", "Contrast", "Qualifier") },
    'C' = { c(base[3], base[5], base[6]) %<-% c("Root Type", "Radionuclide", "Qualifier") },
    'D' = { c(base[3], base[4], base[5], base[6]) %<-% c("Modality", "Treatment Site", "Modality Qualifier", "Isotope") },
    'F' = { c(base[2], base[3], base[4], base[5], base[6]) %<-% c("Section Qualifier", "Root Type", "Body System/Region", "Type Qualifier", "Equipment") },
    'G' = ,
    'H' = { c(base[3], base[4], base[5], base[6]) %<-% c("Root Type", "Qualifier", "Qualifier", "Qualifier") },
    'X' = { base[6] <- "Device/Substance/Technology" }
  )
  return(append(x, list(axes = unname(axes))))
}

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

system <- function(x) {
  
  system <- switch(
    x$section$axis,
    '0' = sys.0(),
    '1' = sys.1(),
    '2' = sys.2(),
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

root <- function(x) {
  
  root <- switch(
    x$section$axis,
    '0' = root.0(),
    '1' = root.1(),
    '2' = root.2(),
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
  
  if (x$section$axis == '2') root <- root[x$split[2]][[1]]
  
  root <- root[x$split[3]]
  
  root <- list(
    axis = names(root),
    title = unname(root))
  
  return(append(x, list(root = root)))
}

part <- function(x) {
  
  part <- switch(
    x$section$axis,
    '0' = part.0(),
    '1' = part.1(),
    '2' = part.2(), # multiple - W, Y
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
  
  if (x$section$axis == '2') part <- part[x$split[3]][[1]]
  
  part <- part[x$split[4]]
  
  part <- list(
    axis = names(part),
    title = unname(part))
  
  return(append(x, list(part = part)))
}

approach <- function(x) {
  
  approach <- switch(
    x$section$axis,
    '0' = app.0(),
    '1' = app.1(),
    '2' = app.2(),
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

device <- function(x) {
  
  device <- switch(
    x$section$axis,
    '0' = dev.0(),
    '1' = dev.1(),
    '2' = dev.2(),
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
  
  if (x$section$axis == '2') device <- device[x$split[5]][[1]]
  
  device <- device[x$split[6]]
  
  device <- list(
    axis = names(device),
    title = unname(device))
  
  return(append(x, list(device = device)))
}

qualifier <- function(x) {
  
  qualifier <- switch(
    x$section$axis,
    '0' = qual.0(),
    '1' = qual.1(),
    '2' = qual.2(),
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
