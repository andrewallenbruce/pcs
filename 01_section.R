#' @param pcs ICD-10-PCS code, a __7-character__ alphanumeric code |> 
#' @return description
#' @examples
#' # example code
#' 
#' @autoglobal
#' @noRd
pcs <- function(pcs) {

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

prep <- function(x,
                 arg = rlang::caller_arg(x),
                 call = rlang::caller_env()) {
  
  
  if (!nzchar(x)) {
    cli::cli_abort("x" = "{.strong x} cannot be an {.emph empty} string.", 
                   call = call)
  }
  
  if (nchar(x) > 7L) {
    cli::cli_abort(c(
      "A valid {.strong PCS} code is {.emph {.strong 7} characters long}.",
      "x" = "{.strong {.val {x}}} is {.strong {.val {nchar(x)}}} characters long."),
      call = call)
  }
  
  if (grepl("[^[0-9A-HJ-NP-Z]]*", x)) {
    cli::cli_abort(c(
      "x" = "{.strong {.val {x}}} contains {.emph non-valid} characters."),
      call = call)
  }
  
  if (is.numeric(x)) x <- as.character(x)
  
  # If any chars are lowercase, capitalize
  if (grepl("[[:lower:]]*", x)) x <- toupper(x)
  
  sp <- unlist(strsplit(x, ""), use.names = FALSE)
  
  names(sp) <- c("section", 
                 "system", 
                 "operation", 
                 "part", 
                 "approach", 
                 "device", 
                 "qualifier")
  
  tbl <- paste0(unname(sp[1:3]), collapse = "")
  
  return(list(code = rlang::sym(x),
              split = sp,
              table = rlang::sym(tbl)))
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
  
  base <- as.matrix(base)
  colnames(base) <- "Meaning"
  return(append(x, list(axes = base)))
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
  
  section <- unname(section[x$split[1]])
  names(section) <- "section"
  return(append(x, list(description = section)))
}

system <- function(x) {
  
  system <- switch(
    x$split[1],
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
    
  if (x$split[1] == '2') { names(system) <- NULL } else { system <- unname(system) }
  
  x$description['system'] <- system
  
  return(x)

}

root <- function(x) {
  
  root <- switch(
    x$split[1],
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
  
  root <- root[x$split[3]]
  
  if (x$split[1] == '2') { names(root) <- NULL } else { root <- unname(root) }
  
  x$description['operation'] <- root
  
  return(x)
}

part <- function(x) {
  
  part <- switch(
    x$split[1],
    '0' = part.0(),
    '1' = part.1(),
    '2' = part.2(),
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
  
  if (x$split[1] == '2') {
    
    part <- part[x$split[3]]
    part <- part[x$split[4]]
    names(part) <- NULL
    part <- unlist(part) 
    
    } else { 
      
      part <- part[x$split[4]]
      part <- unname(part) 
      
      }
  
  x$description['part'] <- part
  return(x)
}

approach <- function(x) {
  
  approach <- switch(
    x$split[1],
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
  
  approach <- unname(approach[x$split[5]])
  x$description['approach'] <- approach
  return(x)
}

device <- function(x) {
  
  device <- switch(
    x$split[1],
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
  
  device <- device[x$split[6]]
  
  if (x$split[1] == '2') { names(device) <- NULL; device <- unlist(device) } 
  else { device <- unname(device) }
  
  x$description['device'] <- device
  return(x)
}

qualifier <- function(x) {
  
  qualifier <- switch(
    x$split[1],
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
  
  qualifier <- unname(qualifier[x$split[7]])
  x$description['qualifier'] <- qualifier
  return(x)
}
