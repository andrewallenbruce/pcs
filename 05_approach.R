#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
app.0 <- function() { # Medical/Surgical
  
  vctrs::vec_c(
    '0' = 'Open',
    '3' = 'Percutaneous',
    '4' = 'Percutaneous Endoscopic',
    '7' = 'Via Natural or Artificial Opening',
    '8' = 'Via Natural or Artificial Opening Endoscopic',
    'F' = 'Via Natural or Artificial Opening with Percutaneous Endoscopic Assistance',
    'X' = 'External')
  
}

app.1 <- function() { # Obstetrics
  
  vctrs::vec_c(
    '0' = 'Open',
    '3' = 'Percutaneous',
    '4' = 'Percutaneous Endoscopic',
    '7' = 'Via Natural or Artificial Opening',
    '8' = 'Via Natural or Artificial Opening Endoscopic',
    'X' = 'External')
  
}

app.2 <- function() { # Placement
  
  vctrs::vec_c('X' = 'External')
  
}