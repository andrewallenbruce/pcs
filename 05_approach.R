#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
app.0 <- function() {
  
  vctrs::vec_c(
    '0' = 'Open',
    '3' = 'Percutaneous',
    '4' = 'Percutaneous Endoscopic',
    '7' = 'Via Natural or Artificial Opening',
    '8' = 'Via Natural or Artificial Opening Endoscopic',
    'F' = 'Via Natural or Artificial Opening with Percutaneous Endoscopic Assistance',
    'X' = 'External')
  
}

#' Return the PCS body system for section Obstetrics
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
app.1 <- function() {
  
  vctrs::vec_c(
    '0' = 'Open',
    '3' = 'Percutaneous',
    '4' = 'Percutaneous Endoscopic',
    '7' = 'Via Natural or Artificial Opening',
    '8' = 'Via Natural or Artificial Opening Endoscopic',
    'F' = 'Via Natural or Artificial Opening with Percutaneous Endoscopic Assistance',
    'X' = 'External')
  
}
