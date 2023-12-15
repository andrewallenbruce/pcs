#' Return the PCS body system for section Medical & Surgical
#' @param x ICD-10-PCS code, the second character represents the body system.
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
app.0 <- function(x) {
  
  if(is.numeric(x)) x <- as.character(x)
  
  if (length(x) > 1) x <- x[5]
  
  approach <- vctrs::vec_c(
    '0' = 'Open',
    '3' = 'Percutaneous',
    '4' = 'Percutaneous Endoscopic',
    '7' = 'Via Natural or Artificial Opening',
    '8' = 'Via Natural or Artificial Opening Endoscopic',
    'F' = 'Via Natural or Artificial Opening with Percutaneous Endoscopic Assistance',
    'X' = 'External')
  
  code <- paste0('[', rlang::sym(x), ']')
  approach <- unname(approach[x])
  
  return(paste(approach, code))
}