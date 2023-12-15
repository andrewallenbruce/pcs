#' Return the PCS root operation for section Medical & Surgical
#' @param x ICD-10-PCS code, the third character represents the root operation.
#' @return character vector of a valid PCS root operation or `NA`
#' @autoglobal
#' @noRd
root.0 <- function(x) {
  
  if(is.numeric(x)) x <- as.character(x)
  
  if (length(x) > 1) x <- paste0(x[3], collapse = "")

  root <- vctrs::vec_c("0" = "Alteration",
                       "1" = "Bypass",
                       "2" = "Change",
                       "3" = "Control",
                       "4" = "Creation",
                       "5" = "Destruction",
                       "6" = "Detachment",
                       "7" = "Dilation",
                       "8" = "Division",
                       "9" = "Drainage",
                       # "A", No A
                       "B" = "Excision",
                       "C" = "Extirpation",
                       "D" = "Extraction",
                       # "E", No E
                       "F" = "Fragmentation",
                       "G" = "Fusion", 
                       "H" = "Insertion",
                       "J" = "Inspection",
                       "K" = "Map",
                       "L" = "Occlusion",
                       "M" = "Reattachment", 
                       "N" = "Release",
                       "P" = "Removal",
                       "Q" = "Repair",
                       "R" = "Replacement",
                       "S" = "Reposition",
                       "T" = "Resection",
                       "U" = "Supplement",
                       "V" = "Restriction",
                       "W" = "Revision",
                       "X" = "Transfer",
                       "Y" = "Transplantation"
  )
  
  code <- paste0('[', rlang::sym(x), ']')
  root <- unname(root[x])
  return(
    paste(
      root, 
      code
      )
    )
}