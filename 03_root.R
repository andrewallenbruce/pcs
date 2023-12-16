#' Return the PCS root operation for section Medical & Surgical
#' @return character vector of a valid PCS root operation or `NA`
#' @autoglobal
#' @noRd
root.0 <- function() {

  vctrs::vec_c(
    "0" = "Alteration",
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
}

#' Return the PCS root operation for section Obstetrics
#' @return character vector of a valid PCS root operation or `NA`
#' @autoglobal
#' @noRd
root.1 <- function() {
  
  vctrs::vec_c(
    "2" = "Change",
    "9" = "Drainage",
    "A" = "Abortion",
    "D" = "Extraction",
    "E" = "Delivery",
    "H" = "Insertion",
    "J" = "Inspection",
    "P" = "Removal",
    "Q" = "Repair",
    "S" = "Reposition",
    "T" = "Resection",
    "Y" = "Transplantation"
  )
}