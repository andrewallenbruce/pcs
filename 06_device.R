#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
dev.0 <- function() { # Medical/Surgical
  
  vctrs::vec_c(
    '0' = 'Drainage Device',
    '2' = 'Monitoring Device',
    '3' = 'Infusion Device',
    '4' = 'Radioactive Element, Cesium-131 Collagen Implant',
    '7' = 'Autologous Tissue Substitute',
    'J' = 'Synthetic Substitute',
    'K' = 'Nonautologous Tissue Substitute',
    'M' = 'Neurostimulator Lead',
    'Y' = 'Other Device',
    'Z' = 'No Device')
  
}

dev.1 <- function() { # Obstetrics
  
  vctrs::vec_c(
    '3' = 'Monitoring Electrode',
    'Y' = 'Other Device',
    'X' = 'No Device')
  
}

dev.2 <- function() { # Placement
  
  w <- vctrs::vec_c(
    '0' = 'Traction Apparatus',
    '1' = 'Splint',
    '2' = 'Cast',
    '3' = 'Brace',
    '4' = 'Bandage',
    '5' = 'Packing Material',
    '6' = 'Pressure Dressing',
    '7' = 'Intermittent Pressure Device',
    '9' = 'Wire',
    'Y' = 'Other Device',
    'Z' = 'No Device')
  
  y <- w[6]
  
  return(list("W" = w, "Y" = y))
  
}