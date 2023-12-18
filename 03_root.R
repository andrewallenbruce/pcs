root.0 <- function() { # Medical/Surgical

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
    "B" = "Excision",
    "C" = "Extirpation",
    "D" = "Extraction",
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

root.1 <- function() { # Obstetrics
  
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

root.2 <- function() { # Placement
  
  w <- vctrs::vec_c(
    "0" = "Change",
    "1" = "Compression",
    "2" = "Dressing",
    "3" = "Immobilization",
    "4" = "Packing",
    "5" = "Removal",
    "6" = "Traction"
  )
  
  y <- w[c(1, 5, 6)]
  
  return(list("W" = w, "Y" = y))
}
