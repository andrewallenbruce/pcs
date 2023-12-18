sys.0 <- function() { # Medical/Surgical
  
  vctrs::vec_c(
    '0' = 'Central Nervous System and Cranial Nerves',
    '1' = 'Peripheral Nervous System',
    '2' = 'Heart and Great Vessels',
    '3' = 'Upper Arteries',
    '4' = 'Lower Arteries',
    '5' = 'Upper Veins',
    '6' = 'Lower Veins',
    '7' = 'Lymphatic and Hemic Systems',
    '8' = 'Eye',
    '9' = 'Ear, Nose, Sinus',
    'B' = 'Respiratory System',
    'C' = 'Mouth and Throat',
    'D' = 'Gastrointestinal System',
    'F' = 'Hepatobiliary System and Pancreas',
    'G' = 'Enbdocrine System',
    'H' = 'Skin and Breast',
    'J' = 'Subcutaneous Tissue and Fascia',
    'K' = 'Muscles',
    'L' = 'Tendons',
    'M' = 'Bursae and Ligaments',
    'N' = 'Head and Facial Bones',
    'P' = 'Upper Bones',
    'Q' = 'Lower Bones',
    'R' = 'Upper Joints',
    'S' = 'Lower Joints',
    'T' = 'Urinary System',
    'U' = 'Female Reproductive System',
    'V' = 'Male Reproductive System',
    'W' = 'Anatomical Regions, General',
    'X' = 'Anatomical Regions, Upper Extremeties',
    'Y' = 'Anatomical Regions, Lower Extremeties')
  
}

sys.1 <- function() { # Obstetrics
  
  vctrs::vec_c('0' = 'Pregnancy')
  
}

sys.2 <- function() { # Placement
  
  vctrs::vec_c('W' = 'Anatomical Regions',
               'Y' = 'Anatomical Orifices')
  
}
