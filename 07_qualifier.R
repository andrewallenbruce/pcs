#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
qual.0 <- function() { # Medical/Surgical
  
  vctrs::vec_c(
    '0' = 'Nasopharynx',
    '1' = 'Mastoid Sinus',
    '2' = 'Atrium',
    '3' = 'Blood Vessel',
    '4' = 'Pleural Cavity',
    '5' = 'Intestine',
    '6' = 'Peritoneal Cavity',
    '7' = 'Urinary Tract',
    '8' = 'Bone Marrow',
    '9' = 'Fallopian Tube',
    'B' = 'Cerebral Cisterns',
    'F' = 'Olfactory Nerve',
    'G' = 'Optic Nerve',
    'H' = 'Oculomotor Nerve',
    'J' = 'Trochlear Nerve',
    'K' = 'Trigeminal Nerve',
    'L' = 'Abducens Nerve',
    'M' = 'Facial Nerve',
    'N' = 'Acoustic Nerve',
    'P' = 'Glossopharyngeal Nerve',
    'Q' = 'Vagus Nerve',
    'R' = 'Accessory Nerve',
    'S' = 'Hypoglossal Nerve',
    'X' = 'Diagnostic',
    'Z' = 'No Qualifier')
  
}

qual.1 <- function() { # Obstetrics
  
  vctrs::vec_c(
    '0' = 'Classical',
    '1' = 'Low Cervical',
    '2' = 'Extraperitoneal',
    '3' = 'Low Forceps',
    '4' = 'Mid Forceps',
    '5' = 'High Forceps',
    '6' = 'Vacuum',
    '7' = 'Internal Version',
    '8' = 'Other',
    '9' = 'Fetal Blood or Manual',
    'A' = 'Fetal Cerebrospinal Fluid',
    'B' = 'Fetal Fluid, Other',
    'C' = 'Amniotic Fluid, Therapeutic',
    'D' = 'Fluid, Other',
    'E' = 'Nervous System',
    'F' = 'Cardiovascular System',
    'G' = 'Lymphatic and Hemic',
    'H' = 'Eye',
    'J' = 'Ear, Nose, and Sinus',
    'K' = 'Respiratory System',
    'L' = 'Mouth and Throat',
    'M' = 'Gastrointestinal System',
    'N' = 'Hepatobiliary and Pancreas',
    'P' = 'Endocrine System',
    'Q' = 'Skin',
    'R' = 'Musculoskeletal System',
    'S' = 'Urinary System',
    'T' = 'Female Reproductive System',
    'U' = 'Amniotic Fluid, Diagnostic',
    'V' = 'Male Reproductive System',
    'W' = 'Laminaria',
    'X' = 'Abortifacient',
    'Y' = 'Other Body System',
    'Z' = 'No Qualifier')
  
}

qual.2 <- function() { # Placement
  
  vctrs::vec_c('Z' = 'No Qualifier')
  
}