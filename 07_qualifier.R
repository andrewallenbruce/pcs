#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
qual.0 <- function() {
  
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
    # 'A' = 'Hypothalamus',
    'B' = 'Cerebral Cisterns',
    # 'C' = 'Cerebellum',
    # 'D' = 'Medulla Oblongata',
    # 'E' = 'Cranial Nerve',
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
    # 'T' = 'Spinal Meninges',
    # 'U' = 'Spinal Canal',
    # 'V' = 'Spinal Cord',
    # 'W' = 'Cervical Spinal Cord',
    'X' = 'Diagnostic',
    # 'Y' = 'Other Device',
    'Z' = 'No Qualifier')
  
}
