#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
part.0 <- function() {
  
  vctrs::vec_c(
    '0' = 'Brain',
    '1' = 'Cerebral Meninges',
    '2' = 'Dura Mater',
    '3' = 'Epidural Space, Intracranial',
    '4' = 'Subdural Space, Intracranial',
    '5' = 'Subarachnoid Space, Intracranial',
    '6' = 'Cerebral Ventricle',
    '7' = 'Cerebral Hemisphere',
    '8' = 'Basal Ganglia',
    '9' = 'Thalamus',
    'A' = 'Hypothalamus',
    'B' = 'Pons',
    'C' = 'Cerebellum',
    'D' = 'Medulla Oblongata',
    'E' = 'Cranial Nerve',
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
    'T' = 'Spinal Meninges',
    'U' = 'Spinal Canal',
    'V' = 'Spinal Cord',
    'W' = 'Cervical Spinal Cord',
    'X' = 'Thoracic Spinal Cord',
    'Y' = 'Lumbar Spinal Cord')
  
}

#' Return the PCS body part for section Obstetrics
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
part.1 <- function() {
  
  vctrs::vec_c(
    '0' = 'Productions of Conception',
    '1' = 'Productions of Conception, Retained',
    '2' = 'Productions of Conception, Ectopic')
  
}
