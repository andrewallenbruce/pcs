#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
part.0 <- function() { # Medical/Surgical
  
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

part.1 <- function() { # Obstetrics
  
  vctrs::vec_c(
    '0' = 'Productions of Conception',
    '1' = 'Productions of Conception, Retained',
    '2' = 'Productions of Conception, Ectopic')
  
}

part.2 <- function() { # Placement
  
  w <- vctrs::vec_c(
    '0' = 'Head',
    '1' = 'Face',
    '2' = 'Neck',
    '3' = 'Abdominal Wall',
    '4' = 'Chest Wall',
    '5' = 'Back',
    '6' = 'Inguinal Region, Right',
    '7' = 'Inguinal Region, Left',
    '8' = 'Upper Extremity, Right',
    '9' = 'Upper Extremity, Left',
    'A' = 'Upper Arm, Right',
    'B' = 'Upper Arm, Left',
    'C' = 'Lower Arm, Right',
    'D' = 'Lower Arm, Left',
    'E' = 'Hand, Right',
    'F' = 'Hand, Left',
    'G' = 'Thumb, Right',
    'H' = 'Thumb, Left',
    'J' = 'Finger, Right',
    'K' = 'Finger, Left',
    'L' = 'Lower Extremity, Right',
    'M' = 'Lower Extremity, Left',
    'N' = 'Upper Leg, Right',
    'P' = 'Upper Leg, Left',
    'Q' = 'Lower Leg, Right',
    'R' = 'Lower Leg, Left',
    'S' = 'Foot, Right',
    'T' = 'Foot, Left',
    'U' = 'Toe, Right',
    'V' = 'Toe, Left'
    )
  
  y <- vctrs::vec_c(
    '0' = 'Mouth and Pharynx',
    '1' = 'Nasal',
    '2' = 'Ear',
    '3' = 'Anorectal',
    '4' = 'Female Genital Tract',
    '5' = 'Urethra'
  )
  
  return(list("W" = w, "Y" = y))
  
}