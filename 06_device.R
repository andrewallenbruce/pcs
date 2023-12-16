#' Return the PCS body system for section Medical & Surgical
#' @return character vector of a valid PCS body system or `NA`
#' @autoglobal
#' @noRd
dev.0 <- function() {
  
  vctrs::vec_c(
    '0' = 'Drainage Device',
    # '1' = 'Cerebral Meninges',
    '2' = 'Monitoring Device',
    '3' = 'Infusion Device',
    '4' = 'Radioactive Element, Cesium-131 Collagen Implant',
    # '5' = 'Subarachnoid Space, Intracranial',
    # '6' = 'Cerebral Ventricle',
    '7' = 'Autologous Tissue Substitute',
    # '8' = 'Basal Ganglia',
    # '9' = 'Thalamus',
    # 'A' = 'Hypothalamus',
    # 'B' = 'Pons',
    # 'C' = 'Cerebellum',
    # 'D' = 'Medulla Oblongata',
    # 'E' = 'Cranial Nerve',
    # 'F' = 'Olfactory Nerve',
    # 'G' = 'Optic Nerve',
    # 'H' = 'Oculomotor Nerve',
    'J' = 'Synthetic Substitute',
    'K' = 'Nonautologous Tissue Substitute',
    # 'L' = 'Abducens Nerve',
    'M' = 'Neurostimulator Lead',
    # 'N' = 'Acoustic Nerve',
    # 'P' = 'Glossopharyngeal Nerve',
    # 'Q' = 'Vagus Nerve',
    # 'R' = 'Accessory Nerve',
    # 'S' = 'Hypoglossal Nerve',
    # 'T' = 'Spinal Meninges',
    # 'U' = 'Spinal Canal',
    # 'V' = 'Spinal Cord',
    # 'W' = 'Cervical Spinal Cord',
    # 'X' = 'Thoracic Spinal Cord',
    'Y' = 'Other Device',
    'Z' = 'No Device')
  
}
