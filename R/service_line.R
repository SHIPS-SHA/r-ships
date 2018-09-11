#' Add provider service and service line to dataset
#'
#' Based on a function developed by SHIPS after discussions with health records,
#' the code implemented below adds provider service and service line information
#' to the dataset.
#'
#' This function requires four fields to be present in the data:
#' \code{PatientServiceSubService}, \code{ProviderServiceDesc},
#' \code{ProviderService}, and \code{ProviderNumber}.
#'
#' @param data data.frame and tibble containing hospital records
#' @return The same dataset as input, but with one new variable
#'   (\code{ServiceLine}) and one updated variable (\code{ProviderService}).
#' @export
add_service_line <- function(data) {
  # Need to check whether some variable names are available
  stopifnot(c("PatientServiceSubService",
              "ProviderServiceDesc",
              "ProviderService",
              "ProviderNumber") %in% names(data))
  data %>%
    dplyr::mutate(ProviderService = dplyr::case_when(
      PatientServiceSubService == '51' ~ "Obstetrics Delivered",
      PatientServiceSubService == '52' ~ "Obstetrics Antepartum",
      PatientServiceSubService == '53' ~ "Obstetrics Aborted",
      PatientServiceSubService == '59' ~ "Obstetrics Postpartum",
      ProviderServiceDesc == 'Family Practice/General Practice' & PatientServiceSubService == '54' ~ "Newborn - Family Practice",
      ProviderServiceDesc == 'Family Practice/General Practice' & PatientServiceSubService != '54' ~ "Family Practitioner (Non-Maternal)",
      ProviderServiceDesc == 'Neonatal-Perinatal Medicine' ~ "Neonatologist",
      ProviderServiceDesc == 'Midwifery' ~ "Newborn - Midwifery",
      stringr::str_detect(ProviderServiceDesc, "Dent") ~ "Dental Surgery/Dentistry/Pediatric Dentistry",
      (stringr::str_detect(ProviderServiceDesc, "Paediatric") &
         !stringr::str_detect(ProviderServiceDesc, "Dent")) |
        (PatientServiceSubServiceDesc == 'Paediatrics' & PatientServiceSubService == '54') ~ "Pediatrician",
      ProviderServiceDesc %in% c('Physiatry', 'Physical Medicine & Rehabilitation') ~ "Physiatrist/Physical Med & Rehab",
      PatientServiceSubService == '76' & ProviderService == '00001' ~ "Physiatry",
      ProviderNumber %in% c('20364', '25054', '22199') ~ "Thoracic Surgery",
      ProviderNumber %in% c('20445', '22249', '20761', '25119') ~ "Vascular Surgery",
      TRUE ~ ProviderServiceDesc)
      ) %>%
    dplyr::mutate(ServiceLine = dplyr::case_when(
      ProviderService %in% c("Internal Medicine", "Endocrinology & Metabolism", "Dermatology",
                             "Gastroenterology", "Respirology", "Rheumatology",
                             "Infectious Diseases", "Nephrology") ~ "Adult Medicine",
      ProviderService %in% c("Dental Surgery/Dentistry/Pediatric Dentistry", "General Surgery",
                             "Oral Surgery", "Orthopaedic Surgery", "Plastic Surgery",
                             "Thoracic Surgery", "Urology", "Urogynecologist",
                             "Otolaryngology", "Ophthalmology", "Vascular Surgery",
                             "Obstetrics & Gynaecology") ~ "Surgery Services",
      ProviderService %in% c("Cardiology", "Cardiac Surgery") ~ "Hearth Health",
      ProviderService %in% c("Haematology", "Medical Oncology", "Radiation Oncology") ~ "Oncology",
      ProviderService %in% c("Neurology", "Neurosurgery") ~ "Brain Health",
      ProviderService %in% c("Neonatologist", "Pediatrician", "Newborn - Midwifery",
                             "Newborn - Family Practice") ~ "Children Services",
      ProviderService %in% c("Physiatry", "Physiatrist/Physical Med & Rehab") ~ "Physiatrist/Physical Med & Rehab",
      ProviderService %in% c("Obstetrics Delivered", "Obstetrics Antepartum",
                             "Obstetrics Aborted", "Obstetrics Postpartum") ~ "Maternal Services",
      ProviderService %in% c("Nuclear Medicine", "Diagnostic Radiology") ~ "Radiologist",
      ProviderService == "Psychiatry" ~ "Mental Health",
      TRUE ~ ProviderService)
      )
}
