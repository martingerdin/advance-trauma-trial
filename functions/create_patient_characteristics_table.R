#' Create a shell table of patient baseline characteristics
#'
#' Builds a shell (template) table illustrating how patient baseline
#' characteristics will be summarised before and after the ATLS training is
#' implemented in a cluster. Where possible the variable type and the response
#' options are pulled from the REDCap trial-data dictionary (metadata), so the
#' table stays in sync with the data actually collected. A few characteristics
#' are not single dictionary fields and are supplied directly: the Injury
#' Severity Score is derived from the recorded injury diagnoses, imaging is
#' summarised by modality from the imaging form, and the mechanism of injury is
#' recorded as an ICD-10 code and so is shown using the grouped categories it
#' will be collapsed into for reporting. The table is laid out with
#' `gtsummary`; the body cells are then blanked, because this is an analysis
#' plan rather than a report and no data are summarised yet, leaving the
#' statistic label in each row to document the format that will be reported.
#'
#' @param data A data frame or NULL. The REDCap trial-data dictionary
#'     (metadata), with the columns `field_name`, `field_type` and
#'     `select_choices_or_calculations`. If NULL (the default) the dictionary is
#'     fetched from REDCap with `get_redcap_data()` using `url.name` and
#'     `api.key.name`. Pass a data frame to supply the dictionary directly, for
#'     example when testing offline.
#' @param url.name Character. Name of the environment variable holding the
#'     REDCap API URL for the trial-data project. Used only when `data` is NULL.
#'     Defaults to "TGI_REDCAP_URL".
#' @param api.key.name Character. Name of the environment variable holding the
#'     REDCap API token for the trial-data project. Used only when `data` is
#'     NULL. Defaults to "TGI_REDCAP_TRIAL_DATA_API_KEY".
#' @param groups Character. Labels for the two strata, given before and after
#'     ATLS training is implemented in a cluster. Defaults to
#'     c("Before ATLS", "After ATLS").
#' @param mechanism.levels Character. The grouped mechanism-of-injury categories
#'     to show, because the underlying field is a full ICD-10 code list that is
#'     collapsed for reporting. Defaults to a provisional grouping that can be
#'     refined once the reporting categories are fixed.
#' @param all Logical. If FALSE (the default), the table shows the key patient
#'     characteristics selected for the main results. If TRUE, the table shows all
#'     non-outcome characteristics relevant for supplementary reporting
#'     (outcomes, administrative fields and free-text fields are excluded).
#' @param include.overall Logical. If TRUE an "Overall" column is appended.
#'     Defaults to TRUE.
#' @return A `gtsummary` table object (class `tbl_summary`), or, for PDF/LaTeX
#'     output, a `kableExtra` table sized to fit the text block.
#'
#' @examples
#' ## Load all project functions first
#' noacsr::source_all_functions()
#'
#' \dontrun{
#' ## Key characteristics (requires the API token in a project-level .env file)
#' create_patient_characteristics_table()
#'
#' ## All non-outcome characteristics for supplementary tables
#' create_patient_characteristics_table(all = TRUE)
#' }
create_patient_characteristics_table <- function(data = NULL,
                                                 url.name = "TGI_REDCAP_URL",
                                                 api.key.name = "TGI_REDCAP_TRIAL_DATA_API_KEY",
                                                 groups = c("Before ATLS", "After ATLS"),
                                                 mechanism.levels = c(
                                                     "Road traffic injury",
                                                     "Fall",
                                                     "Assault",
                                                     "Other"
                                                 ),
                                                 all = FALSE,
                                                 include.overall = TRUE) {
    assertthat::assert_that(is.null(data) || is.data.frame(data))
    assertthat::assert_that(is.character(url.name) && length(url.name) == 1)
    assertthat::assert_that(is.character(api.key.name) && length(api.key.name) == 1)
    assertthat::assert_that(is.character(groups) && length(groups) >= 2)
    assertthat::assert_that(is.character(mechanism.levels) && length(mechanism.levels) >= 2)
    assertthat::assert_that(is.logical(all) && length(all) == 1)
    assertthat::assert_that(is.logical(include.overall) && length(include.overall) == 1)

    if (is.null(data)) {
        data <- get_redcap_data(
            url.name = url.name,
            api.key.name = api.key.name,
            content = "metadata"
        )
    }

    key.requests <- list(
        list(field = "age", label = "Age (years)", source = "dictionary"),
        list(field = "sex", label = "Sex", source = "dictionary"),
        list(field = "referred", label = "Transferred in", source = "dictionary", summary = "dichotomous"),
        list(field = "mechanism_of_injury", label = "Mechanism of injury",
             source = "external", summary = "categorical", levels = mechanism.levels),
        list(field = "injury_severity_score", label = "Injury Severity Score",
             source = "external", summary = "continuous"),
        list(field = "glasgow_coma_scale", label = "Glasgow Coma Scale score", source = "dictionary"),
        list(field = "systolic_blood_pressure", label = "Systolic blood pressure (mmHg)", source = "dictionary"),
        list(field = "respiratory_rate", label = "Respiratory rate (breaths/min)", source = "dictionary"),
        list(field = "oxygen_saturation", label = "Oxygen saturation (%)", source = "dictionary"),
        list(field = "surgery_done", label = "Surgery", source = "dictionary", summary = "dichotomous"),
        list(field = "transfusion_done", label = "Transfusion", source = "dictionary", summary = "dichotomous"),
        list(field = "imaging", label = "Imaging", source = "external", summary = "categorical",
             levels = c("Ultrasound", "X-ray", "CT"))
    )

    all.requests <- list(
        list(field = "age", label = "Age (years)", source = "dictionary"),
        list(field = "sex", label = "Sex", source = "dictionary"),
        list(field = "marital_status", label = "Marital status", source = "dictionary"),
        list(field = "education_level", label = "Education level", source = "dictionary"),
        list(field = "main_work_status", label = "Main work status", source = "dictionary"),
        list(field = "income_level", label = "Income level (INR per month)", source = "dictionary"),
        list(field = "comorbidities", label = "Comorbidities (Charlson Comorbidity Index)", source = "dictionary"),
        list(field = "liver_disease_severity", label = "Severity of liver disease", source = "dictionary"),
        list(field = "diabetes_severity", label = "Severity of diabetes", source = "dictionary"),
        list(field = "malignancy_severity", label = "Severity of malignancy", source = "dictionary"),
        list(field = "clinical_frailty_scale", label = "Clinical Frailty Scale", source = "dictionary"),
        list(field = "transport_mode", label = "Mode of transport", source = "dictionary"),
        list(field = "referred", label = "Transferred in", source = "dictionary", summary = "dichotomous"),
        list(field = "mechanism_of_injury", label = "Mechanism of injury",
             source = "external", summary = "categorical", levels = mechanism.levels),
        list(field = "injury_severity_score", label = "Injury Severity Score",
             source = "external", summary = "continuous"),
        list(field = "injury_source", label = "Injury source data", source = "dictionary"),
        list(field = "glasgow_coma_scale", label = "Glasgow Coma Scale score", source = "dictionary"),
        list(field = "systolic_blood_pressure", label = "Systolic blood pressure (mmHg)", source = "dictionary"),
        list(field = "diastolic_blood_pressure", label = "Diastolic blood pressure (mmHg)", source = "dictionary"),
        list(field = "heart_rate", label = "Heart rate (beats/min)", source = "dictionary"),
        list(field = "respiratory_rate", label = "Respiratory rate (breaths/min)", source = "dictionary"),
        list(field = "oxygen_saturation", label = "Oxygen saturation (%)", source = "dictionary"),
        list(field = "temperature", label = "Body temperature (°F)", source = "dictionary"),
        list(field = "emergency_department_disposition", label = "Emergency department disposition", source = "dictionary"),
        list(field = "ward", label = "Type of admitting ward", source = "dictionary"),
        list(field = "hospital_disposition", label = "Hospital disposition", source = "dictionary"),
        list(field = "hospital_transferred", label = "Transferred to another hospital", source = "dictionary", summary = "dichotomous"),
        list(field = "surgery_done", label = "Surgery", source = "dictionary", summary = "dichotomous"),
        list(field = "preoperative_asa", label = "Preoperative ASA score", source = "dictionary"),
        list(field = "transfusion_done", label = "Transfusion", source = "dictionary", summary = "dichotomous"),
        list(field = "transfusion_type", label = "Type of blood product", source = "dictionary"),
        list(field = "transfusion_units", label = "Number of units transfused", source = "dictionary"),
        list(field = "imaging", label = "Imaging", source = "external", summary = "categorical",
             levels = c("Ultrasound", "X-ray", "CT"))
    )

    requests <- if (isTRUE(all)) all.requests else key.requests

    build_patient_characteristics_shell_table(
        data = data,
        requests = requests,
        groups = groups,
        include.overall = include.overall,
        longtable = isTRUE(all)
    )
}
