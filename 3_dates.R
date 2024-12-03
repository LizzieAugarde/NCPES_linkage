##### NCPES_linkage 

## 3. Dates

##### step 1 - create date of discharge and difference between diagnosis and discharge
discharge_date <- function(x) {
  x |>
    mutate(DISCH_DATE = as.POSIXct(paste(CPES_YEAR_OF_DISCHARGE, 
                                           CPES_MONTH_OF_DISCHARGE, 
                                           CPES_DAY_OF_DISCHARGE, 
                                           sep = "-"), format = "%Y-%m-%d")) |>
    mutate(DIFFDIAGDISCH = as.numeric(as.Date(DISCH_DATE)-as.Date(ATTUM_DIAGNOSISDATEBEST)))
}

dataset_list <- lapply(dataset_list, discharge_date)


##### step 2 - minimum non-negative interval between diagnosis and discharge per patient
min_diag_disch_interval <- function(x) {
  x |>
    mutate(DIFF_CHECK = ifelse(DIFFDIAGDISCH>=-30, DIFFDIAGDISCH, NA)) |>
    group_by(ATTUM_NHSNUMBER, TUMOUR_LINKFLAG_COMBINED) |>
    mutate(MININTERV = ifelse(TUMOUR_LINKFLAG_COMBINED == 1 & !all(is.na(DIFF_CHECK)), 
                              min(DIFF_CHECK, na.rm = TRUE), NA)) |>
    ungroup() |>
    mutate(MININTERVFLAG = case_when(DIFFDIAGDISCH == MININTERV ~ 1, 
                                     is.na(DIFFDIAGDISCH) ~ NA, 
                                     TRUE ~ 0))
}

dataset_list <- lapply(dataset_list, min_diag_disch_interval)





