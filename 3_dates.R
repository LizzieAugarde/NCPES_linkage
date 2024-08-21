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


###########  Making a variable for the minimum value of DIFFDIAGDISCH that is not negative
# 
# There were a number of patients that appeared to have been diagnosed after their discharge date,
# with a large proportion of these patients being in the time window of 1-30 days after their
# discharge. For this reason, patients who were diagnosed over 30 days after their
# discharge date in the CPES dataset were not indicated as a match. This was applied to
# help remove any records that were deemed ‘duplicates’ (by duplicate we mean records
# that were identical for a patient on given variables of interest, and in this case a patient
# that had two or more records that were a match in ICD-10 4-digit or 3-digit level), but also
# to remove any record that matched, but had a date of diagnosis more than 30 days after
# discharge to minimise the possibility of error. 



# MININTERVFLAG = 1 if DIFFDIAGDISCH = MININTERV
CPES_RESPONDENTS          <- CPES_RESPONDENTS       %>% mutate(MININTERVFLAG = ifelse(DIFFDIAGDISCH == MININTERV, 1, 0)) %>%  mutate(MININTERVFLAG = ifelse(is.na(MININTERVFLAG),0,MININTERVFLAG)) %>% mutate(MININTERVFLAG = ifelse(is.na(DIFFDIAGDISCH), NA, MININTERVFLAG))
CPES_NONRESPONDENTS       <- CPES_NONRESPONDENTS    %>% mutate(MININTERVFLAG = ifelse(DIFFDIAGDISCH == MININTERV, 1, 0)) %>%  mutate(MININTERVFLAG = ifelse(is.na(MININTERVFLAG),0,MININTERVFLAG)) %>% mutate(MININTERVFLAG = ifelse(is.na(DIFFDIAGDISCH), NA, MININTERVFLAG))



