##### NCPES_linkage 

## 4. Flags

##### step 1 - flagging patients matched on NHS number and most recent tumour ICD-10 code (4 or 3 digit)
flag1 <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER, MININTERVFLAG) |>
    mutate(count = ave(ATTUM_NHSNUMBER, ATTUM_NHSNUMBER, FUN = length)) |>
    mutate(DUPAVNHSFINAL = case_when(row_number() == 1 & MININTERVFLAG == 1 & count==1 ~ 0, 
                                      MININTERVFLAG == 1 ~ row_number(), 
                                      TRUE ~ NA))
}

dataset_list <- lapply(dataset_list, flag1)
# If two tumour ids have same mininterval (ie same diagnosis and discharge dates) then there will be duplicates

 
##### step 2 - flag which is 1 if DUPAVNHSFINAL is not empty and if there is a tumour match
flag2 <- function(x) {
  x |>
    mutate(PATMATCH = case_when(!is.na(DUPAVNHSFINAL) & TUMOUR_LINKFLAG_COMBINED == 1 ~ 1,
                                MERGE == 0 ~ 9999))
}

dataset_list <- lapply(dataset_list, flag2)


##### step 3 - final flag which is 0 if there is only row for a patient, otherwise records number of rows for the patient
flag3 <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER) |>
    mutate(FINAL_UNIQUE = ifelse(n() == 1 & PATMATCH == 1, 0,
                                 ifelse(PATMATCH == 1, row_number(), NA))) |>
    ungroup() 
}

dataset_list <- lapply(dataset_list, flag3)


##### step 4 - replace the final flag with 1 for single matched records and 0 where there are multiple rows for the patient
flag4 <- function(x) {
  x |>
    mutate(FINAL_UNIQUE = ifelse(FINAL_UNIQUE == 0, 1, 
                                 ifelse(FINAL_UNIQUE > 1, 0, 
                                        ifelse(is.na(FINAL_UNIQUE), 0, FINAL_UNIQUE)))) |>
    mutate(FINAL_UNIQUE = ifelse(MERGE == 0, 99, FINAL_UNIQUE))
}

dataset_list <- lapply(dataset_list, flag4)


##### step 5 - flagging the non-matched records
flag5 <- function(x) {
  x |>
    mutate(MISSINGUNIQUE = ifelse(FINAL_UNIQUE == 0, "Matched", 
                           ifelse(FINAL_UNIQUE %in% c(1, 99), "Non-tumour matched", NA)))
}

dataset_list <- lapply(dataset_list, flag5)





