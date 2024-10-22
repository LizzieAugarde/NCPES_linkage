


##### step 1 - flagging patients matched on NHS number and most recent tumour ICD-10 code (4 or 3 digit)
flag1 <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER, MININTERVFLAG) |>
    mutate(count=ave(AVTUM_NHSNUMBER, AVTUM_NHSNUMBER, FUN = length)) |>
    mutate(DUPAVNHSFINAL = case_when(row_number() == 1 & MININTERVFLAG == 1 & count==1 ~ 0, 
                                      MININTERVFLAG == 1 ~ row_number(), 
                                      TRUE ~ NA))
}

dataset_list <- lapply(dataset_list, flag1)
# If two tumour ids have same mininterval then there will be duplicates

 
##### step 2 - flag which is 1 if DUPAVNHSFINAL is not empty and if there is a tumour match
flag2 <- function(x) {
  x |>
    mutate(PATMATCH = case_when(!is.na(DUPAVNHSFINAL) & TUMOUR_LINKFLAG_COMBINED == 1 ~ 1,
                                MERGE == 0 ~ 9999))
}


##### step 3 - final flag which is 0 if there is only row for a patient, otherwise records number of rows for the patient
flag3 <- function(x) {
  x |>
    group_by(UNIQUENHS) |>
    mutate(FINAL_UNIQUE = ifelse(n() == 1 & PATMATCH == 1, 0,
                                 ifelse(PATMATCH == 1, row_number(), NA))) |>
    ungroup() 
}

##### step 4 - replace the final flag
flag4 <- function(x) {
  x |>
    mutate(FINAL_UNIQUE = ifelse(FINAL_UNIQUE == 0, 1, 
                                 ifelse(FINAL_UNIQUE > 1, 0, 
                                        ifelse(is.na(FINAL_UNIQUE), 0, FINAL_UNIQUE)))) |>
    mutate(FINAL_UNIQUE = ifelse(MERGE == 0, 99, FINAL_UNIQUE))
}

##### step 5 - flagging and cleaning the non-matched records
flag5 <- function(x) {
  x |>
    mutate(MISSINGUNIQUE = factor(ifelse(FINAL_UNIQUE == 0, 1, 
                                  ifelse(FINAL_UNIQUE %in% c(1,99), 0, NA)),
                                  levels = c(0,1)))
}

##### step 6 - creating a count of rows by NHS number
flag5 <- function(x) {
  x |>
    group_by(AVTUM_NHSNUMBER) |>
    mutate(rowcount = ave(AVTUM_NHSNUMBER, AVTUM_NHSNUMBER, fun = length)) |>
    arrange(AVTUM_NHSNUMBER) |>
    mutate(SERIALNOTMATCH = ifelse(row_number() == 1 & rowcount == 1, 0, row_number()))
}
  
  
}