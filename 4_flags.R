


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
