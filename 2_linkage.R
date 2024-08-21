##### NCPES_linkage 

## 2. Linkage

##### step 1 - link on NHS number 
initial_linkage <- function(x) { 
  x |> 
      mutate(MERGE = ifelse(CPES_NHS_NUMBER == ATTUM_NHSNUMBER, 1, 0)) |>
      mutate(MERGE = ifelse(is.na(ATTUM_NHSNUMBER), 0, MERGE))
  }

dataset_list <- lapply(dataset_list, initial_linkage)


##### step 2 - match on 4 digit ICD-10 code 
icd10_4digit_match <- function(x) {
  x |>
      mutate(FLAGICD104 = case_when(ATTUM_DIAGNOSISYEAR <  2013 & 
                                    CPES_ICD10 == ATTUM_SITE_ICD10_O2_PRE2013 ~ 1,
                                    ATTUM_DIAGNOSISYEAR >= 2013 & 
                                    CPES_ICD10 == ATTUM_SITE_ICD10R4_O2_FROM2013 ~ 1,
                                    TRUE ~ 0))
}

dataset_list <- lapply(dataset_list, icd10_4digit_match)


##### step 3 - match on 3 digit ICD-10 code if there is no 4 digit ICD-10 code match
icd10_3digit_match <- function(x) {
  x |>
    mutate(FLAGICD103 = case_when(FLAGICD104 == 0 & ATTUM_DIAGNOSISYEAR <  2013 & 
                                  CPES_ICD10_3CODE == ATTUM_SITE_ICD10_O2_3CHAR_PRE2013 ~ 1,
                                  FLAGICD104 == 0 & ATTUM_DIAGNOSISYEAR >= 2013 & 
                                  CPES_ICD10_3CODE == ATTUM_SITE_ICD10R4_O2_3CHAR_FROM2013 ~ 1,
                                  TRUE ~ 0))
}

dataset_list <- lapply(dataset_list, icd10_3digit_match)


##### step 4 - creating flag for linkage by ICD-10 code and replace with NA if there is no NHS number match
tumour_link_flag <- function(x) {
  x |>
    mutate(TUMOUR_LINKFLAG_COMBINED = ifelse(FLAGICD104==1 | FLAGICD103==1, 1, 0)) |>
    mutate(TUMOUR_LINKFLAG_COMBINED = ifelse(MERGE==0, NA, TUMOUR_LINKFLAG_COMBINED))
  
}

dataset_list <- lapply(dataset_list, tumour_link_flag)


##### step 5 - create IDs to count matches by ICD-10 4 and 3 digit codes, and unmatched ICD-10 codes
#create a unique ID that counts number of unique NHS numbers for those patients that have a match on 4/3 digit ICD10 code 
#(if there are duplicate NHS numbers the ID ID will be repeated for these subsequent rows)
unique_ids <- function(x) {
  x |>
    mutate(UNIQUE_ID1 = ifelse(FLAGICD104 == 1, cur_group_id(), NA_integer_)) |>
    mutate(UNIQUE_ID2 = ifelse(FLAGICD103 == 1, cur_group_id(), NA_integer_))
}

dataset_list <- lapply(dataset_list, unique_ids)


