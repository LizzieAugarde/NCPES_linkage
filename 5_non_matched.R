##### NCPES_linkage 

## 5. Cleaning non-matched records

##### step 1 - creating a count of rows by NHS number
rows_count <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER) |>
    mutate(rowcount = ave(ATTUM_NHSNUMBER, ATTUM_NHSNUMBER, FUN = length)) |>
    arrange(ATTUM_NHSNUMBER) |>
    mutate(SERIALNOTMATCH = ifelse(row_number() == 1 & rowcount == 1, 0, row_number()))
}

dataset_list <- lapply(dataset_list, rows_count)


##### step 2 - create a unique NHS variable per patient
count_unique_nhs <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER) |>
    mutate(NHS = ifelse(MISSINGUNIQUE == "Matched", cur_group_id(), NA)) |>
    ungroup() |>
    group_by(ATTUM_NHSNUMBER, MISSINGUNIQUE) |>
    mutate(NHS1 = ifelse(MISSINGUNIQUE == "Matched" & !all(is.na(ATTUM_NHSNUMBER)), 
                         min(ATTUM_NHSNUMBER, na.rm = TRUE), NA)) |>
    ungroup() |>
    group_by(ATTUM_NHSNUMBER) |>
    mutate(NHS2 = mean(as.numeric(NHS1), na.rm = TRUE), NA) |>
    mutate(NHS2 = ifelse(NHS2=="NaN", NA, NHS2)) |>
    ungroup()
}

dataset_list <- lapply(dataset_list, count_unique_nhs)


##### step 3 - flag where patients were only non-tumour matched 
count_unique_nhs <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER) |>
    mutate(ONLY_NON_TUMOUR_MATCHED = ifelse(MISSINGUNIQUE =="Non-tumour matched" & (NHS2 == NHS1 | (is.na(NHS1) & is.na(NHS2))), 1,0))  
    mutate(ONLY_NON_TUMOUR_MATCHED = ifelse(is.na(ONLY_NON_TUMOUR_MATCHED), 0, ONLY_NON_TUMOUR_MATCHED)) |>
    mutate(FLAG = sum(ONLY_NON_TUMOUR_MATCHED)) |>
    group_by(ATTUM_NHSNUMBER) |>
    arrange(ATTUM_NHSNUMBER) |>
    ungroup() |>
    mutate(ONLY_NON_TUMOUR_MATCHED = ifelse(FLAG > 1 & SERIALNOTMATCH > 1, NA, ONLY_NON_TUMOUR_MATCHED))
}

dataset_list <- lapply(dataset_list, count_unique_nhs)


##### step 4 - combining tumour site codes from pre- and post-2013
combined_site_code <- function(x) {
  x |>
    mutate(ICD_COMBINED = ifelse(ATTUM_DIAGNOSISYEAR < 2013, ATTUM_SITE_ICD10_O2_3CHAR_PRE2013, ATTUM_SITE_ICD10R4_O2_3CHAR_FROM2013))
}

dataset_list <- lapply(dataset_list, combined_site_code)


##### step 5- extract the data frames from the dataset list for the next step
dfs_from_list(dataset_list)


##### step 6 - table of mean difference between diagnosis and discharge by tumour, for rule for non-matched patients
#6a mean tables
mean_diffdiagdisch_resp <- resps_raw |>
  subset(FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0) |>
  aggregate(DIFFDIAGDISCH ~ ICD_COMBINED, FUN = mean)

mean_diffdiagdisch_nonresp <- nonresps_raw |>
  subset(FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0) |>
  aggregate(DIFFDIAGDISCH ~ ICD_COMBINED, FUN = mean)

mean_diffdiagdisch_u16_resp <- u16_resps_raw |>
  subset(FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0) |>
  aggregate(DIFFDIAGDISCH ~ ICD_COMBINED, FUN = mean)

mean_diffdiagdisch_u16_nonresp <- u16_nonresps_raw |>
  subset(FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0) |>
  aggregate(DIFFDIAGDISCH ~ ICD_COMBINED, FUN = mean)

#6b colorectal mean (see README)
colorectal_mean_resp <- mean(subset(resps_raw, FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0 & ICD_COMBINED %in% c("C18", "C19", "C20"))$DIFFDIAGDISCH)
colorectal_mean_nonresp <- mean(subset(nonresps_raw, FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0 & ICD_COMBINED %in% c("C18", "C19", "C20"))$DIFFDIAGDISCH)
colorectal_mean_u16_resp <- mean(subset(u16_resps_raw, FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0 & ICD_COMBINED %in% c("C18", "C19", "C20"))$DIFFDIAGDISCH)
colorectal_mean_u16_nonresp <- mean(subset(u16_nonresps_raw, FINAL_UNIQUE == 1 & DIFFDIAGDISCH >= 0 & ICD_COMBINED %in% c("C18", "C19", "C20"))$DIFFDIAGDISCH)


##### step 7 - adjusting flags for tumours with more stringent timing rules (see README)
#7a function
flagged_tumours_rule <- function(x, mean_table, colorectal_mean) {
  #setting mean values for comparison (errors otherwise if there are no patients with a tumour type)
  c67_mean <- ifelse(length(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C67"]) == 0 || is.null(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C67"]), NA, 
                     mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C67"])
  c50_mean <- ifelse(length(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C50"]) == 0 || is.null(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C50"]), NA, 
                     mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C50"])
  c82_mean <- ifelse(length(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C82"]) == 0 || is.null(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C82"]), NA, 
                     mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C82"])
  c83_mean <- ifelse(length(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C83"]) == 0 || is.null(mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C83"]), NA, 
                     mean_table$DIFFDIAGDISCH[mean_table$ICD_COMBINED == "C83"])
  
  x <- x |>
    mutate(FLAG_RELATED_MATCH = case_when(
    
    #bladder
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C67" & ICD_COMBINED == "D41" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c67_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED==1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "D41" & ICD_COMBINED == "C67" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c67_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C67" & ICD_COMBINED == "D09" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c67_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "D09" & ICD_COMBINED=="C67" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c67_mean)~1,
    
    #breast
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C50" & ICD_COMBINED == "D05" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c50_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "D05" & ICD_COMBINED == "C50" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c50_mean)~1,
    
    #colorectal
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C18" & ICD_COMBINED == "C20" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C20" & ICD_COMBINED == "C18" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C18" & ICD_COMBINED == "C19" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C19" & ICD_COMBINED == "C18" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C20" & ICD_COMBINED == "C19" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C19" & ICD_COMBINED == "C20" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= colorectal_mean)~1,
    
    #lymphoma
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C82" & ICD_COMBINED == "C83" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c83_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C83" & ICD_COMBINED == "C82" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c83_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C85" & ICD_COMBINED == "C82" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c82_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C82" & ICD_COMBINED == "C85" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c82_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C83" & ICD_COMBINED == "C85" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c83_mean)~1,
    (ONLY_NON_TUMOUR_MATCHED == 1 | is.na(ONLY_NON_TUMOUR_MATCHED)) & 
      CPES_ICD10_3CODE == "C85" & ICD_COMBINED == "C83" & 
      (DIFFDIAGDISCH >= -30 & DIFFDIAGDISCH <= c83_mean)~1,
    TRUE ~ 0))
    
  return(x)
}


flagged_tumours_rule(resps_raw, mean_diffdiagdisch_resp, colorectal_mean_resp)
flagged_tumours_rule(nonresps_raw, mean_diffdiagdisch_nonresp, colorectal_mean_nonresp)
flagged_tumours_rule(u16_resps_raw, mean_diffdiagdisch_u16_resp, colorectal_mean_u16resp)
flagged_tumours_rule(u16_nonresps_raw, mean_diffdiagdisch_u16_nonresp, colorectal_mean_u16nonresp)

##### step 8 - cleaning duplicates, unique observations of unequal tumours which could be matched
unequal_tums_dups <- function(x) {
  x |>
    group_by(ATTUM_NHSNUMBER, FLAG_RELATED_MATCH) |>
    mutate(MININTERV2 = ifelse(FLAG_RELATED_MATCH == 1 & !all(is.na(DIFF)), min(DIFF, na.rm = TRUE), NA)) |>
    ungroup() |>
    mutate(FLAG_RELATED_MATCH_UNIQ = ifelse(DIFFDIAGDISCH == MININTERV2, 1, 0)) |>
    mutate(FLAG_RELATED_MATCH_UNIQ = ifelse(is.na(FLAG_RELATED_MATCH_UNIQ), 0, FLAG_RELATED_MATCH_UNIQ)) |>
    mutate(FLAG_RELATED_MATCH_UNIQ = ifelse(is.na(DIFFDIAGDISCH), NA, FLAG_RELATED_MATCH_UNIQ))
}

dataset_list <- lapply(dataset_list, unequal_tums_dups)


##### step 9 - flagging unequal but matched ICD-10 codes to increase yield
#similar to final_unique variable, but with increased yield from adding the unequal but matched ICD-10 code patients
unequal_tums_flag <- function(x) {
  x |>
    mutate(FINAL_UNIQUE_EXTRA = ifelse(FLAG_RELATED_MATCH_UNIQ == 1,1, FINAL_UNIQUE)) |>
    mutate(FINAL_UNIQUE_EXTRA = ifelse(!is.na(FINAL_UNIQUE_EXTRA), FINAL_UNIQUE_EXTRA, 99))
}

dataset_list <- lapply(dataset_list, unequal_tums_flag)