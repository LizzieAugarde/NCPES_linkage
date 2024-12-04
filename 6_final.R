##### NCPES_linkage 

## 6. Final cleaning and write out

##### step 1 - dropping variables and capitalising
drop_vars <- function(x) {
  x <- x |>
    select(c("CPES_PRN", "CPES_TRUST_CODE", "ATTUM_PATIENTID", 
                         "ATTUM_TUMOURID", "MERGE", "FLAGICD104", "FLAGICD103", 
                         "TUMOUR_LINKFLAG_COMBINED", "DIFFDIAGDISCH", "PATMATCH", 
                         "FINAL_UNIQUE", "FLAG_RELATED_MATCH", 
                         "FLAG_RELATED_MATCH_UNIQ", "FINAL_UNIQUE_EXTRA"))
}

dataset_list <- lapply(dataset_list, drop_vars)


##### step 2 - capitalise variable names 
capitalise <- function(x) {
  colnames(x) <- toupper(colnames(x))
  return(x)
}

dataset_list <- lapply(dataset_list, capitalise)


##### step 3- extract the data frames from the dataset list for write out
dfs_from_list(dataset_list)


##### step 4 - write out 
filepath <- paste0(Sys.getenv('ncpes_linkage_drive'), "/Linkage ", year)

resps_filename <- paste0("NC_", year, "_LINKAGE_RES_", cas_snapshot, ".xlsx")
nonresps_filename <- paste0("NC_", year, "_LINKAGE_NONRES_", cas_snapshot, ".xlsx")
u16_resps_filename <- paste0("NC_U16_", year, "_LINKAGE_RES_", cas_snapshot, ".xlsx")
u16_nonresps_filename <- paste0("NC_U16_", year, "_LINKAGE_NONRES_", cas_snapshot, ".xlsx")

write.xlsx(resps_raw, file = paste0(filepath, "/", resps_filename), rowNames = FALSE)
write.xlsx(nonresps_raw, file = paste0(filepath, "/", nonresps_filename), rowNames = FALSE)
write.xlsx(u16_resps_raw, file = paste0(filepath, "/", u16_resps_filename), rowNames = FALSE)
write.xlsx(u16_nonresps_raw, file = paste0(filepath, "/", u16_nonresps_filename), rowNames = FALSE)
