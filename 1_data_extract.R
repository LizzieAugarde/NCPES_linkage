##### NCPES_linkage 

## 1. Raw data extract 

##### step 1 - SQL extracts
resps_query <- paste0("select * from CPES.NC_", year, "_RESPONDENTS@CASREF01 c
                      left outer join ANALYSISNCR.AT_TUMOUR_ENGLAND@", cas_snapshot,
                      " t on replace(to_char(c.CPES_NHS_NUMBER), ' ', '') = t.NHSNUMBER
                      order by c.CPES_NHS_NUMBER")

nonresps_query <- paste0("select * from CPES.NC_", year, "_NONRESPONDENTS@CASREF01 c
                         left outer join ANALYSISNCR.AT_TUMOUR_ENGLAND@", cas_snapshot,
                         " t on replace(to_char(c.CPES_NHS_NUMBER), ' ', '') = t.NHSNUMBER
                         order by c.CPES_NHS_NUMBER")

u16_resps_query <- paste0("select * from CPES.NC_U16_", year, "_RESPONDENTS@CASREF01 c
                          left outer join ANALYSISNCR.AT_TUMOUR_ENGLAND@", cas_snapshot,
                          " t on replace(to_char(c.CPES_NHS_NUMBER), ' ', '') = t.NHSNUMBER
                          order by c.CPES_NHS_NUMBER")

u16_nonresps_query <- paste0("select * from CPES.NC_U16_", year, "_NONRESPONDENTS@CASREF01 c
                             left outer join ANALYSISNCR.AT_TUMOUR_ENGLAND@", cas_snapshot,
                             " t on replace(to_char(c.CPES_NHS_NUMBER), ' ', '') = t.NHSNUMBER
                             order by c.CPES_NHS_NUMBER")

resps_raw <- dbGetQueryOracle(cas, resps_query)
nonresps_raw <- dbGetQueryOracle(cas, nonresps_query)
u16_resps_raw <- dbGetQueryOracle(cas, u16_resps_query)
u16_nonresps_raw <- dbGetQueryOracle(cas, u16_nonresps_query)


##### step 2 - save a copy of raw data
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

adult_excel_file_name <- paste0(Sys.getenv("ncpes_linkage_drive"), "/Linkage ", year, "/CPES_CAS_LINKED_EXTRACT", timestamp, ".xlsx")
u16_excel_file_name <- paste0(Sys.getenv("ncpes_linkage_drive"), "/Linkage ", year, "/CPES_U16_CAS_LINKED_EXTRACT", timestamp, ".xlsx")

write.xlsx(list(Respondents = resps_raw, Nonrespondents = nonresps_raw), 
           file = adult_excel_file_name, rowNames = FALSE)

write.xlsx(list(Respondents = u16_resps_raw, Nonrespondents = u16_nonresps_raw), 
           file = u16_excel_file_name, rowNames = FALSE)


##### step 3 - limit to the fields needed for linkage and rename
dataset_list <- list(resps_raw = resps_raw, 
                     nonresps_raw = nonresps_raw, 
                     u16_resps_raw = u16_resps_raw, 
                     u16_nonresps_raw = u16_nonresps_raw)

limit_cols <- function(x) {
  cols_to_keep <- c("CPES_PRN", "CPES_NHS_NUMBER", "SITE_ICD10_O2", "SITE_ICD10_O2_3CHAR", 
                  "SITE_ICDO3REV2011_3CHAR", "SITE_ICD10_O2_3CHAR_PRE2013", 
                  "SITE_ICD10_O2_PRE2013", "SITE_ICD10R4_O2_FROM2013", 
                  "SITE_ICD10_O2_3CHAR_PRE2013", "SITE_ICD10R4_O2_3CHAR_FROM2013", 
                  "SITE_ICD10", "SITE_ICD10_3CHAR", "NHSNUMBER", "DIAGNOSISYEAR",
                  "DIAGNOSISDATEBEST", "CPES_YEAR_OF_DISCHARGE", 
                  "CPES_MONTH_OF_DISCHARGE", "CPES_DAY_OF_DISCHARGE",
                  "CPES_ICD10", "CPES_ICD10_3CODE")
  
  x |> select(all_of(cols_to_keep))
}

dataset_list <- lapply(dataset_list, limit_cols)


##### step 4 - rename columns to indicate whether they are from CPES data or AT_TUMOUR_ENGLAND
rename_cols <- function(x) {
  current_names <- names(x)
  
  new_names <- 
    ifelse(str_starts(current_names, "CPES"), current_names, 
    paste0("ATTUM_", current_names))
  
  names(x) <- new_names
  return(x)
}

dataset_list <- lapply(dataset_list, rename_cols)


##### step 5 - remove spaces in NHS numbers in CPES data
remove_spaces <- function(x) {
  x |> 
    mutate(CPES_NHS_NUMBER = gsub(" ", "", CPES_NHS_NUMBER))
}

dataset_list <- lapply(dataset_list, remove_spaces)


##### step 6 - extract data frames from list
dfs_from_list(dataset_list)
