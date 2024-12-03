##### NCPES_linkage 

## 0. Set up

library(svDialogs)
library(NDRSAfunctions)
library(tidyverse)
library(dplyr)
library(openxlsx)


# step 1 - inputs 
year <- dlgInput("What year of NCPES data are you linking? YYYY")$res
cas_snapshot <- dlgInput("Which CAS snapshot are you linking to? Format should be like 'cas2408'")$res
analyst <- dlgInput("What is your CAS username? Format should be 'analysisfirstnamesurname'")$res


# step 2 - creating a folder for this year's linkage outputs  
dir.create(paste0(Sys.getenv("ncpes_linkage_drive"), "/Linkage ", year))


# step 3 - CAS connection
cas <- createConnection(username = analyst, port = 1525, sid = cas_snapshot)


# step 4 - create function to extract data frames from list 
dfs_from_list <- function(dataset_list) {
  for (name in names(dataset_list)) {
    assign(name, dataset_list[[name]], envir = .GlobalEnv)
  }
}
