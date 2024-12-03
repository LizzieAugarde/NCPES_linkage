##### NCPES_linkage 

## 0. Set up

library(svDialogs)
library(NDRSAfunctions)
library(tidyverse)
library(dplyr)

# put the NCPES linkage filepath into your renviron
year <- dlgInput("What year of NCPES data are you linking? YYYY", Sys.info()["year"])$res


# put inputs into the inputs.csv file in the folder
inputvalues <- read.csv("inputs.csv") ##for RAP process development only 
#####inputvalues <- read.csv(paste0(Sys.getenv('ncpes_linkage_drive'), "_", year, '/inputs.csv'))
cas_snapshot <- inputvalues[inputvalues$input == "cas_snapshot", 2]
analyst <- inputvalues[inputvalues$input == "cas_username", 2]


# CAS connection
cas <- createConnection(username = analyst, port = 1525, sid = paste0("cas", cas_snapshot))


# Function to extract data frames from list 
dfs_from_list <- function(dataset_list) {
  for (name in names(dataset_list)) {
    assign(name, dataset_list[[name]], envir = .GlobalEnv)
  }
}