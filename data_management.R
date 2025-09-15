####  ONLY FOR EXPERTS WHO HAVE ACCESS TO THE OSF PROJECT #####

# WARNING!!!!!! YOU SHOULD KNOW WHAT YOU ARE DOING BECAUSE IT CAN DESTROY EVERYTHING


library(osfr)
library(tidyverse)


#cloudR::create_ghrepo("ValuGaps_Data", upstream = "git@github.com:sagebiej/ValuGaps_Data.git", collaborators = "ninocav")


if (!dir.exists("data")){dir.create("data")}
## use this to download data from OSF if it does not yet exist
## check if OSF project is reachable with the token
project <- osf_retrieve_node("swczk") %>%  
  osf_ls_nodes(pattern = "Data") %>% 
  osf_ls_files() %>% 
  osf_download(path = "data/", recurse = TRUE, progress = TRUE, verbose = TRUE)




##### use this to upload survey data to OSF ####
 osf_retrieve_node("9dw2j") %>% 
  osf_upload(path = list.files(path = "data/", full.names = TRUE),recurse = TRUE, progress = TRUE, verbose = TRUE, conflicts = "skip")

## for all other secondary data
osf_retrieve_node("e2zvy")  %>%
  osf_mkdir("germany_shapefiles") %>% 
  osf_upload(path = list.files(path = "secondary_data/germany_shapefiles", full.names = TRUE, recursive = T ),recurse = TRUE, progress = TRUE, verbose = TRUE, conflicts = "skip")



## for intermediate results e.g. for aggregation

files_int <- list.files(path ="intermediate_data/", full.names = TRUE)

osf_retrieve_node("pax8y")  %>%
  osf_mkdir("aggregation") %>% 
  osf_upload(path = files_int ,  progress = TRUE, verbose = TRUE, conflicts = "replace") 


## for all estimated models as RDS files.

files <- list.files(path = "Results/MXL", full.names = T)
filtered_files <- files[!grepl("old", files, ignore.case = TRUE) & grepl("\\.rds$", files, ignore.case = TRUE)]


osf_retrieve_node("4zu7w")  %>%
  osf_mkdir("MXL") %>% 
  osf_upload(path = filtered_files ,  progress = TRUE, verbose = TRUE, conflicts = "replace")
 


## conditional logit

filescl <- list.files(path = "Results/Clogit/WTP_space/", full.names = T)
filtered_filescl <- filescl[!grepl("old", filescl, ignore.case = TRUE) & grepl("\\.rds$", filescl, ignore.case = TRUE) ]


osf_retrieve_node("4zu7w")  %>%
  osf_mkdir("Clogit/WTP_space") %>% 
  osf_upload(path = filtered_filescl, progress = TRUE, verbose = TRUE, conflicts = "skip")

## Hybrid models

filescl <- list.files(path = "Results/HC", full.names = T)
filtered_filescl <- filescl[!grepl("old", filescl, ignore.case = TRUE) & grepl("\\.rds$", filescl, ignore.case = TRUE) ]



osf_retrieve_node("4zu7w")  %>%
  osf_mkdir("HC") %>% 
  osf_upload(path = "Output/Estimation_results/HC/Hybrid_with_OL_model.rds", progress = TRUE, verbose = TRUE, conflicts = "skip")  

  

