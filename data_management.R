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



## or a public version

cloudR::download_and_extract_zip(url = "https://files.de-1.osf.io/v1/resources/9dw2j/providers/osfstorage/?view_only=015b3489509c42cbb3d7bb38f0e02d39&zip=",dest_folder = "data/old" , zip_name = "data.zip")


## use this to upload survey data to OSF
 osf_retrieve_node("9dw2j") %>% 
  osf_upload(path = list.files(path = "data/", full.names = TRUE),recurse = TRUE, progress = TRUE, verbose = TRUE, conflicts = "skip")

## for all other secondary data
osf_retrieve_node("e2zvy")  %>%  
  osf_upload(path = list.files(path = "data/", full.names = TRUE),recurse = TRUE, progress = TRUE, verbose = TRUE, conflicts = "skip")



## for all estimated models as RDS files.
osf_retrieve_node("4zu7w")  %>%  
  osf_upload(path = list.files(path = "data/", full.names = TRUE),recurse = TRUE, progress = TRUE, verbose = TRUE, conflicts = "skip")




