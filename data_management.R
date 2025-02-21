library(osfr)
library(tidyverse)


## check if OSF project is reachable with the token
project <- osf_retrieve_node("swczk")


osf_ls_files(project)

osf_ls_nodes(project)[1]

datacomp <- osf_create_component(project, "Data")


## use this to upload data to OSF
#osf_upload(x = datacomp, path = list.files(path = "data/", full.names = TRUE),recurse = TRUE, progress = TRUE, verbose = TRUE)

## use this to download data from OSF if it does not yet exist
datapath <- osf_ls_files(datacomp)
if (!dir.exists("data")){dir.create("data")}

osf_download(datapath, path = "data/", recurse = TRUE, progress = TRUE, verbose = TRUE)
