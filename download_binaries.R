### You can run this script if you want to download the data, secondary gis data, and model results.


options(timeout = 600) 

#to easily download and extract the files, you can use the package cloudR

if(!require("cloudR")) {
  devtools::install_git('https://github.com/sagebiej/cloudR')
}


####### download a public version of the files


## survey data
cloudR::download_and_extract_zip(url = "https://files.de-1.osf.io/v1/resources/9dw2j/providers/osfstorage/?view_only=015b3489509c42cbb3d7bb38f0e02d39&zip=",dest_folder = "data/old" , zip_name = "data.zip")


## spatial data

shapefiles <- "https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/6808b9baa1768f2bd839a7c1/?view_only=f08b8c286f7a49279b5e472826c9090c&zip="

cloudR::download_and_extract_zip(url = shapefiles ,dest_folder = "secondary_data/germany_shapefiles/old" , zip_name = "shape.zip")


## census data
if(!dir.exists("secondary_data/census")) {
  dir.create("secondary_data/census" ,recursive = T)
  
  cat("Downloading census data...\n")
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/682dd4be76df44ca46b06cdd/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "census.zip", dest_folder = "secondary_data/census/old")
}


## Alkis data

if(!dir.exists("secondary_data/ALKIS")) {
  dir.create("secondary_data/ALKIS" ,recursive = T)
  
  cat("Downloading ALKIS...\n")
  
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/68c2d640ffce999ac0a6c1bc/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "alkis.zip", dest_folder = "secondary_data/ALKIS/old")
}


## vg250gem

if(!dir.exists("secondary_data/vg250_gem/")) {
  dir.create("secondary_data/vg250_gem/" ,recursive = T)
  
  cat("Downloading vg250_gem...\n")
  
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/68c2d65273f5eb91955d8180/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "alkis.zip", dest_folder = "secondary_data/vg250_gem/old")
}

## ZIP data
if (!dir.exists("secondary_data/PLZ_data")) {
  dir.create("secondary_data/PLZ_data", recursive = TRUE)
  
  csv_url <- "https://public.opendatasoft.com/explore/dataset/georef-germany-postleitzahl/download/?format=csv&timezone=Europe/Berlin&lang=en"
  destfile <- "secondary_data/PLZ_data/georef-germany-postleitzahl.csv"
  
  cat("Downloading PLZ csv file...\n")
  download.file(csv_url, destfile, mode = "wb")
}


## intermediate results

### only germany aggreated WTP (small)

if (!dir.exists("intermediate_data/aggregation")) {
  dir.create("intermediate_data/aggregation", recursive = TRUE)
  
  agg_url <- "https://osf.io/download/68c2e47ed4b52bccb1d65d4a/?view_only=321a19cb246b40a487002eaad027f83a"
  destfile <- "intermediate_data/aggregation/germanykm_agg.rds"
  
  cat("Downloading aggregation file...\n")
  download.file(agg_url, destfile, mode = "wb")
}
  
  

## model results

modelfiles <- "https://files.de-1.osf.io/v1/resources/4zu7w/providers/osfstorage/?view_only=0d8b994943634bf880da24bf739a9715&zip="


cloudR::download_and_extract_zip(url = modelfiles ,dest_folder = "Results/old" , zip_name = "results.zip")



# final data 1gb

dir.create("finaldata", showWarnings = FALSE)
download.file("https://osf.io/download/68d11b2611b72e9f4f74e860/?view_only=25375f7fb6c84c92818b5b0f7d0c01e2", destfile = "finaldata/all_datasets.RData", mode = "wb")
