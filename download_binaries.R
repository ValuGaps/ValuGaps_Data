### You can run this script if you want to download the data, secondary gis data, and model results.

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

if(!dir.exists("secondary_data/census_agegroups")) {
  dir.create("secondary_data/census_agegroups" ,recursive = T)
  download.file("https://osf.io/download/gefpk/?view_only=f08b8c286f7a49279b5e472826c9090c", destfile = "secondary_data/census_agegroups/1000A-2027_de.xlsx")
   }


## model results

modelfiles <- "https://files.de-1.osf.io/v1/resources/4zu7w/providers/osfstorage/?view_only=0d8b994943634bf880da24bf739a9715&zip="


cloudR::download_and_extract_zip(url = modelfiles ,dest_folder = "Results/old" , zip_name = "results.zip")
