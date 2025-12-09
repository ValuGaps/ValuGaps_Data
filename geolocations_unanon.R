# ------ USE OF FILE ---------------

# Our dataset contains a set of sensible personal information that requires anonymization to protect privacy rights
# In combination the following variables could potentially serve as identifiers in anonymized data

# lat, lon                   # precise location
# q2_1 (res_settlement_type) # settlement type
# postcode, birthyear, gender, married, educ, empl, hhsize, hhnetinc
# pol_btw                     # political affiliation
# house, kids, garden, privateforest, addressexp

# We therefore anonymize the geolocations before publication in order to protect the privacy rights of our respondents


# ---- LOAD packages ########
library(sf)         
library(tidyverse)  # dplyr, tibble, stringr, purrr, ggplot2 
library(leaflet) 
library(RColorBrewer)
library(scales)
library(geosphere)   # for distance calculation
library(readr)
library(readxl)
library(tidylog)
library(openxlsx)
library(fs)
library(terra)

# ---- LOAD necessary data ########

##### Survey data ####

# For data privacy reasons we cannot provide the original survey data files 
# The following script is protected against anonymizing the data twice, hence this script cannot be applied to this data and should the data should not be altered for the script to be applicable to it.

##### Secondary data #####
## Spatial data

### Shapefiles
shapefiles <- "https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/6808b9baa1768f2bd839a7c1/?view_only=f08b8c286f7a49279b5e472826c9090c&zip="
cloudR::download_and_extract_zip(url = shapefiles ,dest_folder = "secondary_data/germany_shapefiles/old" , zip_name = "shape.zip")

### Alkis data
if(!dir.exists("secondary_data/ALKIS")) {
  dir.create("secondary_data/ALKIS" ,recursive = T)
  cat("Downloading ALKIS...\n")
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/68c2d640ffce999ac0a6c1bc/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "alkis.zip", dest_folder = "secondary_data/ALKIS/old")
}

### vg250gem
if(!dir.exists("secondary_data/vg250_gem/")) {
  dir.create("secondary_data/vg250_gem/" ,recursive = T)
  cat("Downloading vg250_gem...\n")
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/68c2d65273f5eb91955d8180/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "alkis.zip", dest_folder = "secondary_data/vg250_gem/old")
}

## Census data
if(!dir.exists("secondary_data/census")) {
  dir.create("secondary_data/census" ,recursive = T)

  cat("Downloading census data...\n")
  cloudR::download_and_extract_zip("https://files.de-1.osf.io/v1/resources/e2zvy/providers/osfstorage/682dd4be76df44ca46b06cdd/?view_only=f08b8c286f7a49279b5e472826c9090c&zip=", zip_name = "census.zip", dest_folder = "secondary_data/census/old")
}

all_data_reduced <- all_data %>%
       select(RID, survey_round, lat, lon, ARS, federal_state, county_name, municipality_name, town_name)
  

# ---- Dataset-level protection ----
if ("anon_applied" %in% names(all_data_reduced)) {
  if (all(all_data_reduced$anon_applied, na.rm = TRUE)) {
    stop("Aborting: This Dataset has been anonymized. This script is for unanonymized data.")
  }
  if (any(is.na(all_data_reduced$anon_applied))) {
    warning("Some rows have missing 'anon_applied' values. For these, geolocations were either removed because of orginal datapoints residing outside of Germany, or non-existent in the data in the first place.")
  }
}




# ---- PREPARING secondary data  ----
# Verified that all observations have lat/lon and that coordinates fall within German administrative boundaries.
# Note: germany_admin (from GADM) misses a few small municipalities in Schleswig-Holstein (SH).
# Original code for this check has been removed; see GitHub for reference.

### Creating germany_admin_corrected - as in clean_data.R ####
#### Combining with secondary data to review if exclusion criterion "residence in Germany" applies
# Load Germany admin shapefile

# germany_admin <- read_sf("secondary_data/germany_shapefiles/", "gadm41_DEU_4") %>%
#   select(NAME_1, NAME_2, NAME_3, NAME_4, CC_4) %>%
#   st_transform(4326)
# #Ref.: GADM. GADM database of Global Administrative Areas, version 4.1. GADM https://gadm.org/download_country.html (2023; accessed 22 May 2025).
# 
# # ARS codes of missing SH municipalities
# ars_codes <- c(
#   "010595990164","010595990148","010595990112","010595990121",
#   "010595990142","010595990147","010595990152","010595990136",
#   "010590045045","010595990154"
# )
# 
# # ALKIS reference names
# sh_alkis <- read_sf("secondary_data/ALKIS/", "KommunalesGebiet_Gemeinden_ALKIS") %>%
#   select(SCHLGMD, LAND, KREIS, AMT) %>%
#   mutate(AMT = str_remove(AMT, "^Amt\\s+|^amtsfreie Gemeinde\\s+")) %>%
#   st_set_geometry(NULL)
# #Ref.: Landesamt für Vermessung und Geoinformation Schleswig-Holstein (LVermGeo SH). 202407_VG_KKAG_ATKIS_ALKIS. Digitale Verwaltungsgrenzen (DVG) – Kommunales Gebiet https://geodaten.schleswig-holstein.de/gaialight-sh/_apps/dladownload/dl-vg_kkag.html (2024; accessed 8 September 2025).
# 
# # Geoportal polygons for missing municipalities
# geoportal_sf <- read_sf("secondary_data/vg250_gem/", "vg250_gem") %>%
#   filter(ars %in% ars_codes & !grepl("^DEBKGVG2000008I", objid)) %>%
#   st_transform(4326)
# #Ref.: Bundesamt für Kartographie und Geodäsie (BKG). Verwaltungsgebiete 1:250 000 mit Einwohnerzahlen – Stand 31.12., VG250-EW-Ebenen. Geodatenzentrum / Digitale Verwaltungsgebiete (VG250) https://www.geoportal.de/Download/4A9DCE2B-DCCA-4939-BA01-54364D11C46D
# 
# # Prepare updated/replacement polygons
# to_replace <- germany_admin %>%
#   filter(CC_4 %in% geoportal_sf$ars) %>%
#   st_set_geometry(NULL) %>%
#   left_join(geoportal_sf %>% select(CC_4 = ars, geometry), by = "CC_4") %>%
#   st_as_sf()
# 
# # New entries from Geoportal not in germany_admin
# new_entries <- geoportal_sf %>%
#   filter(!ars %in% germany_admin$CC_4) %>%
#   rename(CC_4 = ars) %>%
#   mutate(AGS_4 = sub("^(.{5}).{4}(.{3})$", "\\1\\2", CC_4)) %>%
#   left_join(sh_alkis, by = c("AGS_4" = "SCHLGMD")) %>%
#   transmute(NAME_1 = LAND, NAME_2 = KREIS, NAME_3 = AMT, NAME_4 = gen, CC_4, geometry) %>%
#   distinct() %>%
#   st_as_sf()
# 
# # Combine kept, replaced, and new polygons
# germany_admin_corrected <- bind_rows(
#   germany_admin %>% filter(!CC_4 %in% geoportal_sf$ars),
#   to_replace,
#   new_entries
# )
# 
# # Clean up
# rm(germany_admin, sh_alkis, to_replace, new_entries)



# Reviewing the map to decide which points rightfully excluded and which not
missing_ars_with_coords <- all_data_reduced %>%
  filter(is.na(ARS) & !is.na(lat) & !is.na(lon))
leaflet(missing_ars_with_coords) %>%
  addProviderTiles("CartoDB.Positron") %>%  # nice light background
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 5,
    color = "red",
    fillOpacity = 0.7,
    stroke = TRUE,
    label = ~paste0("RID: ", RID)
  ) %>%
  addLegend(
    "topright",
    colors = "red",
    labels = "Missing ARS",
    title = "Unassigned Points"
  )

# RIDs to keep regardless of missing administrative assignment - system considers them outside of Germany
keep_rids <- c("610908", "508450", "505487", "724953", "403741", "504409", "509461")

# 1) Subset to German observations where fallback is allowed
all_data_reduced_GER <- all_data_reduced %>%
  filter(
    ( !is.na(lat) & !is.na(lon) & !is.na(federal_state) ) | RID %in% keep_rids
  )

# 2) Points in this subset that still miss ARS but DO have coordinates
missing_admin <- all_data_reduced_GER %>%
  filter(is.na(ARS) & !is.na(lat) & !is.na(lon))

# If there are such rows, compute nearest-polygon fallback using germany_admin_corrected
nearest_fallback <- NULL
if (nrow(missing_admin) > 0) {
  missing_admin_sf <- st_as_sf(missing_admin, coords = c("lon", "lat"), crs = 4326)
  nearest_idx <- st_nearest_feature(missing_admin_sf, germany_admin_corrected)
  
  nearest_fallback <- tibble(
    RID = missing_admin_sf$RID,
    federal_state_nearest  = germany_admin_corrected$NAME_1[nearest_idx],
    county_name_nearest    = germany_admin_corrected$NAME_2[nearest_idx],
    municipality_name_nearest = germany_admin_corrected$NAME_3[nearest_idx],
    town_name_nearest      = germany_admin_corrected$NAME_4[nearest_idx],
    ARS_nearest            = germany_admin_corrected$CC_4[nearest_idx]
  ) %>%
    group_by(RID) %>%
    slice(1) %>%    # safety: keep one match per RID
    ungroup()
}

# 3) Apply fallback only for those RIDs (join nearest_fallback back to the full all_data_reduced)
if (!is.null(nearest_fallback) && nrow(nearest_fallback) > 0) {
  all_data_reduced <- all_data_reduced %>%
    left_join(nearest_fallback, by = "RID") %>%
    mutate(
      federal_state   = coalesce(federal_state, federal_state_nearest),
      county_name     = coalesce(county_name, county_name_nearest),
      municipality_name = coalesce(municipality_name, municipality_name_nearest),
      town_name       = coalesce(town_name, town_name_nearest),
      ARS             = coalesce(ARS, ARS_nearest)
    ) %>%
    select(-ends_with("_nearest"))  # drop helper columns
}

# 4) Finally, remove coordinates for rows that still have no ARS
all_data_reduced <- all_data_reduced %>%
  mutate(
    lat = if_else(is.na(ARS), NA_real_, lat),
    lon = if_else(is.na(ARS), NA_real_, lon)
  )


rm(all_data_reduced_GER, admin_assignments, missing_admin, missing_ars_with_coords, nearest_fallback, points_sf, admin_sf)


all_data_reduced <- all_data_reduced %>%
  mutate(geoexclusion = ifelse(is.na(ARS), TRUE, FALSE))

save_folder <- "intermediate_data/unanonymized_geolocations"
dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
final_df <- all_data_reduced %>%
  select(RID, ARS, geoexclusion) 
saveRDS(final_df,
        file.path(save_folder, "geolocation_exlusions.rds"))

rm(final_df, data_to_use, missing_admin_sf, all_data_reduced)
