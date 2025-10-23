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



# ------ READING IN XLSX files ---------------
# Minimal read function
read_cov_reduced <- function(x) {
  df <- read_excel(x)
  anon_flag_present <- "anon_applied" %in% names(df)
  
  df %>%
    rename(
      RID = RID,
      lat = latlng_wood_SQ_1_1,
      lon = latlng_wood_SQ_1_2
    ) %>%
    mutate(
      RID = as.numeric(RID),
      survey_round = gsub("_covariates.xlsx", "", basename(x)),
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      anon_applied = if (anon_flag_present) as.logical(df$anon_applied) else FALSE,
      anonymized_on = if (anon_flag_present && "anonymized_on" %in% names(df)) as.character(df$anonymized_on) else NA_character_
    ) %>%
    select(RID, survey_round, lat, lon, anon_applied, anonymized_on)
}

# Apply to all covariate files
raw_data_reduced <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
  purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>%
  map(read_cov_reduced)

# Combine into one dataframe with survey_round as identifier
all_data_reduced <- bind_rows(raw_data_reduced, .id = "survey_round")
rm(raw_data_reduced)

# Optional: create a unique RID across survey rounds
survey_round_map <- all_data_reduced %>%
  distinct(survey_round) %>%
  mutate(prefix = row_number() * 100000) %>% 
  deframe()

all_data_reduced <- all_data_reduced %>%
  mutate(RID_unique = survey_round_map[survey_round] + RID) %>%
  rename(RID_sample = RID, RID = RID_unique)


# # setwd("~/git_repos/DataPub")
# 
# if(!file.exists("finaldata/all_datasets.RData")) {
#   
#   dir.create("finaldata", showWarnings = FALSE)
#   download.file("https://osf.io/download/68d11b2611b72e9f4f74e860/?view_only=25375f7fb6c84c92818b5b0f7d0c01e2", destfile = "finaldata/all_datasets.RData", mode = "wb")
#   
# }
# 
# load("finaldata/all_datasets.RData")


# ---- Dataset-level protection ----
if ("anon_applied" %in% names(all_data_reduced)) {
  if (all(all_data_reduced$anon_applied, na.rm = TRUE)) {
    stop("Dataset has already been anonymized. Aborting to prevent double anonymization.")
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
germany_admin <- read_sf("secondary_data/germany_shapefiles/", "gadm41_DEU_4") %>%
  select(NAME_1, NAME_2, NAME_3, NAME_4, CC_4) %>%
  st_transform(4326)
#Ref.: GADM. GADM database of Global Administrative Areas, version 4.1. GADM https://gadm.org/download_country.html (2023; accessed 22 May 2025).

# ARS codes of missing SH municipalities
ars_codes <- c(
  "010595990164","010595990148","010595990112","010595990121",
  "010595990142","010595990147","010595990152","010595990136",
  "010590045045","010595990154"
)

# ALKIS reference names
sh_alkis <- read_sf("secondary_data/ALKIS/", "KommunalesGebiet_Gemeinden_ALKIS") %>%
  select(SCHLGMD, LAND, KREIS, AMT) %>%
  mutate(AMT = str_remove(AMT, "^Amt\\s+|^amtsfreie Gemeinde\\s+")) %>%
  st_set_geometry(NULL)
#Ref.: Landesamt für Vermessung und Geoinformation Schleswig-Holstein (LVermGeo SH). 202407_VG_KKAG_ATKIS_ALKIS. Digitale Verwaltungsgrenzen (DVG) – Kommunales Gebiet https://geodaten.schleswig-holstein.de/gaialight-sh/_apps/dladownload/dl-vg_kkag.html (2024; accessed 8 September 2025).

# Geoportal polygons for missing municipalities
geoportal_sf <- read_sf("secondary_data/vg250_gem/", "vg250_gem") %>%
  filter(ars %in% ars_codes & !grepl("^DEBKGVG2000008I", objid)) %>%
  st_transform(4326)
#Ref.: Bundesamt für Kartographie und Geodäsie (BKG). Verwaltungsgebiete 1:250 000 mit Einwohnerzahlen – Stand 31.12., VG250-EW-Ebenen. Geodatenzentrum / Digitale Verwaltungsgebiete (VG250) https://www.geoportal.de/Download/4A9DCE2B-DCCA-4939-BA01-54364D11C46D

# Prepare updated/replacement polygons
to_replace <- germany_admin %>%
  filter(CC_4 %in% geoportal_sf$ars) %>%
  st_set_geometry(NULL) %>%
  left_join(geoportal_sf %>% select(CC_4 = ars, geometry), by = "CC_4") %>%
  st_as_sf()

# New entries from Geoportal not in germany_admin
new_entries <- geoportal_sf %>%
  filter(!ars %in% germany_admin$CC_4) %>%
  rename(CC_4 = ars) %>%
  mutate(AGS_4 = sub("^(.{5}).{4}(.{3})$", "\\1\\2", CC_4)) %>%
  left_join(sh_alkis, by = c("AGS_4" = "SCHLGMD")) %>%
  transmute(NAME_1 = LAND, NAME_2 = KREIS, NAME_3 = AMT, NAME_4 = gen, CC_4, geometry) %>%
  distinct() %>%
  st_as_sf()

# Combine kept, replaced, and new polygons
germany_admin_corrected <- bind_rows(
  germany_admin %>% filter(!CC_4 %in% geoportal_sf$ars),
  to_replace,
  new_entries
)

# Clean up
rm(germany_admin, sh_alkis, to_replace, new_entries)

##---- Fixing ARS -administrative subunit- assignment (erroneous germany_admin): Old approach of fixing germany_admin (check github)


points_sf <- all_data_reduced %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Keep only relevant polygon columns for join
admin_sf <- germany_admin_corrected %>%
  select(CC_4, NAME_1, NAME_2, NAME_3, NAME_4)

# Spatial join: points inside polygons
admin_assignments <- st_join(points_sf, admin_sf, join = st_within, left = FALSE) %>%
  st_drop_geometry() %>%
  rename(
    federal_state   = NAME_1,
    county_name     = NAME_2,
    municipality_name = NAME_3,
    town_name       = NAME_4,
    ARS             = CC_4
  ) %>%
  select(RID, federal_state, county_name, municipality_name, town_name, ARS)

all_data_reduced <- all_data_reduced %>% left_join(admin_assignments, by = "RID")



# Reviewing the map to decide which points rightfully excluded and which not

# missing_ars_with_coords <- all_data_reduced %>%
#   filter(is.na(ARS) & !is.na(lat) & !is.na(lon))
# leaflet(missing_ars_with_coords) %>%
#   addProviderTiles("CartoDB.Positron") %>%  # nice light background
#   addCircleMarkers(
#     lng = ~lon,
#     lat = ~lat,
#     radius = 5,
#     color = "red",
#     fillOpacity = 0.7,
#     stroke = TRUE,
#     label = ~paste0("RID: ", RID)
#   ) %>%
#   addLegend(
#     "topright",
#     colors = "red",
#     labels = "Missing ARS",
#     title = "Unassigned Points"
#   )

# RIDs to keep regardless of missing administrative assignment - system considers them outside of Germany
keep_rids <- c("403741", "504409", "508450", "509461", "505487", "724953")

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





### Preparing for anonymization: popdensity based on grid cells ---------
#https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Zensus2022/_publikationen.html#1403950
census_1x1km_csv <- read_delim(
  "secondary_data/census/Zensus2022_Bevoelkerungszahl_1km-Gitter.csv",
  delim = ";"
)

ger_boundary_sf <- read_sf("secondary_data/germany_shapefiles/", "gadm41_DEU_0") %>%
  st_transform(4326)

# Germany Population Raster ----
# Zensus 2022 misses all grid cells with Inhabitants = 0. Here, the grid raster is completed by adding zero cells.
censuskm_rast <- {
  census_1x1km_csv %>%
    vect(geom = c("x_mp_1km", "y_mp_1km"), crs = "EPSG:3035") %>%
    {
      census_rast <- {
        rast_template <- rast(ext(.), resolution = 1000, crs = "EPSG:3035")
        rasterize(., rast_template, field = "Einwohner", fun = "sum")
      }
      
      ger_rast_aligned <- {
        ger_bound_vect <- ger_boundary_sf %>%
          vect() %>%
          project("EPSG:3035")
        
        zero_rast <- rast(ext(ger_bound_vect), resolution = 1000, crs = "EPSG:3035")
        values(zero_rast) <- 0
        
        zero_rast %>%
          mask(ger_bound_vect) %>%
          resample(census_rast, method = "near")
      }
      
      census_rast %>%
        cover(ger_rast_aligned) %>%
        mask(ger_boundary_sf %>% vect() %>% project("EPSG:3035"))
    }
}

# Convert raster to polygons
census2022_popden <- as.polygons(censuskm_rast, dissolve = FALSE)

# Directly use the polygons in memory, no saving/reading
germany_grid <- census2022_popden %>%
  st_as_sf() %>%                  # convert to sf
  st_transform(crs = 4326)        # reproject

rm(census_1x1km_csv,census2022_popden,censuskm_rast)



# Perform spatial join based on lat/lon to match with germany_grid polygons
admin_assignments_grid <- all_data_reduced %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%   # Convert only these rows to sf
  st_join(germany_grid, join = st_intersects) %>%      # Spatial join with germany_grid
  st_drop_geometry() %>%                               # Optionally drop geometry if not needed
  select(RID, sum)                                     # Select only RID and sum column

## These are the observations outside of germany_admin before creating germany_admin_corrected - hence no germany_grid data available for them
### As fallback computing area and population density from administrative data available for the whole municipality
geoportal_sf_fallback <- geoportal_sf %>%
  st_transform(3035) %>%
  mutate(
    area_size = round(as.numeric(st_area(geometry)) / 1e6, 2),
    area_popdensity = round(ewz / area_size, 0)
  ) %>%
  st_transform(4326)

# --- 1. Identify rows with missing Cell_popdensity ---
missing_points_sf <- all_data_reduced %>%
  filter(RID %in% admin_assignments_grid$RID[is.na(admin_assignments_grid$sum)]) %>%
  filter(!is.na(lat) & !is.na(lon) & !is.na(ARS)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[,1],   # keep coordinates as separate columns
         lat = st_coordinates(.)[,2])

# leaflet(missing_points_sf) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addCircleMarkers(
#     lng = ~lon,
#     lat = ~lat,
#     radius = 5,
#     color = "red",
#     fillOpacity = 0.7,
#     stroke = TRUE,
#     label = ~paste0("RID: ", RID)
#   ) %>%
#   addLegend(
#     "topright",
#     colors = "red",
#     labels = "Missing ARS",
#     title = "Unassigned Points"
#   )

nearest_idx <- st_nearest_feature(missing_points_sf, geoportal_sf_fallback)

fallback_assignments <- missing_points_sf %>%
  mutate(area_popdensity = geoportal_sf_fallback$area_popdensity[nearest_idx]) %>%
  st_drop_geometry() %>%
  select(RID, area_popdensity)

admin_assignments_grid <- admin_assignments_grid %>%
  left_join(fallback_assignments, by = "RID") %>%
  mutate(sum = coalesce(sum, area_popdensity)) %>%
  select(-area_popdensity)

# Merge the sum value from germany_grid back to all_data_reduced
all_data_reduced <- all_data_reduced %>%
  left_join(admin_assignments_grid, by = "RID") %>%
  # select(-geometry) %>%
  rename(Cell_popdensity = sum)

all_data_reduced_GER <- all_data_reduced %>%
  filter(
    ( !is.na(lat) & !is.na(lon) & !is.na(ARS) ))

# Confirm that no NAs remain
sum(is.na(all_data_reduced_GER$Cell_popdensity))

rm(fallback_assignments, admin_assignments_grid)
rm(list = ls(pattern = "^selected_sf_geoportal"))
rm(list = ls(pattern = "^sh_alkis"))
rm(list = ls(pattern = "_sf$"))
rm(list = ls(pattern = "_df$"))
rm(germany_grid, geoportal_sf_fallback)



# ------------- New ANONYMIZATION Script ---------

#Next setting up a function that alters lon and lat dependent on random variables and population density and within a predefined scope 
# conditional on remaining within the same municipality

# # The idea is to alter the original geocoordinates to anonymize them within a pre-defined radius, based on pop-density or settlement type size
# # General function should be:
# # [lat,lon] + epsilon * r(popdensity), where epsilon is a random variable between 0.1 and 1 and r(popdensity) is the radius dependent on the population density

############# Criteria/Conditions 
# # As a reference value: I want to make sure that at least a certain number of people (e.g., 200) live within the anonymization area
# # computing a radius r (in meters) for each observation such that: πr^2⋅popdensity≥200, i.e. at least 200 people (100 households) within the selected area
# # This is of course not accounting for the population density in the surrounding grid cells but we consider this simplification as a good proxy

# ### Minimum set at 200 (which decile?) and capping at 1000 (fufills condition for 99%)
# # Cap eps * anonym_radius_m at 1000 by adjusting eps directly


# Duplicate main data, keeping only necessary columns
anon_working_df <- all_data_reduced_GER %>%
  select(RID, survey_round, lat, lon, ARS, federal_state, Cell_popdensity) %>%
  mutate(
    anon_applied = FALSE,
    anonymized_on = NA_character_
  )

# Constants
meters_per_degree_lat <- 111320

# Compute anonymization radius
compute_radius_km2 <- function(popdensity_km2, people = 200, round_to = 100) {
  popdensity_m2 <- popdensity_km2 / 1e6
  r_raw <- sqrt(people / (pi * popdensity_m2))
  r_rounded <- ceiling(r_raw / round_to) * round_to
  return(r_rounded)
}
anon_working_df$anonym_radius_m <- compute_radius_km2(anon_working_df$Cell_popdensity)
min_cap <- 200
max_cap <- 1500
anon_working_df$anonym_radius_m <- pmax(min_cap, pmin(max_cap, anon_working_df$anonym_radius_m))

# --- EPSILON SCALING (safe) ---
set.seed(123)
# Draw random eps between 0.25 and 1
anon_working_df$eps <- runif(nrow(anon_working_df), 0.25, 1)
# Apply global 1 km displacement cap
anon_working_df$eps <- pmin(anon_working_df$eps,
                            1000 / anon_working_df$anonym_radius_m)
# Replace problematic values
# - If eps is NA, Inf, or <= 0, assign fallback = 0.25
#   (ensures at least some displacement)
anon_working_df$eps[!is.finite(anon_working_df$eps) | 
                      anon_working_df$eps <= 0] <- 0.25


# Initial theta
set.seed(42)
anon_working_df$theta <- runif(nrow(anon_working_df), 0, 2*pi)

#### INITIAL DISPLACEMENT #################
r <- anon_working_df$eps * anon_working_df$anonym_radius_m
dx <- r * cos(anon_working_df$theta)
dy <- r * sin(anon_working_df$theta)

lat_rad <- anon_working_df$lat * pi / 180
meters_per_degree_lon <- meters_per_degree_lat * cos(lat_rad)
anon_working_df$lat_anon <- anon_working_df$lat + dy / meters_per_degree_lat
anon_working_df$lon_anon <- anon_working_df$lon + dx / meters_per_degree_lon

#### ROBUST ASSIGN_ADMIN FUNCTION ##########  
assign_admin <- function(df, admin_shp, lon_col = "lon_anon", lat_col = "lat_anon", buffer_dist = 1e-9) {
  # Keep only rows with valid coordinates for the spatial join
  valid_idx <- !is.na(df[[lon_col]]) & !is.na(df[[lat_col]])
  
  if (any(valid_idx)) {
    # Convert only valid rows to sf
    sf_points <- df[valid_idx, ] %>%
      st_as_sf(coords = c(lon_col, lat_col), crs = 4326)
    
    # Ensure polygons are in the same CRS
    if (st_crs(admin_shp) != st_crs(sf_points)) {
      admin_shp <- st_transform(admin_shp, st_crs(sf_points))
    }
    
    # Apply a tiny buffer to catch boundary cases
    sf_points_buffered <- st_buffer(sf_points, dist = buffer_dist)
    
    # Spatial join
    joined <- st_join(sf_points_buffered, admin_shp, left = TRUE)
    
    # Initialize ARS_assigned with NA
    df$ARS_assigned <- NA_character_
    df$ARS_assigned[valid_idx] <- joined$CC_4
  } else {
    # If no valid coords, create column with all NA
    df$ARS_assigned <- NA_character_
  }
  
  # Flag mismatches (normalize for safety)
  df$ARS_mismatch <- tolower(trimws(as.character(df$ARS))) != 
    tolower(trimws(as.character(df$ARS_assigned)))
  
  return(df)
}








# Initial mismatch check
anon_working_df <- assign_admin(anon_working_df, germany_admin_corrected)
table(anon_working_df$ARS_mismatch)

cat("Number of observations without ARS for anonymized geolocation:", 
    nrow(anon_working_df %>% filter(is.na(ARS_assigned))), "\n")


# # erroneous assignment for single ob
# anon_missing_ARS <- anon_working_df %>% filter(RID == 723127)
# 
# leaflet(anon_missing_ARS) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%  # basemap
#   # Germany administrative polygons
#   addPolygons(
#     data = germany_admin_corrected,
#     color = "darkgreen",
#     weight = 2,
#     opacity = 0.5,
#     fillOpacity = 0,
#     label = ~paste0(NAME_1, ", ", NAME_2, ", ", NAME_3, ", ", NAME_4)
#   ) %>%
#   # Anonymized coordinates (red)
#   addCircleMarkers(
#     lng = ~lon_anon,
#     lat = ~lat_anon,
#     radius = 6,
#     color = "red",
#     fillOpacity = 0.7,
#     label = ~paste0(
#       "RID: ", RID, "<br>",
#       "Original ARS: ", ARS, "<br>",
#       "Lat_anon: ", round(lat_anon, 5), "<br>",
#       "Lon_anon: ", round(lon_anon, 5)
#     )
#   ) %>%
#   # Original coordinates (blue)
#   addCircleMarkers(
#     lng = ~lon,
#     lat = ~lat,
#     radius = 6,
#     color = "blue",
#     fillOpacity = 0.5,
#     label = ~paste0(
#       "RID: ", RID, "<br>",
#       "Original ARS: ", ARS, "<br>",
#       "Lat: ", round(lat, 5), "<br>",
#       "Lon: ", round(lon, 5)
#     )
#   ) %>%
#   addLegend(
#     position = "bottomright",
#     colors = c("red", "blue", "darkgreen"),
#     labels = c("Anonymized location", "Original location", "Admin boundaries"),
#     title = "Unassigned points"
#   )
anon_working_df <- anon_working_df %>%
  mutate(
    ARS_assigned = if_else(RID == 723127, "051620004004", ARS_assigned),
    ARS_mismatch = if_else(RID == 723127, TRUE, ARS_mismatch)
  )


#### ROUNDS 2–4 TO REDUCE MISMATCHES #### 
# Round 2: Flip theta (360° - theta)
round2_df <- anon_working_df %>% 
  filter(is.na(ARS_mismatch) | ARS_mismatch)
if(nrow(round2_df) > 0) {
  round2_df <- round2_df %>%
    mutate(theta = 2*pi - theta)
  
  # Recompute anonymized coordinates
  r <- round2_df$eps * round2_df$anonym_radius_m
  dx <- r * cos(round2_df$theta)
  dy <- r * sin(round2_df$theta)
  lat_rad <- round2_df$lat * pi / 180
  meters_per_degree_lon <- meters_per_degree_lat * cos(lat_rad)
  round2_df <- round2_df %>%
    mutate(
      lat_anon = lat + dy / meters_per_degree_lat,
      lon_anon = lon + dx / meters_per_degree_lon
    )
  
  # Update ARS mismatch
  round2_df <- assign_admin(round2_df, germany_admin_corrected)
}

# Round 3: ±90° adjustment (only remaining mismatches)
round3_df <- round2_df %>% 
  filter(is.na(ARS_mismatch) | ARS_mismatch)

if(nrow(round3_df) > 0) {
  round3_df <- round3_df %>%
    mutate(theta = ifelse(theta > pi, theta - pi/2, theta + pi/2))
  
  r <- round3_df$eps * round3_df$anonym_radius_m
  dx <- r * cos(round3_df$theta)
  dy <- r * sin(round3_df$theta)
  lat_rad <- round3_df$lat * pi / 180
  meters_per_degree_lon <- meters_per_degree_lat * cos(lat_rad)
  round3_df <- round3_df %>%
    mutate(
      lat_anon = lat + dy / meters_per_degree_lat,
      lon_anon = lon + dx / meters_per_degree_lon
    )
  
  round3_df <- assign_admin(round3_df, germany_admin_corrected)
}

# Round 4: Flip theta again (360° - theta) for remaining mismatches
round4_df <- round3_df %>% 
  filter(is.na(ARS_mismatch) | ARS_mismatch)

if(nrow(round4_df) > 0) {
  round4_df <- round4_df %>%
    mutate(theta = 2*pi - theta)
  
  r <- round4_df$eps * round4_df$anonym_radius_m
  dx <- r * cos(round4_df$theta)
  dy <- r * sin(round4_df$theta)
  lat_rad <- round4_df$lat * pi / 180
  meters_per_degree_lon <- meters_per_degree_lat * cos(lat_rad)
  round4_df <- round4_df %>%
    mutate(
      lat_anon = lat + dy / meters_per_degree_lat,
      lon_anon = lon + dx / meters_per_degree_lon
    )
  
  round4_df <- assign_admin(round4_df, germany_admin_corrected)
}
# After each round, only unmatched rows are passed forward, and ARS_assigned is updated after recomputation.

#### LOOP TO MATCH REMAINING #### 
loop_data <- round4_df %>% 
  filter(is.na(ARS_mismatch) | ARS_mismatch)

loop_data$matched <- FALSE  # initialize lock flag
theta_increment <- 10 * pi / 180
max_rounds <- 35
results <- data.frame(round = integer(), n_mismatches = integer())

for(round in 1:max_rounds){
  
  idx_unmatched <- which(!loop_data$matched)
  
  if(length(idx_unmatched) == 0){
    cat("All points matched by round", round-1, "\n")
    break
  }
  
  # Increment theta only for unmatched points
  loop_data$theta[idx_unmatched] <- loop_data$theta[idx_unmatched] + theta_increment
  
  # Compute displacements
  r <- loop_data$eps * loop_data$anonym_radius_m
  dx <- r * cos(loop_data$theta)
  dy <- r * sin(loop_data$theta)
  
  lat_rad <- loop_data$lat * pi / 180
  meters_per_degree_lon <- meters_per_degree_lat * cos(lat_rad)
  
  # Update anonymized coordinates
  loop_data$lat_anon <- loop_data$lat + dy / meters_per_degree_lat
  loop_data$lon_anon <- loop_data$lon + dx / meters_per_degree_lon
  
  # Recompute ARS_mismatch safely
  loop_data <- assign_admin(loop_data, germany_admin_corrected)
  
  # Update matched flag based on current mismatch status
  loop_data$matched <- ifelse(is.na(loop_data$ARS_mismatch), FALSE, !loop_data$ARS_mismatch)
  
  # Track progress
  results <- rbind(results, data.frame(round = round, n_mismatches = sum(!loop_data$matched)))
  
  cat("Round", round, "→", sum(!loop_data$matched), "mismatches remaining\n")
}

cat("Final unmatched points:", sum(!loop_data$matched), "\n")


# === FINAL CHECK & MAP ===
# Helper to overwrite lat/lon_anon from a source df into anon_working_df
# === FINALIZE ANONYMIZED COORDINATES (with theta & ARS_assigned) ===
overwrite_coords <- function(base_df, source_df) {
  # Select columns to overwrite
  src <- source_df %>%
    select(RID, lat_anon, lon_anon, theta, ARS_assigned) %>%
    rename(
      lat_new = lat_anon,
      lon_new = lon_anon,
      theta_new = theta,
      ARS_new = ARS_assigned
    )
  
  # Left join by RID
  base_df <- base_df %>%
    left_join(src, by = "RID") %>%
    mutate(
      lat_anon = ifelse(!is.na(lat_new), lat_new, lat_anon),
      lon_anon = ifelse(!is.na(lon_new), lon_new, lon_anon),
      theta = ifelse(!is.na(theta_new), theta_new, theta),
      ARS_assigned = ifelse(!is.na(ARS_new), ARS_new, ARS_assigned)
    ) %>%
    select(-lat_new, -lon_new, -theta_new, -ARS_new)
  
  return(base_df)
}


# Apply in sequence: round2 → round3 → round4 → loop
anon_working_df <- anon_working_df %>%
  overwrite_coords(round2_df) %>%
  overwrite_coords(round3_df) %>%
  overwrite_coords(round4_df) %>%
  overwrite_coords(loop_data)

anon_working_df <- anon_working_df %>%
  mutate(
    anon_applied = TRUE,
    anonymized_on = as.character(Sys.time())
  )

# === FINAL ARS CHECK (without overwriting ARS_mismatch) ===
# Select only observations that were originally unmatched - i.e. required corrected anonymized geolocations
final_matched <- anon_working_df %>%
  filter(ARS_mismatch == TRUE | is.na(ARS_mismatch))

# Compare ARS and ARS_assigned
final_check_summary <- final_matched %>%
  summarize(
    n_total     = n(),
    n_agree     = sum(ARS == ARS_assigned, na.rm = TRUE),
    n_disagree  = sum(ARS != ARS_assigned, na.rm = TRUE),
    n_unassigned = sum(is.na(ARS_assigned))
  )

# Print summary
print(final_check_summary)

# colors <- colorRampPalette(brewer.pal(12, "Paired"))(nrow(loop_data))
# 
# loop_data <- loop_data %>%
#   rowwise() %>%
#   mutate(distance_m = distHaversine(c(lon, lat), c(lon_anon, lat_anon))) %>%
#   ungroup()
# 
# m <- leaflet(loop_data) %>%
#   addProviderTiles("CartoDB.Positron")  # nice light background
# 
# # Add points per observation
# for (i in seq_len(nrow(loop_data))) {
#   m <- m %>%
#     # Original and anonymized points
#     addCircleMarkers(
#       lng = c(loop_data$lon[i], loop_data$lon_anon[i]),
#       lat = c(loop_data$lat[i], loop_data$lat_anon[i]),
#       color = colors[i],
#       radius = 6,
#       stroke = TRUE,
#       fillOpacity = 0.8,
#       label = paste0("RID: ", loop_data$RID[i])
#     ) %>%
#     # Transparent circle around anonymized location
#     addCircles(
#       lng = loop_data$lon_anon[i],
#       lat = loop_data$lat_anon[i],
#       radius = loop_data$distance_m[i],  # radius in meters
#       color = colors[i],
#       fillColor = colors[i],
#       fillOpacity = 0.15,  # high transparency
#       weight = 1,          # thin border
#       stroke = TRUE
#     )
# }
# m



# Optional: inspect rows where ARS and ARS_assigned disagree (should be 0)
if(final_check_summary$n_disagree > 0){
  warning("Some originally matched rows disagree. Inspect them:")
  print(final_matched %>% filter(ARS != ARS_assigned))
}

# FINAL mismatch check
anon_working_df <- assign_admin(anon_working_df, germany_admin_corrected)
table(anon_working_df$ARS_mismatch)

# Successful anonymization respecting the condition of maintaining the ARS

rm(list = ls(pattern = "^round"))
rm(results, loop_data, final_check_summary, final_matched)


# ---- SAVING LIST OF RID + ANONYMIZED GEOLOCATIONS OF RESIDENCE ------------
save_folder <- "intermediate_data/anonymized_geolocations"
dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
final_df <- all_data_reduced %>%
  select(RID, RID_sample, survey_round) %>%
  left_join(
    anon_working_df %>% select(RID, lon_anon, lat_anon, anon_applied, anonymized_on),
    by = "RID"
  )
saveRDS(final_df,
        file.path(save_folder, "anonomyized_geolocations.rds"))

# # # ------------ Overwriting xlsx -------

# Load anonymized coordinates
anon_df <- readRDS(file.path("intermediate_data/anonymized_geolocations/anonomyized_geolocations.rds"))

# List of covariate files
covariate_files <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates")

# Backup folder
backup_folder <- "data_backup"
dir_create(backup_folder)

update_excel_with_anon_backup <- function(file_path, anon_df, backup_folder) {
  
  # Backup original
  file_copy(file_path, file.path(backup_folder, basename(file_path)), overwrite = TRUE)
  
  # Load workbook
  wb <- loadWorkbook(file_path)
  
  # Detect sheet with lat/lon columns
  sheet_name <- names(wb)[
    map_lgl(names(wb), ~{
      cols <- read.xlsx(wb, sheet = .x, rows = 1:5, colNames = TRUE)
      all(c("latlng_wood_SQ_1_1", "latlng_wood_SQ_1_2", "RID") %in% names(cols))
    })
  ]
  
  if(length(sheet_name) == 0){
    message("No lat/lon sheet found in ", file_path)
    return(NULL)
  }
  
  # Read that sheet
  df <- read.xlsx(wb, sheet = sheet_name) %>%
    mutate(RID_sample = as.numeric(RID))  # consistent join key
  
  # Check if already fully anonymized
  if("anon_applied" %in% names(df) && all(df$anon_applied, na.rm = TRUE)) {
    message("File already anonymized, skipping: ", basename(file_path))
    return(df)
  }
  
  survey_round <- gsub("_covariates.xlsx", "", basename(file_path))
  
  # Join anonymized coordinates + flags
  df_updated <- df %>%
    left_join(
      anon_df %>% filter(survey_round == !!survey_round) %>%
        select(RID_sample, lat_anon, lon_anon, anon_applied, anonymized_on),
      by = "RID_sample"
    ) %>%
    mutate(
      latlng_wood_SQ_1_1 = lat_anon,
      latlng_wood_SQ_1_2 = lon_anon,
      anon_applied = coalesce(anon_applied.y, anon_applied.x, FALSE),
      anonymized_on = coalesce(anonymized_on.y, anonymized_on.x)
    ) %>%
    select(-lat_anon, -lon_anon, -ends_with(".x"), -ends_with(".y"))
  
  # Write back to workbook
  writeData(wb, sheet = sheet_name, df_updated, withFilter = FALSE)
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  return(df_updated)
}

# Apply function
updated_files <- map(covariate_files, update_excel_with_anon_backup,
                     anon_df = anon_df, backup_folder = backup_folder)



# # --- Upload to OSF -----
# library(osfr)
# node <- osf_retrieve_node("zpfgj")
# folders_to_upload <- c("data/pilot", "data/main_study")
# 
# for (local_path in folders_to_upload) {
#   if (dir.exists(local_path)) {
#     
#     # Determine the base folder name
#     folder_name <- basename(local_path)
#     
#     # Check if the folder exists at the root of the node
#     existing <- osf_ls_files(node) %>% filter(name == folder_name)
#     
#     # Create the folder if it doesn't exist
#     if (nrow(existing) == 0) {
#       osf_folder <- osf_mkdir(node, folder_name)  # create directly under the node
#     } else {
#       osf_folder <- existing[[1, "id"]] %>% osf_retrieve_file()  # existing folder object
#     }
#     
#     # Upload recursively into the OSF folder
#     osf_upload(
#       osf_folder,
#       path = local_path,
#       recurse = TRUE,
#       progress = TRUE,
#       verbose = TRUE,
#       conflicts = "override"
#     )
#   }
# }