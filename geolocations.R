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


# ------ USE OF FILE ---------------

# Our dataset contains a set of sensible personal information that requires anonymization to protect privacy rights
# In combination the following variables could potentially serve as identifiers in anonymized data

# lat, lon                   # precise location
# q2_1 (res_settlement_type) # settlement type
# postcode, birthyear, gender, married, educ, empl, hhsize, hhnetinc
# pol_btw                     # political affiliation
# house, kids, garden, privateforest, addressexp

# We therefore anonymize the geolocations before publication in order to protect the privacy rights of our respondents




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

#Als nächstes Funktion aufstellen, die lon und lat in zuvor gesetzten rahmen in Abhängigkeit von zufallsvariable und siedlungsgröße verschiebt
# unter der Nebenbedingung, dass wir innerhalb der gemeindegrenzen bleiben

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






# --------------- Final checks ########## 
#### Individual level #####
# 
# # 1. Take only rows with anonymized coordinates
# check_df <- anon_working_df %>%
#   filter(!is.na(lat), !is.na(lon), !is.na(lat_anon), !is.na(lon_anon))
# 
# # 2. Create lines between original and anonymized points
# lines_list <- lapply(seq_len(nrow(check_df)), function(i) {
#   coords <- matrix(c(
#     check_df$lon[i], check_df$lat[i],
#     check_df$lon_anon[i], check_df$lat_anon[i]
#   ), ncol = 2, byrow = TRUE)
#   st_linestring(coords)
# })
# 
# lines_sf <- st_sfc(lines_list, crs = 4326) %>%
#   st_sf(data.frame(RID = check_df$RID))
# 
# # 3. Extract polygon boundaries of Germany admin
# germany_boundaries <- st_boundary(germany_admin_corrected)
# 
# # 4. Check which lines intersect any polygon boundary
# cross_border_idx <- st_intersects(lines_sf, germany_boundaries, sparse = FALSE)
# cross_border_any <- apply(cross_border_idx, 1, any)
# 
# # 5. Subset of observations whose anonymization line crosses a boundary
# cross_border_df <- check_df[cross_border_any, ]
# 
# # Optional: also create an sf object of the lines that cross boundaries
# cross_lines_sf <- lines_sf[cross_border_any, ]
# 
# # 6. Map in leaflet
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
# 
#   # Add Germany admin polygons
#   addPolygons(
#     data = germany_admin_corrected,
#     color = "darkgreen",
#     weight = 2,
#     opacity = 0.5,
#     fillOpacity = 0
#   ) %>%
# 
#   # Original points (blue)
#   addCircleMarkers(
#     data = cross_border_df,
#     lng = ~lon,
#     lat = ~lat,
#     radius = 4,
#     color = "blue",
#     fillOpacity = 0.6,
#     label = ~paste0("RID: ", RID, "\nOriginal")
#   ) %>%
# 
#   # Anonymized points (red)
#   addCircleMarkers(
#     data = cross_border_df,
#     lng = ~lon_anon,
#     lat = ~lat_anon,
#     radius = 4,
#     color = "red",
#     fillOpacity = 0.6,
#     label = ~paste0("RID: ", RID, "\nAnonymized")
#   ) %>%
# 
#   # Lines showing displacement (orange)
#   addPolylines(
#     data = cross_lines_sf,
#     color = "orange",
#     weight = 2,
#     opacity = 0.8,
#     label = ~paste0("RID: ", RID)
#   ) %>%
# 
#   addLegend(
#     position = "bottomright",
#     colors = c("blue", "red", "orange"),
#     labels = c("Original points", "Anonymized points", "Displacement crossing boundary"),
#     opacity = 0.8
#   )
# 
# ### All cases are within the same ARS municipality
# 
# 
# 
# 
#### Aggregate level #####
# 
# # Plots for final 
# 
# # Plot histogram of distances
# ggplot(anon_working_df, aes(x = distance_m)) +
#   geom_histogram(bins = 50, fill = "steelblue", color = "white") +
#   labs(
#     title = "Histogram of distances between true and anonymized locations",
#     x = "Distance (meters)",
#     y = "Count"
#   ) +
#   theme_minimal()
# 
# # 2. Define breaks for 10-degree bins
# breaks <- seq(0, 360, by = 10)
# 
# anon_working_df <- anon_working_df %>%
#   mutate(theta_deg = (theta * 180 / pi) %% 360,  # convert to degrees
#          theta_bin = cut(theta_deg, breaks = breaks, include.lowest = TRUE, right = FALSE))
# 
# # 3. Compute average distance per bin, assign midpoint correctly
# st_by_angle <- anon_working_df %>%
#   group_by(theta_bin) %>%
#   summarise(mean_dist = mean(distance_m, na.rm = TRUE), .groups = "drop") %>%
#   mutate(theta_mid = head(breaks, -1) + diff(breaks) / 2)
# 
# # 4. Polar bar plot (modified)
# ggplot(st_by_angle, aes(x = theta_mid, y = mean_dist)) +
#   geom_col(fill = "steelblue", width = 8) +
#   coord_polar(start = 0) +
#   scale_y_continuous(
#     breaks = seq(50, 350, 50),  # radial labels
#     limits = c(0, 350)          # max radius
#   ) +
#   scale_x_continuous(
#     breaks = c(90, 180, 270, 360),  # angle labels
#     labels = c("90°", "180°", "270°", "360°")
#   ) +
#   labs(
#     title = "Average Anonymization Distance per 10° Direction",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.y = element_text(size = 10),  # radial labels
#     axis.text.x = element_text(size = 10)   # angular labels
#   )
# 
# 
# obs_per_ars <- anon_working_df %>%
#   group_by(ARS) %>%
#   summarise(n_obs = n(), .groups = "drop")
# 
# 
# 
# 
# 
# 
# # Filter for the ARS of interest
# ars_data <- anon_working_df %>% filter(ARS == "160530000000")
# 
# # Generate a unique color per observation
# # 26 observations, so use a qualitative palette
# colors <- colorRampPalette(brewer.pal(12, "Paired"))(nrow(ars_data))
# 
# # Create leaflet map
# m <- leaflet(ars_data) %>%
#   addProviderTiles("CartoDB.Positron")  # nice light background
# 
# # Add points per observation
# for (i in seq_len(nrow(ars_data))) {
#   m <- m %>%
#     # Original and anonymized points
#     addCircleMarkers(
#       lng = c(ars_data$lon[i], ars_data$lon_anon[i]),
#       lat = c(ars_data$lat[i], ars_data$lat_anon[i]),
#       color = colors[i],
#       radius = 6,
#       stroke = TRUE,
#       fillOpacity = 0.8,
#       label = paste0("RID: ", ars_data$RID[i])
#     ) %>%
#     # Transparent circle around anonymized location
#     addCircles(
#       lng = ars_data$lon_anon[i],
#       lat = ars_data$lat_anon[i],
#       radius = ars_data$distance_m[i],  # radius in meters
#       color = colors[i],
#       fillColor = colors[i],
#       fillOpacity = 0.15,  # high transparency
#       weight = 1,          # thin border
#       stroke = TRUE
#     )
# }
# 
# # Zoom map to ARS bounding box
# bounds <- ars_data %>% 
#   summarise(
#     lng_min = min(c(lon, lon_anon)),
#     lng_max = max(c(lon, lon_anon)),
#     lat_min = min(c(lat, lat_anon)),
#     lat_max = max(c(lat, lat_anon))
#   )
# 
# m <- m %>% fitBounds(lng1 = bounds$lng_min, lat1 = bounds$lat_min,
#                      lng2 = bounds$lng_max, lat2 = bounds$lat_max)
# 
# saveWidget(m, "ARS_160530000000_map.html", selfcontained = TRUE)
# 
# # Summary statistics for distance_m
# distance_summary <- ars_data %>%
#   summarise(
#     n = n(),
#     mean_distance = mean(distance_m, na.rm = TRUE),
#     sd_distance = sd(distance_m, na.rm = TRUE),
#     min_distance = min(distance_m, na.rm = TRUE),
#     q25 = quantile(distance_m, 0.25, na.rm = TRUE),
#     median_distance = median(distance_m, na.rm = TRUE),
#     q75 = quantile(distance_m, 0.75, na.rm = TRUE),
#     max_distance = max(distance_m, na.rm = TRUE)
#   )
# 
# 
# 
# 
# 
# 
# 
# # 1) Prepare table of interest and print it
# cells_df <- anon_working_df %>%
#   select(Cell_popdensity, distance_m, RID) %>%   # include RID if you want an ID column
#   filter(!is.na(Cell_popdensity) & !is.na(distance_m))
# 
# # 2) Theoretical (unrounded) radius function for 200 people
# required_radius_for_people <- function(popdensity_km2, people = 200) {
#   popdensity_m2 <- popdensity_km2 / 1e6
#   r <- sqrt(people / (pi * popdensity_m2))  # meters
#   return(r)
# }
# 
# # 3) Make curve data across density range
# dens_min <- min(cells_df$Cell_popdensity, na.rm = TRUE)
# dens_max <- max(cells_df$Cell_popdensity, na.rm = TRUE)
# density_seq <- exp(seq(log(pmax(dens_min, 1e-6)), log(dens_max + 1), length.out = 400))
# 
# curve_df <- data.frame(
#   Cell_popdensity = density_seq,
#   required_radius_m = required_radius_for_people(density_seq, people = 200)
# )
# 
# # 4) Plot scatter + theoretical line + caps
# min_cap <- 50
# max_cap <- 1000
# 
# 
# 
# 
# cells_df <- cells_df %>%
#   mutate(
#     required_r200 = required_radius_for_people(Cell_popdensity, people = 200),
#     required_r100 = required_radius_for_people(Cell_popdensity, people = 100),
#     above_required = distance_m > required_r200,
#     below_required = distance_m < required_r200,
#     below_required100 = distance_m < required_r100,
#     capped_low = distance_m <= min_cap,
#     capped_high = distance_m >= max_cap,
#     within_caps = (distance_m > min_cap & distance_m < max_cap)
#   )
# 
# # Count for 200-people radius
# counts200 <- cells_df %>%
#   group_by(below_required) %>%
#   summarise(n = n(), .groups = "drop")
# 
# n_blue  <- counts200$n[counts200$below_required == TRUE]
# n_orange <- counts200$n[counts200$below_required == FALSE]
# 
# # Count for 100-people radius
# counts100 <- cells_df %>%
#   group_by(below_required100) %>%
#   summarise(n = n(), .groups = "drop")
# 
# n_blue100  <- counts100$n[counts100$below_required100 == TRUE]
# n_orange100 <- counts100$n[counts100$below_required100 == FALSE]
# 
# # Values of people for the red lines
# people_values <- c(50, 100, 200, 400, 800)
# radius_y <- 2000  # y position for the labels
# x_positions <- sapply(people_values, function(p) {
#   (p * 1e6) / (pi * radius_y^2)  # correct density in people/km²
# })
# 
# 
# # Plot with annotations
# ggplot(cells_df, aes(x = Cell_popdensity, y = distance_m)) +
#   geom_point(aes(color = below_required), alpha = 0.7, size = 2) +
#   scale_color_manual(
#     values = c("TRUE" = "blue", "FALSE" = "orange"),
#     labels = c("Below required radius", "Above required radius")
#   ) +
#   # Theoretical radius for 200 people
#   stat_function(fun = function(dens_km2) {
#     sqrt(200 / (pi * (dens_km2 / 1e6)))
#   }, color = "red", size = 1) +
#   # Radius for 100 people
#   stat_function(fun = function(dens_km2) {
#     sqrt(100 / (pi * (dens_km2 / 1e6)))
#   }, color = "red", size = 1, linetype = "dashed") +
#   # Radius for 400 people
#   stat_function(fun = function(dens_km2) {
#     sqrt(400 / (pi * (dens_km2 / 1e6)))
#   }, color = "red", size = 1, linetype = "dashed") +
#   # Radius for 50/800 people 
#   stat_function(fun = function(dens_km2) {
#     sqrt(50 / (pi * (dens_km2 / 1e6)))
#   }, color = "red", size = 1, linetype = "dotted") +
#   stat_function(fun = function(dens_km2) {
#     sqrt(800 / (pi * (dens_km2 / 1e6)))
#   }, color = "red", size = 1, linetype = "dotted") +
#   # Min/max caps
#   geom_hline(yintercept = 50, linetype = "dashed") +
#   geom_hline(yintercept = 200, linetype = "dotted") +
#   geom_hline(yintercept = 1000, linetype = "dashed") +
#   geom_hline(yintercept = 1500, linetype = "dotted") +
#   # Add counts as text
#   annotate("text",
#            x = min(cells_df$Cell_popdensity, na.rm = TRUE) + 5,
#            y = max(cells_df$distance_m, na.rm = TRUE) + 200,
#            label = paste0("Below 200 threshold = ", n_blue),
#            color = "blue",
#            size = 4,
#            nudge_x = 0.1) +  # shifts text slightly to the right
#   annotate("text",
#            x = min(cells_df$Cell_popdensity, na.rm = TRUE) + 5,
#            y = max(cells_df$distance_m, na.rm = TRUE) + 100,
#            label = paste0("Below 100 threshold = ", n_blue100),
#            color = rgb(0, 0, 1, alpha = 0.5),  # blue with 50% transparency
#            size = 4,
#            nudge_x = 0.1) +  # shifts text slightly to the right
#   annotate("text", x = max(cells_df$Cell_popdensity, na.rm = TRUE), 
#            y = max(cells_df$distance_m, na.rm = TRUE) + 200,
#            label = paste0("Above 200 threshold = ", n_orange),
#            hjust = 1, color = "orange", size = 4) +
#   annotate("text", x = max(cells_df$Cell_popdensity, na.rm = TRUE), 
#            y = max(cells_df$distance_m, na.rm = TRUE) + 100,
#            label = paste0("Above 100 threshold = ", n_orange100),
#            hjust = 1, color = rgb(1, 0.5, 0, alpha = 0.5), size = 4) +
#   # Red numbers at y = 2000
#   annotate("text", x = x_positions, y = radius_y,
#            label = people_values, color = "red", size = 4) +
#   scale_x_log10(labels = comma_format(accuracy = 1)) +
#   scale_y_continuous(labels = comma_format(), limits = c(0, 2500))+
#   labs(
#     x = "Cell population density (people / km²) — log scale",
#     y = "Radius in meters (distance_m)",
#     color = "Relation to red curve",
#     title = "Anonymization distance vs. population density",
#     subtitle = "Red line = radius for 200 people, Dashed/Dotted = min/max caps.\nBlue = below curve, Orange = above curve"
#   ) +
#   theme_minimal()
# 
# # 5) Quick summary counts: how many above/below the theoretical and caps
# summary_tbl <- tibble(
#   n_total = nrow(cells_df),
#   n_above_required = sum(cells_df$above_required, na.rm = TRUE),
#   n_below_required = sum(cells_df$below_required, na.rm = TRUE),
#   n_capped_low = sum(cells_df$capped_low, na.rm = TRUE),
#   n_capped_high = sum(cells_df$capped_high, na.rm = TRUE),
#   n_within_caps = sum(cells_df$within_caps, na.rm = TRUE)
# )
# 
# print(summary_tbl)
# 
# 
# 
# percentile_summary <- anon_working_df %>%
#   summarise(
#       min  = min(distance_m, na.rm = TRUE),
#       p1  = quantile(distance_m, 0.01, na.rm = TRUE),
#       p2  = quantile(distance_m, 0.02, na.rm = TRUE),
#       p3  = quantile(distance_m, 0.03, na.rm = TRUE),
#       p4  = quantile(distance_m, 0.04, na.rm = TRUE),
#       p5  = quantile(distance_m, 0.05, na.rm = TRUE),
#       
#         d10 = quantile(distance_m, 0.1, na.rm = TRUE),
#       d20 = quantile(distance_m, 0.2, na.rm = TRUE),
#       d30 = quantile(distance_m, 0.3, na.rm = TRUE),
#       d40 = quantile(distance_m, 0.4, na.rm = TRUE),
#       d50 = quantile(distance_m, 0.5, na.rm = TRUE),
#       d60 = quantile(distance_m, 0.6, na.rm = TRUE),
#       d70 = quantile(distance_m, 0.7, na.rm = TRUE),
#       d80 = quantile(distance_m, 0.8, na.rm = TRUE),
#       d90 = quantile(distance_m, 0.9, na.rm = TRUE),
#       
#         
#         p95 = quantile(distance_m, 0.95, na.rm = TRUE),
#       p96 = quantile(distance_m, 0.96, na.rm = TRUE),
#       p97 = quantile(distance_m, 0.97, na.rm = TRUE),
#       p98 = quantile(distance_m, 0.98, na.rm = TRUE),
#       p99 = quantile(distance_m, 0.99, na.rm = TRUE),
#       max = max(distance_m, na.rm = TRUE)
#       )
# 
# percentile_summary
# 
# table(anon_working_df$anonym_radius_m)


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




# # ----------- CHECK -----
# # Reading in the updated xslx files again
# # Comparing it with the anonomyized_geolocations.rds (anon_df)
# 
# 
# #### PREP #####
# # Remove all other objects
# keep <- c("anon_df", "survey_round_map", "read_cov_reduced")
# rm(list = setdiff(ls(), keep))
# 
# # Apply to all covariate files
# raw_data_reduced <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
#   purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>%
#   map(read_cov_reduced)
# 
# # Combine into one dataframe with survey_round as identifier
# all_data_reduced <- bind_rows(raw_data_reduced, .id = "survey_round")
# 
# all_data_reduced <- all_data_reduced %>%
#   mutate(RID_unique = survey_round_map[survey_round] + RID) %>%
#   rename(RID_sample = RID, RID = RID_unique) %>%
#   select(RID, RID_sample, survey_round, lat, lon)
# 
# covariate_files <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates")
# backup_folder <- "data_backup"   # your backup folder
# report_folder <- "checks_report"
# dir_create(report_folder)
# 
# 
# 
# 
# 
# 
# #### 1. Check - XSLX correctly updated? ##################
# # Comparing anonymized dataframe with lightweight read-in of anonymized xlsx files
# 
# # Tolerance for numeric equality (adjust if needed)
# tol <- 1e-6
# 
# # Numeric equality helper tolerant to NA
# equal_num <- function(a, b, tol = 1e-6) {
#   a_na <- is.na(a)
#   b_na <- is.na(b)
#   both_na <- a_na & b_na
#   both_num_equal <- (!a_na & !b_na) & (abs(as.numeric(a) - as.numeric(b)) <= tol)
#   return(both_na | both_num_equal)
# }
# 
# # Basic sanity: ensure key columns exist
# stopifnot("RID" %in% names(all_data_reduced), "lat" %in% names(all_data_reduced), "lon" %in% names(all_data_reduced))
# stopifnot("RID" %in% names(anon_df) | "RID_sample" %in% names(anon_df))
# # if anon_df uses RID_sample as key rename to RID for the join
# if ("RID_sample" %in% names(anon_df) & !("RID" %in% names(anon_df))) {
#   anon_df <- anon_df %>% rename(RID = RID_sample)
# }
# 
# stopifnot("lat_anon" %in% names(anon_df), "lon_anon" %in% names(anon_df))
# 
# # Make sure RID types align
# all_data_reduced <- all_data_reduced %>% mutate(RID = as.numeric(RID))
# anon_df <- anon_df %>% mutate(RID = as.numeric(RID))
# 
# # How many unique RIDs in each
# n_reduced <- n_distinct(all_data_reduced$RID)
# n_anon    <- n_distinct(anon_df$RID)
# cat("Unique RIDs in all_data_reduced:", n_reduced, "\n")
# cat("Unique RIDs in anon_df:           ", n_anon, "\n")
# 
# # Overlap
# rids_reduced <- sort(unique(all_data_reduced$RID))
# rids_anon    <- sort(unique(anon_df$RID))
# rids_intersect <- intersect(rids_reduced, rids_anon)
# cat("RIDs in both:", length(rids_intersect), "\n")
# cat("RIDs only in reduced:", length(setdiff(rids_reduced, rids_anon)), "\n")
# cat("RIDs only in anon_df: ", length(setdiff(rids_anon, rids_reduced)), "\n\n")
# 
# # Merge rowwise by RID and compute diagnostics
# diag <- all_data_reduced %>%
#   select(RID, lat, lon) %>%
#   full_join(anon_df %>% select(RID, lat_anon, lon_anon), by = "RID") %>%
#   arrange(RID) %>%
#   mutate(
#     anon_available = (!is.na(lat_anon) & !is.na(lon_anon)),       # both anon coords present
#     lat_matches_anon = equal_num(lat, lat_anon, tol),
#     lon_matches_anon = equal_num(lon, lon_anon, tol),
#     both_match_anon = lat_matches_anon & lon_matches_anon,
#     orig_missing = is.na(lat) & is.na(lon),
#     anon_missing = is.na(lat_anon) & is.na(lon_anon)
#   )
# 
# # Summary statistics
# total_rows <- nrow(diag)
# n_anon_avail <- sum(diag$anon_available, na.rm = TRUE)
# pct_anon_avail <- n_anon_avail / total_rows
# n_both_match <- sum(diag$both_match_anon, na.rm = TRUE)
# pct_both_match_of_anon <- if (n_anon_avail>0) n_both_match / n_anon_avail else NA_real_
# n_lat_match <- sum(diag$lat_matches_anon, na.rm = TRUE)
# n_lon_match <- sum(diag$lon_matches_anon, na.rm = TRUE)
# 
# cat("Total rows (union):", total_rows, "\n")
# cat("Anon coords available (rows):", n_anon_avail, "(", scales::percent(pct_anon_avail), "of rows )\n")
# cat("Rows where both lat & lon equal anon:", n_both_match, " (", scales::percent(pct_both_match_of_anon), " of anon-available rows )\n")
# cat("Rows where lat matches anon:", n_lat_match, "\n")
# cat("Rows where lon matches anon:", n_lon_match, "\n\n")
# 
# # Which RIDs didn't match when anon was available?
# bad_matches <- diag %>%
#   filter(anon_available & !both_match_anon)
# 
# cat("Rows where anon available but updated coords DO NOT equal anon (n):", nrow(bad_matches), "\n")
# if (nrow(bad_matches) > 0) {
#   cat("Example mismatching RIDs (up to 10):", paste(head(bad_matches$RID, 10), collapse = ", "), "\n")
# }
# 
# # Which RIDs exist in reduced but have no anon entry
# missing_in_anon <- diag %>% filter(is.na(lat_anon) & is.na(lon_anon) & (!is.na(lat) | !is.na(lon)))
# cat("Rows in reduced that have NO anon entry (but have lat/lon in reduced):", nrow(missing_in_anon), "\n")
# 
# 
# rm(bad_matches, missing_in_anon, diag)
# 
# #### 2. Check - Orginal XSLX vs. Anonymized XSLX ##################
# read_cov <- function(x) {
# 
#   # Function to categorize device based on user agent string
#   extract_device_category <- function(ua_string) {
#     if (grepl("Windows", ua_string, ignore.case = TRUE)) {
#       return("Laptop/Desktop")
#     } else if (grepl("Macintosh|Mac OS X", ua_string, ignore.case = TRUE)) {
#       return("Laptop/Desktop")
#     } else if (grepl("CrOS", ua_string, ignore.case = TRUE)) {
#       return("Laptop/Desktop")  # Chrome OS devices are typically laptops
#     } else if (grepl("Android", ua_string, ignore.case = TRUE)) {
#       if (grepl("Mobile", ua_string, ignore.case = TRUE)) {
#         return("Mobile Phone")
#       } else {
#         return("Tablet")
#       }
#     } else if (grepl("iPhone", ua_string, ignore.case = TRUE)) {
#       return("Mobile Phone")
#     } else if (grepl("iPad", ua_string, ignore.case = TRUE)) {
#       return("Tablet")
#     } else {
#       return("Other/Unknown")  # Default category for unidentified devices
#     }
#   }
# 
#   # Function to extract specific device type
#   extract_device_type <- function(ua_string) {
#     if (grepl("Windows", ua_string, ignore.case = TRUE)) {
#       return("Windows")
#     } else if (grepl("Macintosh|Mac OS X", ua_string, ignore.case = TRUE)) {
#       return("Mac")
#     } else if (grepl("Linux", ua_string, ignore.case = TRUE)) {
#       return("Linux")
#     } else if (grepl("Android", ua_string, ignore.case = TRUE)) {
#       return("Android")
#     } else if (grepl("iPhone", ua_string, ignore.case = TRUE)) {
#       return("iPhone")
#     } else if (grepl("iPad", ua_string, ignore.case = TRUE)) {
#       return("iPad")
#     } else if (grepl("CrOS", ua_string, ignore.case = TRUE)) {
#       return("Chrome OS")
#     } else {
#       return("Other")  # Default category for unidentified operating systems
#     }
#   }
# 
#   # Identify DCE files in the same directory and read them
#   dcepath <- list.files(dirname(x), full.names = TRUE, pattern = "DCE")
#   # Read and merge DCE data
#   dcedata <- dcepath %>%
#     purrr::set_names(gsub("\\.xlsx", "", basename(.))) %>%
#     map(read_excel) %>%
#     bind_rows(.id = "dce_source") %>%
#     mutate(RID=as.numeric(RID), pref1 = ifelse(grepl("swap", dce_source), 3 - as.numeric(pref1), as.numeric(pref1)))  # Adjust pref1 based on dce_source
# 
#   # Read main dataset and process columns
#   raw_data <- read_excel(x) %>%
#     rename(lat = latlng_wood_SQ_1_1,
#            lon = latlng_wood_SQ_1_2,
#            lat_tc = latlng_wood_SQ2_1_1,
#            lon_tc = latlng_wood_SQ2_1_2,
# 
#            # Renaming the specified columns
#            participation_consent = q1,
#            res_settlement_type = q2_1,
#            knowledge_hnv_share = q10,
#            knowledge_pa_effectiveness = q14,
#            estimated_hnv_near_residence = q16,
#            estimated_pa_near_residence = q18,
#            envisioned_levy_distribution = q27_2,
# 
#            # q91test_1 to q91test_9 renamed to most_visited_nature_type_x
#            most_visited_nature_type_mountain = q91test_1,
#            most_visited_nature_type_grassland = q91test_2,
#            most_visited_nature_type_agriculture = q91test_3,
#            most_visited_nature_type_urbanpark = q91test_4,
#            most_visited_nature_type_forest = q91test_5,
#            most_visited_nature_type_lake = q91test_6,
#            most_visited_nature_type_coastal = q91test_7,
#            most_visited_nature_type_other = q91test_8,
#            most_visited_nature_type_unknown = q91test_9,
# 
#            # Renaming the q27_1_1 to q27_1_11 columns for clarity
#            eval_time_taken = q27_1_1,
#            eval_survey_meaningful = q27_1_2,
#            eval_response_consistency = q27_1_3,
#            eval_conscientious_reading = q27_1_4,
#            eval_attention_check = q27_1_5,
#            eval_understanding_difficulty = q27_1_6,
#            eval_alt_distinction_difficulty = q27_1_7,
#            eval_cost_realism = q27_1_8,
#            eval_referendum_realism = q27_1_9,
#            eval_measures_effectiveness_belief = q27_1_10,
#            eval_policy_relevance_assumption = q27_1_11
#     ) %>%
#     mutate(
#       RID = as.numeric(RID),
#       survey_round = gsub("_covariates.xlsx", "", basename(x)),
# 
# 
#       # Recoding STATUS variable
#       STATUS_recoded = case_when(
#         STATUS == 1 ~ "New respondent",
#         STATUS == 2 ~ "Invalid entry",
#         STATUS == 3 ~ "Pending",
#         STATUS == 4 ~ "Over quota on Segment Assignment",
#         STATUS == 5 ~ "Rejected",
#         STATUS == 6 ~ "Started",
#         STATUS == 7 ~ "Complete",
#         STATUS == 8 ~ "Screened out",
#         STATUS == 9 ~ "User initiated timeout",
#         STATUS == 10 ~ "System initiated timeout",
#         STATUS == 11 ~ "Bad parameters",
#         STATUS == 12 ~ "Survey closed",
#         STATUS == 13 ~ "System error",
#         STATUS == 14 ~ "Reentrant",
#         STATUS == 15 ~ "Active",
#         STATUS == 16 ~ "Over Quota at Start",
#         STATUS == 17 ~ "Manually Screened Out",
#         TRUE ~ NA_character_
#       ),
# 
#       # Recoding gender
#       gender_chr = case_when(
#         gender == 1 ~ "male",
#         gender == 2 ~ "female",
#         gender == 3 ~ "diverse",
#         gender == 4 ~ "na"
#       ),
#       gender_male = case_when(gender==1 ~ 1, TRUE~0),
# 
#       # Convert percentage-based numeric variables
#       sq_hnv_share = as.numeric(gsub("%", "", sq_hnv_share)),
#       sq_pa_share = as.numeric(gsub("%", "", sq_pa_share)),
#       cv = as.numeric(cv),
#       birthyear_uncleaned = as.numeric(birthyralt_other),
#       birthyear = ifelse(birthyear_uncleaned < 1900 | birthyear_uncleaned > 3000, NA, birthyear_uncleaned),
# 
# 
#       # Recode satisfaction, health, and income variables
#       lifesat_recode = coalesce(lifesat, lifesat_mobile) - 1,
#       healthphys_recode = coalesce(healthphys, healthphys_mobile) - 1,
#       healthpsych_recode = coalesce(healthpsych, healthpsych_mobile) - 1,
# 
# 
#       # recode househole income
#       hhnetinc_recode = factor(hhnetinc, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'),
#                                labels = c('less than 500 Euro', '500 - 999 Euro', '1000 - 1499 Euro',
#                                           '1500 - 1999 Euro', '2000 - 2499 Euro', '2500 - 2999 Euro',
#                                           '3000 - 3499 Euro', '3500 - 3999 Euro', '4000 - 4999 Euro',
#                                           'more than 5000 Euro', 'k.A.')),
#       hhnetinc_numeric = case_when(
#         hhnetinc == '1' ~ 250,
#         hhnetinc == '2' ~ 750,
#         hhnetinc == '3' ~ 1250,
#         hhnetinc == '4' ~ 1750,
#         hhnetinc == '5' ~ 2250,
#         hhnetinc == '6' ~ 2750,
#         hhnetinc == '7' ~ 3250,
#         hhnetinc == '8' ~ 3750,
#         hhnetinc == '9' ~ 4500,
#         hhnetinc == '10' ~ 5500,
#         TRUE ~ NA_real_  # Exclude 'k.A.'
#       ),
# 
#       # Recode voting and protest variables
#       voting = factor(pol_btw, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#                       labels = c("CDU/CSU", "SPD", "BSW", "AfD", "Die Linke", "Die Grünen", "FDP", "Keine Angabe", "Sonstige")),
#       protest_1_recode = factor(protest_1, levels = c("1", "2", "3", "4", "5", "6"),
#                                 labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
#       protest_2_recode = factor(protest_2, levels = c("1", "2", "3", "4", "5", "6"),
#                                 labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
#       protest_3_recode = factor(protest_3, levels = c("1", "2", "3", "4", "5", "6"),
#                                 labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
#       protest_4_recode = factor(protest_4, levels = c("1", "2", "3", "4", "5", "6"),
#                                 labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
#       protest_5_recode = factor(protest_5, levels = c("1", "2", "3", "4", "5", "6"),
#                                 labels = c("Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme voll und ganz zu", "K.A.")),
# 
#       # Recode urban vs rural category
#       urban_rural = case_when(
#         res_settlement_type < 3 ~ "Small City",
#         res_settlement_type < 5 ~ "Medium-size City",
#         res_settlement_type < 7 ~ "Large City",
#         TRUE ~ NA_character_
#       ),
#       dogowner = case_when(
#         dog %in% c(1, 2) ~ 1,  # Assign 1 if dog is 1 or 2
#         dog == 3 ~ 2,          # Assign 2 if dog is 3
#         TRUE ~ NA_real_        # Assign NA for all other cases
#       ),
# 
#       # Compute time spent in minutes
#       hours_spend = replace_na(as.numeric(hours_spend), 0),
#       minutes_spend = replace_na(as.numeric(minutes_spend), 0),
#       time_spend_tc = hours_spend * 60 + minutes_spend,
#       zoom_first_cc = case_when(getZoom1 > 0 ~ 1, TRUE~0),
#       payment_distribution = case_when(envisioned_levy_distribution == 1 ~ "Progressive", envisioned_levy_distribution == 2 ~ "Nobody pays", envisioned_levy_distribution == 3 ~ "Equal", envisioned_levy_distribution == 4 ~"Not thought about it"),
#       payment_vision = if ("q1171" %in% names(.)) {
#         case_when(
#           q1171 == 1 ~ "Every household the same",
#           q1171 == 2 ~ "Relative to their income tax",
#           q1171 == 3 ~ "Do not care about distribution",
#           q1171 == 4 ~ "Other"
#         )
#       } else {
#         NA_character_  # Return NA if q1171 does not exist
#       },
# 
#       # Device-related classifications
#       device_type = sapply(respondent_ua, extract_device_type),
#       device_category = sapply(respondent_ua, extract_device_category)
#     ) %>%
# 
#     # Merge timestamps and calculate time spend on page
#     left_join(read_excel(gsub("covariates", "timestamps", x) , col_types = "numeric") %>% mutate(RID=as.numeric(RID)), by = "RID") %>%
#     mutate(across(starts_with("PAGE_SUBMIT"), ~ . - get(sub("SUBMIT", "DISPLAY", cur_column())), .names = "time_spent_{col}")) %>%
# 
# 
# 
#     # Merge DCE data
#     left_join(dcedata, by = "RID") %>%
# 
#     # Compute total preference scores and categorize protester types
#     group_by(RID) %>%
#     mutate(
#       total_pref1 = sum(pref1, na.rm = TRUE),
#       protester = case_when(
#         total_pref1 == 20 ~ 0,
#         total_pref1 > 13 ~ 1,
#         total_pref1 > 10 & total_pref1 <= 13 ~ 2,
#         total_pref1 == 10 ~ 3,
#         TRUE ~ NA_real_
#       )
#     ) %>%
#     ungroup() %>%
# 
#     # Create dummy variables for specific conditions
#     mutate(
#       Dummy_pa_half = case_when(a2_x2 == 2 ~ 1, TRUE ~ 0),
#       Dummy_pa_full = case_when(a2_x2 == 3 ~ 1, TRUE ~ 0),
#       Dummy_hnv_visible = case_when(a2_x4 == 2 ~ 1, TRUE ~ 0),
#       Dummy_pa_no = case_when(a2_x2 == 1 ~ 1, TRUE ~ 0),
#       Dummy_hnv_no = case_when(a2_x4 == 1 ~ 1, TRUE ~ 0),
#       # Compute hnv_att and pa_att based on experiment type and response
#       hnv_att = case_when(
#         dce_version %in% c(1, 2) & a2_x3 == 1 ~ sq_hnv_area + 100,
#         dce_version %in% c(1, 2) & a2_x3 == 2 ~ sq_hnv_area + 200,
#         dce_version %in% c(1, 2) & a2_x3 == 3 ~ sq_hnv_area + 300,
#         dce_version %in% c(1, 2) & a2_x3 == 4 ~ sq_hnv_area + 500,
#         dce_version %in% c(1, 2) & a2_x3 == 5 ~ sq_hnv_area + 800,
# 
#         dce_version %in% c(3, 4) & a2_x3 == 1 ~ sq_hnv_area + 200,
#         dce_version %in% c(3, 4) & a2_x3 == 2 ~ sq_hnv_area + 400,
#         dce_version %in% c(3, 4) & a2_x3 == 3 ~ sq_hnv_area + 600,
#         dce_version %in% c(3, 4) & a2_x3 == 4 ~ sq_hnv_area + 1000,
#         dce_version %in% c(3, 4) & a2_x3 == 5 ~ sq_hnv_area + 1600
#       ),
#       pa_att = case_when(
#         dce_version %in% c(1, 2) & a2_x1 == 1 ~ sq_pa_area + 100,
#         dce_version %in% c(1, 2) & a2_x1 == 2 ~ sq_pa_area + 200,
#         dce_version %in% c(1, 2) & a2_x1 == 3 ~ sq_pa_area + 300,
#         dce_version %in% c(1, 2) & a2_x1 == 4 ~ sq_pa_area + 500,
#         dce_version %in% c(1, 2) & a2_x1 == 5 ~ sq_pa_area + 800,
# 
#         dce_version %in% c(3, 4) & a2_x1 == 1 ~ sq_pa_area + 200,
#         dce_version %in% c(3, 4) & a2_x1 == 2 ~ sq_pa_area + 400,
#         dce_version %in% c(3, 4) & a2_x1 == 3 ~ sq_pa_area + 600,
#         dce_version %in% c(3, 4) & a2_x1 == 4 ~ sq_pa_area + 1000,
#         dce_version %in% c(3, 4) & a2_x1 == 5 ~ sq_pa_area + 1600),
# 
#       # Assign cost based on response levels
#       cost_att = case_when(
#         grepl("pilot", survey_round) ~ case_when(
#           a2_x5 == 1 ~ 5,
#           a2_x5 == 2 ~ 10,
#           a2_x5 == 3 ~ 40,
#           a2_x5 == 4 ~ 80,
#           a2_x5 == 5 ~ 120,
#           a2_x5 == 6 ~ 150,
#           a2_x5 == 7 ~ 200,
#           a2_x5 == 8 ~ 250,
#           TRUE ~ NA_real_
#         ),
#         grepl("Main", survey_round) ~ case_when(
#           a2_x5 == 1 ~ 5,
#           a2_x5 == 2 ~ 10,
#           a2_x5 == 3 ~ 20,
#           a2_x5 == 4 ~ 40,
#           a2_x5 == 5 ~ 60,
#           a2_x5 == 6 ~ 80,
#           a2_x5 == 7 ~ 120,
#           a2_x5 == 8 ~ 150,
#           a2_x5 == 9 ~ 200,
#           a2_x5 == 10 ~ 250,
#           TRUE ~ NA_real_
#         ),
#         TRUE ~ NA_real_  # Default case for unexpected values
#       )
#       ,
# 
#       scope_dce = case_when(dce_version %in% c(1,2)~"low", TRUE~"high"),
# 
#       survey_round_pooled = case_when(grepl("pilot", survey_round) ~ "Pilot", TRUE ~ "Main"),
#       device = case_when(
#         str_detect(respondent_ua, "iPhone") ~ "iPhone",
#         str_detect(respondent_ua, "iPad") ~ "iPad",
#         str_detect(respondent_ua, "Android") ~ "Android",
#         TRUE ~ "Laptop/Other"
#       ),
#       NR_score = ifelse(uhh != 3 & nr6_1 != 6 & nr6_2 != 6 & nr6_3 != 6 &
#                           nr6_4 != 6 & nr6_5 != 6 & nr6_6 != 6,
#                         (nr6_1 + nr6_2 + nr6_3 + nr6_4 + nr6_5 + nr6_6) / 6,
#                         NA),
# 
#       # Convert character variables to appropriate types
#       across(where(is.character), ~ type.convert(.x, as.is = TRUE)),
#       across(any_of(c("lat","lon","birthyralt_other","natvisit_company","forest_size", "natvisit_next12m", "false_zip", "allocationa_0630", "allocationb_0630", "slope_0630", "to_a_max_0630",
#                       "to_b_max_0630", "allocationa_0820", "allocationb_0820", "slope_0820" ,"to_a_max_0820", "to_b_max_0820", "hhsize", "postcode", "pref1")), as.numeric),
#     )
# 
#   return(raw_data)
# }
# 
# 
# # Read all files into a list of data frames
# raw_data <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
#   purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>%
#   map(read_cov)
# 
# 
# all_data <- bind_rows(raw_data, .id = "survey_round")
# 
# survey_round_map <- all_data %>%
#   distinct(survey_round) %>%
#   mutate(prefix = row_number() * 100000) %>%  # Assign a unique 10,000s prefix
#   deframe()
# 
# # Generate `RID_unique`
# all_data <-all_data %>%
#   mutate(RID_unique = survey_round_map[survey_round] + RID) %>%
#   arrange(RID_unique) %>%
#   select(-RID,-RID_sample) %>%
#   rename(RID=RID_unique,
#          preferred_levy_distribution = q1171,
#          visited_nature_last12m = tc1,
#          natvisit_last12m_estimation_basis = nv_2a,
#          natvisit_fav_estimation_basis = nv_4a)
# 
# all_data <- all_data %>%   distinct(RID,.keep_all=TRUE)
# 
# 
# # --- Overwrite the updated files with their backed up original version ---
# # overwrite_from_backup.R
# # Copies specified .xlsx files from data_backup/ to target subfolders under data/
# # Overwrites existing files, creates missing directories, reports summary.
# 
# copy_file_overwrite <- function(src, dest) {
#   # Ensure destination directory exists
#   dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
#   # Try copying and return logical success and optional message
#   tryCatch({
#     ok <- file.copy(from = src, to = dest, overwrite = TRUE)
#     if (!isTRUE(ok)) stop("file.copy returned FALSE")
#     list(success = TRUE, src = src, dest = dest, msg = NULL)
#   }, error = function(e) {
#     list(success = FALSE, src = src, dest = dest, msg = conditionMessage(e))
#   })
# }
# 
# main <- function() {
#   # Base folders (relative to working directory)
#   backup_dir <- "data_backup"
#   data_dir   <- "data"
# 
#   # Check backup folder exists
#   if (!dir.exists(backup_dir)) {
#     stop(sprintf("Backup folder not found: '%s'. Run this script from the project root or adjust paths.", backup_dir))
#   }
# 
#   results <- list()
# 
#   # 1) Main_i files (i = 1..7)
#   for (i in seq_len(7)) {
#     fname <- sprintf("Main_%d_covariates.xlsx", i)
#     src <- file.path(backup_dir, fname)
#     target <- file.path(data_dir, "main_study", sprintf("Main_%d", i), fname)
# 
#     if (!file.exists(src)) {
#       results[[paste0("Main_", i)]] <- list(success = FALSE, src = src, dest = target, msg = "source_not_found")
#       message(sprintf("[MISSING] %s (expected at %s)", fname, src))
#       next
#     }
# 
#     res <- copy_file_overwrite(src, target)
#     if (res$success) {
#       message(sprintf("[COPIED] %s -> %s", src, target))
#     } else {
#       message(sprintf("[FAILED] %s -> %s : %s", src, target, res$msg))
#     }
#     results[[paste0("Main_", i)]] <- res
#   }
# 
#   # 2) Pilot files
#   pilots <- list(
#     list(fname = "pilot_2.1_covariates.xlsx", target_dir = file.path(data_dir, "pilot", "Pilot_2.1")),
#     list(fname = "pilot_3_covariates.xlsx",   target_dir = file.path(data_dir, "pilot", "Pilot_3")),
#     list(fname = "pilot_4_covariates.xlsx",   target_dir = file.path(data_dir, "pilot", "Pilot_4"))
#   )
# 
#   for (p in pilots) {
#     src <- file.path(backup_dir, p$fname)
#     target <- file.path(p$target_dir, p$fname)
#     key <- paste0("pilot-", p$fname)
# 
#     if (!file.exists(src)) {
#       results[[key]] <- list(success = FALSE, src = src, dest = target, msg = "source_not_found")
#       message(sprintf("[MISSING] %s (expected at %s)", p$fname, src))
#       next
#     }
# 
#     res <- copy_file_overwrite(src, target)
#     if (res$success) {
#       message(sprintf("[COPIED] %s -> %s", src, target))
#     } else {
#       message(sprintf("[FAILED] %s -> %s : %s", src, target, res$msg))
#     }
#     results[[key]] <- res
#   }
# 
#   # Summary
#   cat("\n=== SUMMARY ===\n")
#   n_copied <- sum(vapply(results, function(x) is.list(x) && isTRUE(x$success), logical(1)))
#   n_total  <- length(results)
#   n_failed <- n_total - n_copied
#   cat(sprintf("Total targets: %d\n", n_total))
#   cat(sprintf("Successfully copied: %d\n", n_copied))
#   cat(sprintf("Failed or missing: %d\n\n", n_failed))
# 
#   if (n_failed > 0) {
#     cat("Failures / Missing sources:\n")
#     for (nm in names(results)) {
#       r <- results[[nm]]
#       if (!is.list(r)) next
#       if (!isTRUE(r$success)) {
#         reason <- if (!is.null(r$msg)) r$msg else "unknown"
#         cat(sprintf(" - %s\n     src:  %s\n     dest: %s\n     reason: %s\n",
#                     nm, r$src %||% "NA", r$dest %||% "NA", reason))
#       }
#     }
#   }
# 
#   invisible(results)
# }
# 
# # helper: null-coalesce
# `%||%` <- function(a, b) if (!is.null(a)) a else b
# 
# # Run
# main()

# 
# # Read all files into a list of data frames
# raw_data_backup <- list.files("data", full.names = TRUE, recursive = TRUE, pattern = "covariates") %>%
#   purrr::set_names(gsub("_covariates.xlsx","",basename(.))) %>%
#   map(read_cov)
# 
# 
# all_data_backup <- bind_rows(raw_data_backup, .id = "survey_round")
# 
# survey_round_map_backup <- all_data_backup %>%
#   distinct(survey_round) %>%
#   mutate(prefix = row_number() * 100000) %>%  # Assign a unique 10,000s prefix
#   deframe()
# 
# # Generate `RID_unique`
# all_data_backup <- all_data_backup %>%
#   mutate(RID_unique = survey_round_map_backup[survey_round] + RID) %>%
#   arrange(RID_unique) %>%
#   select(-RID) %>%
#   rename(RID=RID_unique,
#          preferred_levy_distribution = q1171,
#          visited_nature_last12m = tc1,
#          natvisit_last12m_estimation_basis = nv_2a,
#          natvisit_fav_estimation_basis = nv_4a)
# 
# all_data_backup <- all_data_backup %>%   distinct(RID,.keep_all=TRUE)
# 
# 
# ### Checks #########
# equal_num <- function(a, b, tol = 1e-8) {
#   a_na <- is.na(a)
#   b_na <- is.na(b)
#   both_na <- a_na & b_na
#   both_num_equal <- (!a_na & !b_na) & (abs(a - b) <= tol)
#   both_na | both_num_equal
# }
# 
# # --- 1. Compare dimensions ---
# cat("Dimensions:\n")
# cat("all_data:", dim(all_data), "\n")
# cat("all_data_backup:", dim(all_data_backup), "\n\n")
# 
# # --- 2. Compare column names ---
# cols_only_in_all_data <- setdiff(names(all_data), names(all_data_backup))
# cols_only_in_backup <- setdiff(names(all_data_backup), names(all_data))
# cat("Columns only in all_data:", paste(cols_only_in_all_data, collapse = ", "), "\n")
# cat("Columns only in backup:", paste(cols_only_in_backup, collapse = ", "), "\n\n")
# 
# # --- 3. Compare common columns row by row ---
# common_cols <- intersect(names(all_data), names(all_data_backup))
# 
# diff_list <- map(common_cols, ~ {
#   col <- .x
#   # convert both columns to character for comparison, lowercase for logicals
#   a <- all_data[[col]]
#   b <- all_data_backup[[col]]
#   
#   if (is.logical(a)) a <- tolower(as.character(a))
#   if (is.logical(b)) b <- tolower(as.character(b))
#   
#   diff_rows <- which(a != b | (is.na(a) != is.na(b)))
#   
#   if (length(diff_rows) > 0) {
#     tibble(
#       column = col,
#       row = diff_rows,
#       all_data_value = as.character(a[diff_rows]),
#       backup_value   = as.character(b[diff_rows])
#     )
#   } else {
#     NULL
#   }
# }) %>% compact()
# 
# 
# # --- 4. Report results ---
# if (length(diff_list) == 0) {
#   cat("✅ all_data and all_data_backup are identical (up to tolerance for numeric columns).\n")
# } else {
#   cat("⚠ Differences found:\n")
#   bind_rows(diff_list)
# }
# 
# #### Changes to 24,142 rows 
# ## 24,087 anonymized geolocations.
# ## 55 outside of Germany - removed the lat,lon