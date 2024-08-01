# This script calculates proportion of trees per block group that are of low, medium, or high vulnerability to climate change
# author: Adrienne Keller
# dependencies: 00_build_MSPtreeinventory
# output: treeinv_CVI_df (df, NOT sf, with proportion of trees in low, mod, high vuln for low and high emissions scenario) - at census tract level
################################################################################

# read in Climate Vulnerability Index from Brandt et al. 2021 Frontiers in Ecology and Evolution
CVI <- read.csv("R/vulnerability_index_species.csv",
                stringsAsFactors = F)
CVI <- CVI %>%
  filter(!is.na(epithet)) %>%
  unite("genus_epi", c("Genus", "epithet"), sep = " ") %>%
  dplyr::select(c("genus_epi", "Minneapolis.vuln.high.emissions", "Minneapolis.vuln.low.emissions")) %>%
  mutate(vuln_high_emissions = ifelse(Minneapolis.vuln.high.emissions == "moderate-high" |
                                        Minneapolis.vuln.high.emissions == "high", "high",
                                      ifelse(Minneapolis.vuln.high.emissions == "moderate", "moderate", "low"))) %>%
  mutate(vuln_low_emissions = ifelse(Minneapolis.vuln.low.emissions == "moderate-high" |
                                       Minneapolis.vuln.low.emissions == "high", "high",
                                     ifelse(Minneapolis.vuln.low.emissions == "moderate", "moderate", "low"))) %>%
  distinct()

### CENSUS TRACT ANALYSIS ----
### HIGH EMISSIONS analysis
# create slim df of tree layers with CVI
treeinv <- left_join(tree_layers[ , "genus_epi"], CVI, by = "genus_epi")
treeinv_cvi <- treeinv[ , c("genus_epi", "vuln_high_emissions")] # creates slim df with species and geometry only

# add census geometry to tree inventory (slim)
treeinv_censustract <- st_join(MSP_censustract, treeinv_cvi)
treeinv_censustract <- treeinv_censustract %>% filter(!is.na(vuln_high_emissions))

# filter out census tracts with fewer than 20 trees
tract_keep_CVI <- treeinv_censustract %>% as.data.frame() %>% group_by(TRACTCE) %>%
  summarise(n = length(genus_epi)) %>%
  filter(n >= 20) %>%
  dplyr::select(TRACTCE)
treeinv_censustract <- inner_join(treeinv_censustract, tract_keep_CVI, by = "TRACTCE")
tree_C_high <- treeinv_censustract

# sf of tracts included in high emissions analysis
aoi_tracts_highemissions <- treeinv_censustract %>% dplyr::select(TRACTCE) %>%
  group_by(TRACTCE) %>% summarise(geometry = st_union(geometry))

# calculate proportion of trees in each census tract that are in a given vuln category
total_trees_censustract <- treeinv_censustract %>% group_by(TRACTCE) %>%
  summarise(n_tract = length(vuln_high_emissions))
trees_per_vuln_cat <- treeinv_censustract %>% group_by(TRACTCE, vuln_high_emissions) %>%
  summarise(n_vuln_high_emissions = length(vuln_high_emissions)) %>%
  st_drop_geometry()
trees_per_vuln_cat_wide <- trees_per_vuln_cat %>% spread(vuln_high_emissions, n_vuln_high_emissions) %>%
  mutate(low = ifelse(is.na(low), 0, low),
         moderate = ifelse(is.na(moderate), 0, moderate),
         high = ifelse(is.na(high), 0, high))
trees_per_vuln_cat <- trees_per_vuln_cat_wide %>% gather("vuln_high_emissions", "n_vuln_high_emissions", -TRACTCE)
treeinv_CVI_high <- left_join(total_trees_censustract, trees_per_vuln_cat, by = "TRACTCE")
treeinv_CVI_high <- st_as_sf(treeinv_CVI_high)
treeinv_CVI_high$vuln_high_emissions_prop <- (treeinv_CVI_high$n_vuln_high_emissions/treeinv_CVI_high$n_tract)*100
treeinv_CVI_high_highvuln <- treeinv_CVI_high %>% filter(vuln_high_emissions == "high")
treeinv_CVI_high_modvuln <- treeinv_CVI_high %>% filter(vuln_high_emissions == "moderate")
treeinv_CVI_high_lowvuln <- treeinv_CVI_high %>% filter(vuln_high_emissions == "low")

### LOW EMISSIONS analysis
# create slim df of tree layers with CVI
treeinv <- left_join(tree_layers[ , "genus_epi"], CVI, by = "genus_epi")
treeinv_cvi <- treeinv[ , c("genus_epi", "vuln_low_emissions")] # creates slim df with species and geometry only

# add census geometry to tree inventory (slim)
treeinv_censustract <- st_join(MSP_censustract, treeinv_cvi)
treeinv_censustract <- treeinv_censustract %>% filter(!is.na(vuln_low_emissions))

# filter out census tracts with fewer than 20 trees
tract_keep_CVI <- treeinv_censustract %>% as.data.frame() %>% group_by(TRACTCE) %>%
  summarise(n = length(genus_epi)) %>%
  filter(n >= 20) %>%
  dplyr::select(TRACTCE)
treeinv_censustract <- inner_join(treeinv_censustract, tract_keep_CVI, by = "TRACTCE")
tree_C_low <- treeinv_censustract

# sf of tracts included in low emissions analysis
aoi_tracts_lowemissions <- treeinv_censustract %>% dplyr::select(TRACTCE) %>%
  group_by(TRACTCE) %>% summarise(geometry = st_union(geometry))

# calculate proportion of trees in each census tract that are in a given vuln category
total_trees_censustract <- treeinv_censustract %>% group_by(TRACTCE) %>%
  summarise(n_tract = length(vuln_low_emissions))
trees_per_vuln_cat <- treeinv_censustract %>% group_by(TRACTCE, vuln_low_emissions) %>%
  summarise(n_vuln_low_emissions = length(vuln_low_emissions)) %>%
  st_drop_geometry()
trees_per_vuln_cat_wide <- trees_per_vuln_cat %>% spread(vuln_low_emissions, n_vuln_low_emissions) %>%
  mutate(low = ifelse(is.na(low), 0, low),
         moderate = ifelse(is.na(moderate), 0, moderate),
         high = ifelse(is.na(high), 0, high))
trees_per_vuln_cat <- trees_per_vuln_cat_wide %>% gather("vuln_low_emissions", "n_vuln_low_emissions", -TRACTCE)
treeinv_CVI_low <- left_join(total_trees_censustract, trees_per_vuln_cat, by = "TRACTCE")
treeinv_CVI_low <- st_as_sf(treeinv_CVI_low)
treeinv_CVI_low$vuln_low_emissions_prop <- (treeinv_CVI_low$n_vuln_low_emissions/treeinv_CVI_low$n_tract)*100
treeinv_CVI_low_lowvul <- treeinv_CVI_low %>% filter(vuln_low_emissions == "low")
treeinv_CVI_low_modvul <- treeinv_CVI_low %>% filter(vuln_low_emissions == "moderate")
treeinv_CVI_low_highvul <- treeinv_CVI_low %>% filter(vuln_low_emissions == "high")

# combine into one CVI df
treeinv_CVI_low_slim <- treeinv_CVI_low %>% as.data.frame() %>%
  dplyr::select(c(TRACTCE, vuln_low_emissions,vuln_low_emissions_prop))
treeinv_CVI_df_low <- treeinv_CVI_low_slim %>%
  pivot_wider(names_from = vuln_low_emissions, values_from = vuln_low_emissions_prop) %>%
  mutate(high = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                       ifelse(is.na(high), 0, high))) %>%
  mutate(moderate = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                           ifelse(is.na(moderate), 0, moderate))) %>%
  mutate(low = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                      ifelse(is.na(low), 0, low))) %>%
  rename(lowemissions_highvuln = high,
         lowemissions_lowvuln = low,
         lowemissions_modvuln = moderate)

treeinv_CVI_high_slim <- treeinv_CVI_high %>% as.data.frame() %>%
  dplyr::select(c(TRACTCE, vuln_high_emissions, vuln_high_emissions_prop))
treeinv_CVI_df_high <- treeinv_CVI_high_slim %>%
  pivot_wider(names_from = vuln_high_emissions, values_from = vuln_high_emissions_prop) %>%
  mutate(high = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                       ifelse(is.na(high), 0, high))) %>%
  mutate(moderate = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                           ifelse(is.na(moderate), 0, moderate))) %>%
  mutate(low = ifelse(is.na(high) & is.na(low) & is.na(moderate), NA,
                      ifelse(is.na(low), 0, low))) %>%
  rename(highemissions_highvuln = high,
         highemissions_lowvuln = low,
         highemissions_modvuln = moderate)
treeinv_CVI_df <- dplyr::full_join(treeinv_CVI_df_low, treeinv_CVI_df_high, by = "TRACTCE")
treeinv_CVI_sf <- dplyr::left_join(treeinv_CVI_df, MSP_censustract)
treeinv_CVI_sf <- st_as_sf(treeinv_CVI_sf)
