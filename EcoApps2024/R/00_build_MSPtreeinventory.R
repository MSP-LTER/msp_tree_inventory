# This script builds the MSP tree inventory dataset
# author: Adrienne Keller
# dependencies: none
################################################################################

### Notes:
# species names: I have manually edited names (via recode csv file) and have
#  elected NOT to resolve names via taxise() or other package.
#  I format hybrid species names as Genus x hybrid specific epithet. In some cases
#  (e.g., Platanus x acerifolia) this may not be the accepted name, but I have
#  elected to keep the x for all hybrids (identified manually as such by AK,
#  this designation may also not be 100% correct) for consistency and to be able
#  to quickly identify hybrids. Another option could be to add a column: hybrid 0/1
#  (but I decided against this).

# column names metadata
#  column names have been shorted to 10 characters to allow for consistent output
#  to shapefile. Here are longer names and descriptions for column names
#  entity = entity name (name of municipality, agency, non-profit, etc that provided data)
#  data_restr = data restrictions (0/1); 0 = no data restrictions, 1 = some data restrictions, see MetaData spreadsheet file
#  date_1 = earliest date of inventory recorded (does not include planting dates, but rather dates of inventory data taken, maintenance, etc)
#  date_2 = latest date of inventory recorded
#  ash_only = ash only (0/1); 1 = inventory only includes ash trees
#  stem = number of stems (most are NA, i.e., no stem count available)
#  dbh_in = dbh in units inches
#  maintenanc = maintenance (column that contains information on maintenance)
#  exist_rmvd = exists or removed (information about whether the tree has been removed or still exists)
#  land_type = information about land type/use provided in original inventory
#  land_use = manually coded, using information primarily from land_type to assign: "park", "private", or "street"
#  disease = disease (column that contains information on disease)
#  condition = condition (column that contains information on tree condition)
#  dbh_char = dbh are recorded in type character (rather than numeric), sometimes this is a range or just general size of tree
#  given_name = name from tree inventory prior to recoding (manually translating to latin species names) or resolving (taxize and then manual edits to names)
#  genus_epi = manually edited scientific name - name from tree inventory after manually translating to latin species names
#  genus = genus of manually recoded name from genus_epi
#  epithet = the specific epithet of manually recoded name from genus_epi

rm(list = ls())

# load packages
#install.packages("pacman")
pacman::p_load(tidycensus, tidyverse, dplyr, plotly, tigris, readxl, tmap, rgeos,
               sp, sf, rgdal, fuzzySim, patchwork, spdep, raster, tmaptools,
               RColorBrewer, shinyjs, vegan, car)

# source fncs
source(file = "R_analyses/fcns_MSPtreeinvetory.R")

# read in tree_layers compiled from 01_compile-tree-inventories (this 01 script takes a long time, so best to just read-in compiled tree layers )
z <- unzip("R/tree_layers_all.csv.zip")
tree_csv <- read.csv(z[1])
#tree_csv <- read.csv(file = "R/tree_layers_all.csv")
tree_layers_all <- st_as_sf(tree_csv, coords = c("X", "Y"), crs = 26915)
tree_layers_all$plot_level <- ifelse(tree_layers_all$entity == "tchep" |
                                       tree_layers_all$entity == "macro1", 1, 0)
tree_layers_all <- tree_layers_all %>%
  rename(data_restr = "dt_rstr",
         ash_only = "ash_nly",
         exist_rmvd = "exst_rm",
         maintenanc = "mantnnc",
         land_type = "lnd_typ",
         condition = "conditn",
         dbh_char = "dbh_chr",
         given_name = "givn_nm",
         genus_epi = "genus_p",
         COCTU_ID = "COCTU_I",
         COCTU_CODE = "COCTU_C",
         COCTU_DESC = "COCTU_D",
         CTU_ID_CEN = "CTU_ID_",
         CTU_CODE = "CTU_COD",
         CTU_NAME = "CTU_NAM",
         COMCD_CENS = "COMCD_C",
         Shape_Leng = "Shp_Lng",
         Shape_Area = "Shap_Ar",
         COUNTYFP = "COUNTYF",
         NAMELSA = "NAMELSA",
         FUNCSTAT = "FUNCSTA",
         INTPTLO = "INTPTLO",
         trees_landcov = "trs_lnd",
         land_cover = "lnd_cvr")

# define land_cover
tree_layers_all <- tree_layers_all %>%
  mutate(land_cover = dplyr::recode(land_cover, '0' = "unk", '1' = "vacant", '2' = "industrial",
                            '3' = "agriculture", '4' = "rec_parkland", '5' = "commercial",
                            '6' = "residential", '7' = "water", '8' = "major_hwy",
                            '9' = "street"))

# redefine land_cover and land_cover_groups
tree_layers_all <- tree_layers_all %>%
  mutate(land_cover = ifelse(land_cover == "water", "rec_parkland",
                             ifelse(land_cover == "major_hwy", "rec_parkland",
                                    land_cover))) %>%
  mutate(land_cover_groups = ifelse(land_cover == "street", "street",
                                    ifelse(land_cover == "rec_parkland", "rec_parkland", "other")))#

# read in political geometries
CoCTU <- st_read("./Data/TreeInventory_GISData/_covariates/MSP_CountyCTUs/CountiesAndCTUs.shp")
CoCTU <- st_make_valid(CoCTU)

# download census track for 7 country metro area
MSP_censustract <- tigris::tracts(state = "MN",
                                  county = c("Anoka", "Dakota", "Washington", "Ramsey", "Hennepin", "Carver", "Scott"))
MSP_censustract <- st_transform(MSP_censustract, crs = 26915)

# clip to MSP 7-county area
tree_layers_all <- tree_layers_all[CoCTU, ]

# create aoi
aoi_tracts <- MSP_censustract %>% group_by(TRACTCE) %>% summarize(geometry = st_union(geometry))
aoi_metro <- MSP_censustract %>% st_union()
aoi_counties <- CoCTU %>% group_by(CO_NAME) %>% mutate(geometry = st_union(geometry))
minneapolis_bound <- CoCTU %>% filter(CTU_NAME == "Minneapolis") %>% st_union
stpaul_bound <- CoCTU %>% filter(CTU_NAME == "St. Paul") %>% st_union
minnstpaul <- CoCTU %>% filter(CTU_NAME == "Minneapolis" | CTU_NAME == "St. Paul") %>% st_union
exurban <- CoCTU %>% filter(CTU_NAME != "Minneapolis" & CTU_NAME != "St. Paul") %>% st_union
msp_tracts <- MSP_censustract %>% group_by(TRACTCE) %>%
  summarise(geometry = st_union(geometry))

### Create datasets A and B
tree_A <- tree_layers_all # dataset A includes all point data included in full inventory
tree_B <- tree_layers_all %>% filter(epithet != "spp.") # dataset B includes only point data defined to species and with 20 or more trees per census tract
tract_keep <- tree_B %>% as.data.frame() %>% group_by(TRACTCE) %>%
  summarise(n = length(genus_epi)) %>%
  filter(n >= 20) %>%
  dplyr::select(TRACTCE)
tree_B <- inner_join(tree_B, tract_keep, by = "TRACTCE")
#tree_C: dataset C will be filtered in 11_CVI-analysis script below; dataset C includes only species with CVI and tracts with 20 or more trees
#tree_C_high and tree_C_low are tree point data specific to low and high emissions (essentially the same point data but with emission-specific data)

### Calculate diversity for dataset B
# set data
tree_layers <- tree_B

# source species diversity calculations
source(file = "./R_analyses/10_calc-species-diversity.R")

### Calculate CVI and SVI and create dataset C, pulling from tree_B
# set data
tree_layers <- tree_B

# source CVI calculations
source(file = "R_analyses/11_CVI-analysis.R")

# source SVI data
source(file = "R_analyses/12_SVI.R")

# join CVI and SVI to create final tree_C_tracts summarized by tracts
tree_C_tracts <- dplyr::left_join(treeinv_spdiv, treeinv_CVI_df, by = "TRACTCE")
tree_C_tracts <- dplyr::left_join(tree_C_tracts, SVI_df, by = "TRACTCE")

