# This script calculates multiple metrics of species diversity using compiled tree inventory
# author: Adrienne Keller
# dependencies: 00_build_MSPtreeinventory
# output: treeinv_spdiv (multipolygon sf that has species diversity at census tract level) - unique ID = GEOID
# treeinv_spdiv_CTU (multipolygon sf that has species diversity at CTU level) - unique ID = CTU_NAME
################################################################################

### CENSUS TRACK ANALYSIS ----
### create species matrix for each census track from tree inventory data
# first, select species column (geometry sticks)
sp <- "genus_epi" # change to appropriate column name with species ID
treeinv <- tree_layers # change to specific inventory name, if relevant
treeinv_sp <- treeinv[ , sp] # creates slim df with species and geometry only

# create species matrix
treeinv_splong <- as.data.frame(treeinv[ , c("TRACTCE", sp)])
treeinv_splong <- treeinv_splong %>% filter(!is.na(genus_epi))
treeinv_spshort <- fuzzySim::splist2presabs(treeinv_splong,
                                            sites.col = "TRACTCE", sp.col = sp, keep.n = T)
treeinv_spshort[,-1] <- lapply(treeinv_spshort[, -1], function(x) as.numeric(x))
row.names(treeinv_spshort) <- treeinv_spshort$TRACTCE
treeinv_spshort[1] <- NULL

### calculate diversity indices
# Shannon's diversity index
treeinv_H <- as.data.frame(vegan::diversity(treeinv_spshort)) %>%
  rename("shannonH" = "vegan::diversity(treeinv_spshort)") %>%
  mutate(shannonH = ifelse(shannonH == 0, NA, shannonH))
treeinv_H$TRACTCE <- rownames(treeinv_H)
rownames(treeinv_H) <- NULL

# Simpson's diversity index
treeinv_simpson <- as.data.frame(vegan::diversity(treeinv_spshort, 'simpson')) %>%
  rename("simpson" = "vegan::diversity(treeinv_spshort, \"simpson\")") %>%
  mutate(simpson = ifelse(simpson == 0, NA, simpson))
treeinv_simpson$TRACTCE <- rownames(treeinv_simpson)
rownames(treeinv_simpson) <- NULL
treeinv_simpson$inverse_simpson <- 1/treeinv_simpson$simpson

# species richness
treeinv_spshort[treeinv_spshort > 0] <- 1
treeinv_richness <- apply(treeinv_spshort, 1, sum)
treeinv_richness <- as.data.frame(treeinv_richness)
treeinv_richness$TRACTCE <- rownames(treeinv_richness)
rownames(treeinv_richness) <- NULL

# phylogenetic diversity
pd <- read.csv(file = "R/phylogenetic_diversity_msp.csv")
pd$TRACTCE <- sub("'", "", pd$TRACTCE)
pd <- pd %>% filter(!is.na(mpd))

# join together diversity indices
tree_div <- full_join(treeinv_H, treeinv_simpson, by = "TRACTCE")
tree_div <- full_join(tree_div, treeinv_richness, by = "TRACTCE")
tree_div <- inner_join(tree_div, pd, by = "TRACTCE")

# filter out missing richness, shannon H rows
tree_div <- tree_div %>% filter(!is.na(treeinv_richness)) %>%
  filter(!is.na(shannonH))

# join diversity indices to census tract
tree_div_censustract <- left_join(MSP_censustract, tree_div, by = "TRACTCE")

### create Genus-level matrix for each census tract from tree inventory data
# first, select Genus column (geometry sticks)
treeinv_genus <- treeinv[ , "genus"] # creates slim df with genus and geometry only

# join to tigris census geometry
treeinv_genus_censustrack <- st_join(treeinv_genus, MSP_censustract)

# create genus-level matrix
treeinv_genuslong <- as.data.frame(treeinv_genus_censustrack[ , c("TRACTCE", "genus")])
treeinv_genuslong <- treeinv_genuslong %>% filter(!is.na(genus))
treeinv_genusshort <- fuzzySim::splist2presabs(treeinv_genuslong,
                                               sites.col = "TRACTCE", sp.col = "genus", keep.n = T)
treeinv_genusshort[,-1] <- lapply(treeinv_genusshort[, -1], function(x) as.numeric(x))
row.names(treeinv_genusshort) <- treeinv_genusshort$TRACTCE
treeinv_genusshort[1] <- NULL

# calculate % of specific genera that may be particularly vulnerable
treeinv_pctgenus <- treeinv_genusshort
treeinv_pctgenus$n_trees <- rowSums(treeinv_pctgenus[ ,1:length(colnames(treeinv_pctgenus))])
# Fraxinus (ash)
treeinv_pctgenus$pct_Fraxinus <- if("Fraxinus" %in% names(treeinv_pctgenus)) {
  treeinv_pctgenus$pct_Fraxinus <- treeinv_pctgenus$Fraxinus/treeinv_pctgenus$n_trees*100
} else {
  treeinv_pctgenus$pct_Fraxinus <- 0
}

# convert rownames to column for joining back to main spatial dataframe
treeinv_pctgenus$TRACTCE <- rownames(treeinv_pctgenus)
rownames(treeinv_pctgenus) <- NULL
treeinv_Fraxinus <- treeinv_pctgenus %>% dplyr::select(c(TRACTCE, pct_Fraxinus))

### join calculated indices to census geometry - end result is treeinv_spdiv which
#  is a spatial df with species-level diversity indices and % Fraxinus by census tract
#  across 7-county metro area for full tree inventory
treeinv_spdiv <- left_join(MSP_censustract, treeinv_H, by = "TRACTCE")
treeinv_spdiv <- left_join(treeinv_spdiv, treeinv_simpson, by = "TRACTCE")
treeinv_spdiv <- left_join(treeinv_spdiv, treeinv_richness, by = "TRACTCE")
treeinv_spdiv$evenness <- treeinv_spdiv$shannonH/log(treeinv_spdiv$treeinv_richness) # calculate evenness
treeinv_spdiv <- left_join(treeinv_spdiv, treeinv_Fraxinus, by = "TRACTCE")
treeinv_spdiv <- left_join(treeinv_spdiv, pd, by = "TRACTCE")
