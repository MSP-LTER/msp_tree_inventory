# This script calculates statistics for tree inventory MS
# author: Adrienne Keller
# dependencies: 00_build_MSPtreeinventory
################################################################################

# Demographic etc. data for methods section
a <- tidycensus::load_variables(year = 2020, dataset = "acs5/subject")

# population and pop density
pop <- get_acs(geography = "tract",
               survey = "acs5",
               state = "MN",
               county = c("Anoka", "Dakota", "Washington", "Ramsey", "Hennepin", "Carver", "Scott"),
               variables = "S0101_C01_001")
MSP_censustract_area <- MSP_censustract %>% as.data.frame() %>% dplyr::select(c(GEOID, ALAND, AWATER))
pop_area <- left_join(pop, MSP_censustract_area, by = "GEOID") %>%
  rowwise() %>%
  mutate(area = ALAND/1000000) %>%
  mutate(pop_density = estimate/area)
summary(pop_area$pop_density) # pop density = 0 is airport
area_total <- MSP_censustract_area %>% rowwise() %>% mutate(total = sum(ALAND, AWATER)/1000000) %>% as.data.frame()
area_total %>% summarise(total = sum(total))

# medhinc
medhinc <- get_acs(
  geography = "tract",
  table = "B19013", # median household income
  survey = "acs5",
  year = 2020,
  state = "MN",
  county = c("Anoka", "Dakota", "Washington", "Ramsey", "Hennepin", "Carver", "Scott"),
  cache_table = T,
)

summary(medhinc$estimate)

# owner occupied
own_occupied <- get_acs(geography = "tract",
                        survey = "acs5",
                        yaer = 2020,
                        state = "MN",
                        county = c("Anoka", "Dakota", "Washington", "Ramsey", "Hennepin", "Carver", "Scott"),
                        table = "B25003") %>%
  dplyr::filter(variable == "B25003_001" | variable == "B25003_002") %>%
  dplyr::select(-"moe") %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(pct_owned = B25003_002/B25003_001*100)
summary(own_occupied$pct_owned)

### Subsection: Tree diversity patterns
nrow(tree_A) # 639368 trees in full database
length(unique(unlist(tree_A$CTU_ID))) # 110 CTUs
nrow(tree_A_minnstpaul <- tree_A[minnstpaul, ])/nrow(tree_A)*100 # 54% of trees were located in Minneapolis/St. Paul
tree_A %>% st_drop_geometry() %>% # % of trees that fall into each land cover type
  mutate(n_total = length(land_cover)) %>%
  group_by(land_cover, n_total) %>% summarize(n_land_cover = length(land_cover)) %>%
  mutate(prop_land_cover = n_land_cover/n_total*100) %>%
  mutate(prop_land_cover = round(prop_land_cover, 2))

### Subsection: Tree species composition and diversity
nrow(tree_A %>% st_drop_geometry() %>% group_by(genus) %>% summarise(n = length(genus))) # 101 genera in dataset A
nrow(tree_A %>% st_drop_geometry() %>% group_by(genus_epi) %>% summarise(n = length(genus_epi))) # 449 species in dataset A
tree_A %>% st_drop_geometry() %>% mutate(total_trees = length(genus)) %>% # proportion of trees in top genera
  group_by(genus, total_trees) %>% summarise(n = length(genus)) %>%
  mutate(prop_genus = n/total_trees *100) %>%
  arrange(desc(n))
tree_A %>% st_drop_geometry() %>% filter(genus == "Acer") %>% # proportion of Acer trees by species
  mutate(total_Acer = length(genus_epi)) %>%
  group_by(genus_epi, total_Acer) %>% summarise(n = length(genus)) %>%
  mutate(prop_species = n/total_Acer *100) %>%
  arrange(desc(n))

hist(treeinv_spdiv$treeinv_richness) # right skewed richness histogram
hist(treeinv_spdiv$shannonH) # left shewed Shannon's H histogram
hist(treeinv_spdiv$mpd)

## spatial autocorrelation analysis for database B
# notes: good resources: https://crd230.github.io/lab7.html#Neighbor_weights
#  https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#inverse-distance-weights
# set df
dat_tract <- treeinv_spdiv
dat_tract <- dat_tract %>% filter(!is.na(treeinv_richness)) %>% filter(!is.na(shannonH))

# Spatial weights matrix: 1) who is your neighbor 2) weights: how much does your neighbor matter?
# Distance-based neighbors- nb object
coords <- st_centroid(st_geometry(dat_tract)) # centroids of tracts
# Create weights - inverse distance decay
#  first need to determine upper and lower distance bounds
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
#  now calculate distance-band neighbors
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
#  now calculate inverse distance btwn all neighbors
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
length(invd) # should match number of polygons
invd[1] # check to see if range of distances are "ok" - don't want them too small/very close to zero (this would be due to very large distance prior to inverse --> in which case, rescale)
#  now calculate inverse distance decay weights
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
summary(invd_weights)

# Moran Scatterplot
#png(file = "Output/20220620_moran_richness_tract.png")
moran.plot(dat_tract$treeinv_richness, listw=invd_weights)
#dev.off()
#png(file = "Output/20220620_moran_shannon_tract.png")
moran.plot(dat_tract$shannonH, listw=invd_weights)
#dev.off()

# Global spatial autocorrelation - Moran's I (can also look at Geary's c, a cousin to Moran's I)
moran.test(dat_tract$treeinv_richness, invd_weights) # Moran's I is a correlation, typically +/-0.3 or more is meaningful
moran.mc(dat_tract$treeinv_richness, invd_weights, nsim=999) # compute p-value using monte carlo simulation
moran.test(dat_tract$shannonH, invd_weights)
moran.mc(dat_tract$shannonH, invd_weights, nsim = 999)
moran.test(dat_tract$mpd, invd_weights)
moran.mc(dat_tract$mpd, invd_weights, nsim=999)

## stats across MSP and comparing urban to sub- and ex-urban areas
dat <- treeinv_spdiv %>% filter(!is.na(treeinv_richness)); dat <- dat %>% filter(!is.na(shannonH))
urban_sf <- dat[minnstpaul, ]
urban_df <- urban_sf %>% st_drop_geometry()
exurban_sf <- dat[exurban, ]
exurban_df <- exurban_sf %>% st_drop_geometry()
minneapolis_df <- dat[minneapolis_bound, ]
stpaul_df <- dat[stpaul_bound, ]

# richness
dat <- treeinv_spdiv %>% filter(!is.na(treeinv_richness)); dat <- dat %>% filter(!is.na(shannonH))
mean(dat$treeinv_richness); sd(dat$treeinv_richness) #richness across MSP
mean(urban_df$treeinv_richness); sd(urban_df$treeinv_richness)#richness in Minn + St. Paul
mean(exurban_df$treeinv_richness); sd(exurban_df$treeinv_richness) # richness outside Minneapolis + St. Paul
t.test(urban_df$treeinv_richness, exurban_df$treeinv_richness) # urban richness > exurban richness

#Shannon's diversity
mean(dat$shannonH); sd(dat$shannonH) #shannonH across MSP
mean(urban_df$shannonH); sd(urban_df$shannonH) #shannonH in Minn + St. Paul
mean(exurban_df$shannonH); sd(exurban_df$shannonH) # shannonH outside Minneapolis + St. Paul
t.test(urban_df$shannonH, exurban_df$shannonH) # urban shannonH > exurban shannonH

# MPD phylogenetic diversity
summary(dat$mpd); sd(dat$mpd)
t.test(urban_df$mpd, exurban_df$mpd)
mean(minneapolis_df$mpd); sd(minneapolis_df$mpd)
mean(stpaul_df$mpd); sd(stpaul_df$mpd)
t.test(minneapolis_df$mpd, stpaul_df$mpd)

# SI Table 1 PSE and PSV phylogenetic diversity
summary(dat$PSEs); sd(dat$PSEs)
summary(dat$PSVs); sd(dat$PSVs)

## Subsection: Urban forest vulnerability to climate change
# under low emissions
mean(tree_C_tracts$lowemissions_highvuln, na.rm = T) # Under a low emissions scenario, the mean proportion of trees with high vulnerability within a census tract was 2.5%.

tracts_lowemission_highvuln <- tree_C_tracts %>% filter(lowemissions_highvuln >=20)
n_tracts_total <- nrow(tree_C_tracts %>% filter(!is.na(lowemissions_highvuln)))
nrow(tracts_lowemission_highvuln)/n_tracts_total*100 # 2.6% of tracts with low emissions data have more than 20% highly vulnerable trees

aoi_coonrapids <- CoCTU %>% filter(CTU_NAME == "Coon Rapids") %>% st_union()
tree_C_low[aoi_coonrapids, ] %>% st_drop_geometry() %>%
  group_by(genus_epi) %>%
  summarise(n_genus_epi = length(genus_epi)) %>%
  mutate(n_total = sum(n_genus_epi)) %>%
  mutate(prop_genus_epi = n_genus_epi/n_total*100) %>%
  arrange(desc(prop_genus_epi)) # Fraxinus excelsior makes up ~30% of inventory in Coon Rapids area

# under high emissions
mean(tree_C_tracts$highemissions_highvuln, na.rm = T)

tracts_highemissions_highvuln <- tree_C_tracts %>% filter(highemissions_highvuln >=20)
nrow(tracts_highemissions_highvuln)/n_tracts_total*100

# tree species that shift vulnerability between low and high emissions
tree_C_low_topspecies <- tree_C_low %>% st_drop_geometry() %>%
  group_by(genus_epi) %>%
  summarise(n_genus_epi = length(genus_epi)) %>%
  mutate(n_total = sum(n_genus_epi)) %>%
  mutate(prop_genus_epi = n_genus_epi/n_total*100) %>%
  arrange(desc(prop_genus_epi))

# total number of species in each vulnerability category
tree_C_low %>% st_drop_geometry() %>% distinct(genus_epi, .keep_all = T) %>%
  group_by(vuln_low_emissions) %>%
  summarise(n_spp = length(genus_epi)) %>%
  mutate(n_total = sum(n_spp),
         pct = n_spp/n_total*100)

tree_C_high %>% st_drop_geometry() %>% distinct(genus_epi, .keep_all = T) %>%
  group_by(vuln_high_emissions) %>%
  summarise(n_spp = length(genus_epi)) %>%
  mutate(n_total = sum(n_spp),
         pct = n_spp/n_total*100)

# table of top 20 species in tree_C with vulnerability in low and high emissions scenario
vuln_tree_C_low <- tree_C_low %>% st_drop_geometry() %>% dplyr::select(c(genus_epi, vuln_low_emissions))
vuln_tree_C_high <- tree_C_high %>% st_drop_geometry() %>% dplyr::select(c(genus_epi, vuln_high_emissions))
topspecies_lowvuln <- left_join(tree_C_low_topspecies, vuln_tree_C_low) %>% distinct(.)
topspecies_lowhighvuln <- left_join(topspecies_lowvuln, vuln_tree_C_high) %>% distinct(.)
#write.csv(file = "Output/species_vuln_table_top20.csv", topspecies_lowhighvuln)

# under high emissions, is vulnerability spatially correlated?
# set data
dat_tract <- treeinv_CVI_sf %>% filter(!is.na(highemissions_highvuln))
# first just look at map of high vulnerability census tracts
dat <- dat_tract %>% filter(highemissions_highvuln >= 20)
tm_shape(MSP_censustract) + tm_polygons() +
  tm_shape(tree_C_high) + tm_fill("grey") +
  tm_shape(dat) + tm_fill("red")
# Spatial weights matrix: 1) who is your neighbor 2) weights: how much does your neighbor matter?
# Distance-based neighbors- nb object
coords <- st_centroid(st_geometry(dat_tract)) # centroids of tracts
# Create weights - inverse distance decay
#  first need to determine upper and lower distance bounds
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
#  now calculate distance-band neighbors
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
#  now calculate inverse distance btwn all neighbors
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
length(invd) # should match number of polygons
invd[1] # check to see if range of distances are "ok" - don't want them too small/very close to zero (this would be due to very large distance prior to inverse --> in which case, rescale)
#  now calculate inverse distance decay weights
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
summary(invd_weights)
# Moran scatterplot
moran.plot(dat_tract$highemissions_highvuln, listw=invd_weights) # suggests inverse spatial relationship
# Global spatial autocorrelation - Moran's I (can also look at Geary's c, a cousin to Moran's I)
moran.test(dat_tract$highemissions_highvuln, invd_weights, alternative = "greater") # Moran's I is a correlation, typically +/-0.3 or more is meaningful
moran.mc(dat_tract$highemissions_highvuln, invd_weights, nsim=10000, alternative = "two.sided") # compute p-value using monte carlo simulation

moran.range <- function(lw){
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(invd_weights)

### Relationship between urban forest biodiversity and vulnerability to climate change
## is there a relationship between Shannon H index and % trees moderately or highly vulnerable in a given census tract? !?! This section needs to be revised/finished
tree_C_tracts$highemissions_modhighvuln <- tree_C_tracts$highemissions_highvuln + tree_C_tracts$highemissions_modvuln
dat <- tree_C_tracts %>% filter(!is.na(highemissions_modhighvuln)) %>%
  filter(!is.na(shannonH))
tm_shape(dat) + tm_polygons("highemissions_modhighvuln")
ggplot(dat, aes(x = shannonH, y = highemissions_modhighvuln)) +
  geom_point() +
  geom_smooth(method = "lm")
# fit non spatial lm model
mod <- lm(dat$highemissions_modhighvuln ~ dat$shannonH)
# plot residuals from lm model
dat <- dat %>% mutate(olsresid = resid(mod))
tm_shape(dat) + tm_polygons(col = "olsresid", style = "quantile", palette = "Reds")

# spatial autocorrelation
datb <- poly2nb(dat, queen = T)

# Distance-based neighbors- nb object
coords <- st_centroid(st_geometry(dat)) # centroids of tracts
# Create weights - inverse distance decay
#  first need to determine upper and lower distance bounds
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
#  now calculate distance-band neighbors
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
#  now calculate inverse distance btwn all neighbors
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
length(invd) # should match number of polygons
invd[1] # check to see if range of distances are "ok" - don't want them too small/very close to zero (this would be due to very large distance prior to inverse --> in which case, rescale)
#  now calculate inverse distance decay weights
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
fit.lag <- spatialreg::lagsarlm(highemissions_modhighvuln ~ shannonH, data = dat, listw = invd_weights)
summary(fit.lag)

### plot spatial hotspots of low biodiversity and high vulnerability to climate change
tree_C_tracts <- tree_C_tracts %>% mutate(shannonH_quantiles = ntile(shannonH, 2),
                                          highemissions_highvuln_quantiles = ntile(highemissions_highvuln, 2),
                                          highemissions_modvuln_quantiles = ntile(highemissions_modvuln, 2))
table(tree_C_tracts$shannonH_quantiles)
dat_high <- tree_C_tracts %>% filter(shannonH_quantiles == 1) %>% filter(highemissions_highvuln_quantiles == 2)
dat_mod <- tree_C_tracts %>% filter(shannonH_quantiles == 1) %>% filter(highemissions_modvuln_quantiles == ntile(highemissions_modvuln, 2))

tm_shape(CoCTU) + tm_fill(col = "grey") +
  tm_shape(dat_high) + tm_fill(col = "red", legend.show = F) +
  tm_shape(dat_mod) + tm_fill(col = "orange", legend.show = F)

## SVI spatially autocorrelated?
dat <- tree_C_tracts %>% filter(!is.na(RPL_THEMES))
coords <- st_centroid(st_geometry(dat)) # centroids of tracts
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
length(invd) # should match number of polygons
invd[1] # check to see if range of distances are "ok" - don't want them too small/very close to zero (this would be due to very large distance prior to inverse --> in which case, rescale)
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
summary(invd_weights)
moran.plot(dat$RPL_THEMES, listw=invd_weights)
moran.test(dat$RPL_THEMES, invd_weights) # Moran's I is a correlation, typically +/-0.3 or more is meaningful
moran.mc(dat$RPL_THEMES, invd_weights, nsim=999) # compute p-value using monte carlo simulation

# non-spatial relationship between SVI and climate vulnerability?
summary(lm(tree_C_tracts$highemissions_highvuln ~ tree_C_tracts$RPL_THEMES))
tree_C_tracts$highemissions_modhighvuln <- tree_C_tracts$highemissions_highvuln + tree_C_tracts$highemissions_modvuln
dat <- tree_C_tracts %>% filter(RPL_THEMES > 0.5) %>% filter(highemissions_modhighvuln > 50)
tm_shape(CoCTU) + tm_polygons() + tm_shape(dat) + tm_fill("red", legend.show = F)

nrow(dat)/nrow(tree_C_tracts) #23.3% of census tracts we have data for are both socially and climate vulnerable

## relationship between biodiversity and SVI?
dat <- tree_C_tracts %>% filter(!is.na(highemissions_modhighvuln))
summary(lm(dat$RPL_THEMES ~ dat$shannonH))
ggplot(dat, aes(x = shannonH, y = RPL_THEMES)) + geom_point() +
  geom_smooth()

