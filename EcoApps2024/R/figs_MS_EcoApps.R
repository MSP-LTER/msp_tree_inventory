# This script makes figures for tree inventory MS
# author: Adrienne Keller
# dependencies: 00_build_MSPtreeinventory
################################################################################

# source dependencies
#source(file = "R_analyses/00_build_MSPtreeinventory.R")

### Figure 2 - map of tree data points with dataset A. Four panels: all data and then subset by landcover group ----
lakes <- st_read(dsn = "R/urban_lakes/urban_lakes_sub.shp")
#MN state fig for spatial context
counties <- tigris::counties(state = "MN")
MN <- counties %>% st_union()
aoi_counties_union <- aoi_counties %>% st_union()
fig_2_MN <- tm_shape(counties) + tm_polygons() + tm_shape(MN) + tm_borders(lwd = 2, col = "black") +
  tm_shape(aoi_counties_union) + tm_borders(lwd = 4, col = "blue") +
  tm_layout(frame = F)
#tmap_save(fig_2_MN, file = "Output/fig2_MN.png")

#fig2a
tree_map_all <- tm_shape(msp_tracts) + tm_fill(col = "white") + tm_borders(col = "darkgrey", lwd = 0.5) +
  tm_shape(aoi_counties) + tm_fill(alpha = 0) + tm_borders(col = "darkgrey", lwd = 1.5) +
  tm_shape(aoi_metro) + tm_borders("black", lwd = 2) +
  tm_shape(tree_A) + tm_dots(col = "#228833", shape = 16, size = 0.001) +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(lakes) + tm_fill(col = "grey") +
  tm_layout(frame = F)
#fig2b
tree_A_street <- tree_A %>% filter(land_cover_groups == "street")
tree_map_street <- tm_shape(msp_tracts) + tm_fill(col = "white") + tm_borders(col = "darkgrey", lwd = 0.5) +
  tm_shape(aoi_counties) + tm_fill(alpha = 0) + tm_borders(col = "darkgrey", lwd = 1.5) +
  tm_shape(aoi_metro) + tm_borders("black", lwd = 2) +
  tm_shape(tree_A_street) + tm_dots(col = "#228833", shape = 16, size = 0.01) +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(lakes) + tm_fill(col = "grey") +
  tm_layout(frame = F)
#fig2c
tree_A_park <- tree_A %>% filter(land_cover_groups == "rec_parkland")
tree_map_park <- tm_shape(msp_tracts) + tm_fill(col = "white") + tm_borders(col = "darkgrey", lwd = 0.5) +
  tm_shape(aoi_counties) + tm_fill(alpha = 0) + tm_borders(col = "darkgrey", lwd = 1.5) +
  tm_shape(aoi_metro) + tm_borders("black", lwd = 2) +
  tm_shape(tree_A_park) + tm_dots(col = "#228833", shape = 16, size = 0.01) +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(lakes) + tm_fill(col = "grey") +
  tm_layout(frame = F)
#fig2d
tree_A_other <- tree_A %>% filter(land_cover_groups == "other")
tree_map_other <- tm_shape(msp_tracts) + tm_fill(col = "white") + tm_borders(col = "darkgrey", lwd = 0.5) +
  tm_shape(aoi_counties) + tm_fill(alpha = 0) + tm_borders(col = "darkgrey", lwd = 1.5) +
  tm_shape(aoi_metro) + tm_borders("black", lwd = 2) +
  tm_shape(tree_A_other) + tm_dots(col = "#228833", shape = 16, size = 0.01) +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(lakes) + tm_fill(col = "grey") +
  tm_layout(frame = F)
#save individually (patchwork doesn't work with tmap)
#tmap_save(tree_map_all, "Output/fig2a.png")
#tmap_save(tree_map_street, "Output/fig2b.png")
#tmap_save(tree_map_park, "Output/fig2c.png")
#tmap_save(tree_map_other, "Output/fig2d.png")

### Figure 3 - top 10 genera barplot with dataset A. (barplots by landcover groups for supplementary) ----
dat <- tree_A %>% group_by(genus) %>% st_drop_geometry() %>%
  summarise(n_genus = length(genus))  %>%
  mutate(n_total = sum(n_genus),
         n_pct = n_genus/n_total*100)
dat <- dat %>% arrange(desc(n_genus))
dat <- dat[1:10, ]
options(scipen=100000)
fig3 <- ggplot(dat, aes(x = reorder(genus, -n_genus), y = n_genus)) + geom_bar(stat = "identity", fill = "darkgrey") +
  ylab("# Individual Trees Per Genus") +
  xlab("Genus") +
  theme_basic +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22),
        panel.border = element_rect(colour = "NA")) +
  scale_y_continuous(breaks = seq(0, 200000, by = 25000), labels = scales::label_comma())
fig3
#ggsave(fig3, file = "Output/fig3.png", height = 7, width = 14)

### Fig 4 - species diversity maps with dataset B, df = treeinv_spdiv output from 10_calc-species-diversity ----
fig4a <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(treeinv_spdiv) +
  tm_fill("treeinv_richness", breaks = c(1,25,50,75,100,200), title = "Species richness         ",
          labels = c("5 to 25", "26 to 50", "51 to 75", "76 to 100", "> 100"),
          textNA = "No data available", colorNA = NULL,
          palette = c("#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C")) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.title.size = 2, legend.text.size = 1.2, legend.position = c(0,0.75))

fig4b <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(treeinv_spdiv) +
  tm_fill("shannonH", title = "Shannon's Diversity Index",
          labels = c("< 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5"),
          textNA = "No data available", colorNA = NULL,
          palette = c("#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C"),
          drop.levels = F) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.title.size = 2, legend.text.size = 1.2, legend.position = c(0,0.75))
#tmap_save(fig4a, file = "Output/fig4a.png")
#tmap_save(fig4b, file = "Output/fig4b.png")

dat_use <- treeinv_spdiv %>% filter(!is.na(shannonH))
fig4c <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat_use) +
  tm_fill("mpd", breaks = c(0,200,300,400,500,1000), title = "Mean phylogenetic distance",
          labels = c("<= 200", "201 to 300", "301 to 400", "401 to 500", "> 500"),
          textNA = "No data available", colorNA = NULL,
          palette = c("#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C"),
          drop.levels = F) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.title.size = 2, legend.text.size = 1.2, legend.position = c(0,0.75))
#tmap_save(fig4a, file = "Output/fig4a.png")
#tmap_save(fig4b, file = "Output/fig4b.png")
#tmap_save(fig4c, file = "Output/fig4c.png")

# histogram of diversity distributions across MSP
fig4d <- ggplot(treeinv_spdiv, aes(x = treeinv_richness)) +
  geom_histogram(binwidth = 10, fill = "darkgrey") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  xlab("Species richness") +
  ylab("Count ") +
  xlim(0,200)
fig4e <- ggplot(treeinv_spdiv, aes(x = shannonH)) +
  geom_histogram(binwidth = 0.5, fill = "darkgrey") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  xlab("Shannon's Diversity Index") +
  ylab("Count ") +
  xlim(0, 5)
fig4f <- ggplot(dat_use, aes(x = mpd)) +
  geom_histogram(binwidth = 20, fill = "darkgrey") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16)) +
  xlab("Mean phylogenetic distance") +
  ylab("Count ")
#ggsave(fig4c, file = "Output/fig4d.png", height = 4, width = 5)
#ggsave(fig4d, file = "Output/fig4e.png", height = 4, width = 5) # fig 4 needs to be combined in powerpoint
#ggsave(fig4f, file = "Output/fig4f.png", height = 4, width = 5)

### Fig 5 -----
# low emissions scenario - high vuln
dat <- tree_C_tracts
fig_5a <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat) + tm_fill("lowemissions_highvuln" , title = "% Highly Vulnerable Trees",
                          colorNA = NULL,
                          breaks = c(0,10,20,30,40,50,100),
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-100%")) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
fig_5a
#tmap_save(fig_5a, file = "Output/Fig5a.png")

# high emissions scenario - high vuln - more breaks
fig_5b<- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat) + tm_fill("highemissions_highvuln" , title = "% Highly Vulnerable Trees",
                          colorNA = NULL,
                          breaks = c(0,10, 20, 30, 40, 50, 100),
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-100%")) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
fig_5b
#tmap_save(fig_5b, file = "Output/fig_5b.png")

### Figure 6 - SVI-CVI overlap
# SVI map
tree_C_tracts$highemissions_modhighvuln <- tree_C_tracts$highemissions_highvuln + tree_C_tracts$highemissions_modvuln
datasetC_aoi <- tree_C_tracts %>% filter(!is.na(highemissions_modhighvuln))
dat_over50 <- tree_C_tracts %>% filter(RPL_THEMES > 0.5) %>% filter(highemissions_modhighvuln > 50)
fig_6a <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(SVI) + tm_fill("RPL_THEMES" , title = "Social Vulnerability Index",
                          colorNA = NULL,
                          palette = c("#ffffd4", "#fed98e", "#FE9929","#D95F0E"),
                          breaks = c(0, .25, .5, .75, 1)) +
#  tm_shape(datasetC_aoi) + tm_borders(col = "lightgrey") +
  #tm_shape(dat_over50) + tm_borders(col = "black") +
  #tm_shape(dat_under50) + tm_borders(col = "lightgrey") +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
fig_6a
#tmap_save(file = "Output/fig_6a.png", fig_6a)

fig_6b <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(datasetC_aoi) + tm_fill("grey") +
  tm_shape(dat_over50) + tm_fill("red" ,
                          colorNA = NULL) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_add_legend(type = "fill", labels = c("High social & tree vuln.", "Not high vulnerability", "No data available       "),
                col = c("red", "grey", "white")) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
fig_6b
#tmap_save(file = "Output/fig_6b.png", fig_6b)

### Fig SI 1 - code not available (from Jeannine Cavendar-Bares)

### Fig SI 2 - spatial patterns
# SI2a
dat_tract <- treeinv_spdiv %>% filter(!is.na(treeinv_richness)) %>% filter(!is.na(shannonH))
coords <- st_centroid(st_geometry(dat_tract)) # centroids of tracts
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
# Local spatial autocorrelation - Getis-ord
localg <-localG(dat_tract$treeinv_richness,  invd_weights) # returns object with Z-scores for the Gi stat.
#Large positive z-scores = hot spots (cluster of high diversity), large negative z-socres = cold spots (cluster of low diversity)
dat_tract <- dat_tract %>%
  mutate(localg = as.numeric(localg))
breaks <- c(min(dat_tract$localg), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, max(dat_tract$localg)) # set z-score cut-offs at different significances
#png(file = "Output/SIFig2a.png")
tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat_tract) +
  tm_polygons(col = "localg", title = "Gi value", palette = "-RdBu",
              breaks = breaks) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_layout(frame = F)
dev.off()

# SI 2b
dat_tract <- treeinv_spdiv %>% filter(!is.na(shannonH))
coords <- st_centroid(st_geometry(dat_tract)) # centroids of tracts
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
# Local spatial autocorrelation - Getis-ord
localg <-localG(dat_tract$shannonH,  invd_weights) # returns object with Z-scores for the Gi stat.
#Large positive z-scores = hot spots (cluster of high diversity), large negative z-socres = cold spots (cluster of low diversity)
dat_tract <- dat_tract %>%
  mutate(localg = as.numeric(localg))
breaks <- c(min(dat_tract$localg), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, max(dat_tract$localg)) # set z-score cut-offs at different significances
#png(file = "Output/SIFig2b.png")
tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat_tract) +
  tm_polygons(col = "localg", title = "Gi value", palette = "-RdBu",
              breaks = breaks) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_layout(frame = F)
dev.off()

# SI 2c
dat_tract <- treeinv_spdiv %>% filter(!is.na(mpd))
coords <- st_centroid(st_geometry(dat_tract)) # centroids of tracts
k1 <- knn2nb(knearneigh(coords))
crit_threshold <- max(unlist(nbdists(k1, coords)))
nb_dist_band <- dnearneigh(coords, 0, crit_threshold)
distances <- nbdists(nb_dist_band, coords)
invd <- lapply(distances, function(x) (1/(x/1000))) # calculates inverse with rescaled distances
invd_weights <- nb2listw(nb_dist_band, glist = invd, style = "B")
# Local spatial autocorrelation - Getis-ord
localg <-localG(dat_tract$mpd,  invd_weights) # returns object with Z-scores for the Gi stat.
#Large positive z-scores = hot spots (cluster of high diversity), large negative z-socres = cold spots (cluster of low diversity)
dat_tract <- dat_tract %>%
  mutate(localg = as.numeric(localg))
breaks <- c(min(dat_tract$localg), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, max(dat_tract$localg)) # set z-score cut-offs at different significances
#png(file = "Output/SIFig2c.png")
tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat_tract) +
  tm_polygons(col = "localg", title = "Gi value", palette = "-RdBu",
              breaks = breaks) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#c2a5cf", lwd = 3.5) +
  tm_layout(frame = F)
dev.off()

### SI Figure 3
SIfig3a <- ggplot(tree_C_tracts, aes(x = highemissions_highvuln)) +
  geom_histogram(binwidth = 10, colour = "white", fill = "darkred") +
  xlab("% trees in census tract of high vulnerability") +
  ylab("Count") +
  xlim(0,105) +
  theme_basic +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

SIfig3b <- ggplot(tree_C_tracts, aes(x = highemissions_modvuln)) +
  geom_histogram(binwidth = 10, colour = "white", fill = "darkorange") +
  xlab("% trees in census tract of moderate vulnerability") +
  ylab("Count") +
  xlim(0,105) +
  theme_basic +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

SIfig3c <- ggplot(tree_C_tracts, aes(x = highemissions_lowvuln)) +
  geom_histogram(binwidth = 10, colour = "white", fill = "darkgreen") +
  xlab("% trees in census tract of low vulnerability") +
  ylab("Count") +
  xlim(0,105) +
  theme_basic +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))
#ggsave(SIfig3a, file = "Output/SIfig3a.png")
#ggsave(SIfig3b, file = "Output/SIfig3b.png")
#ggsave(SIfig3c, file = "Output/SIfig3c.png")

### SI Figure 4
# set data for NATIVES
native_df <- read.csv(file = "R/native-status_tree-inventory-datasetC.csv")
native_df <- native_df[ , 1:2]
tree_layers <- inner_join(tree_B, native_df, by = "genus_epi")
tree_layers <- tree_layers %>% filter(northamer_status == "native")

# source CVI calculations and join to make tracts sf
source(file = "R/11_CVI-analysis_EcoApps.R")
dat <- dplyr::left_join(treeinv_spdiv, treeinv_CVI_df, by = "TRACTCE")

# make native fig
SIfig4a <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat) + tm_fill("highemissions_highvuln" , title = "% Highly Vulnerable \nNative Trees",
                          colorNA = NULL,
                          breaks = c(0,10, 20, 30, 40, 50, 100),
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-100%")) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
SIfig4a
#tmap_save(SIfig4a, file = "Output/SIfig4a.png")

# set data for NON-NATIVES
tree_layers <- inner_join(tree_B, native_df, by = "genus_epi")
tree_layers <- tree_layers %>% filter(northamer_status == "non-native")

# source CVI calculations and join to make tracts sf
source(file = "R/11_CVI-analysis_EcoApps.R")
dat <- dplyr::left_join(treeinv_spdiv, treeinv_CVI_df, by = "TRACTCE")

# make native fig
SIfig4b <- tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black", alpha = 0) +
  tm_shape(dat) + tm_fill("highemissions_highvuln" , title = "% Highly Vulnerable \n Non-Native Trees",
                          colorNA = NULL,
                          breaks = c(0,10, 20, 30, 40, 50, 100),
                          labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-100%")) +
  tm_shape(aoi_counties) + tm_fill("CO_NAME", alpha = 0, legend.show = F) +
  tm_borders(col = "grey", lwd = .5) +
  tm_shape(aoi_metro) + tm_borders(lwd = 1.5, col = "black") +
  tm_shape(minneapolis_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_shape(stpaul_bound) + tm_borders(col = "#af8dc3", lwd = 3.5) +
  tm_layout(frame = F, legend.text.size = 1.2, legend.title.size = 1.5)
SIfig4b
#tmap_save(SIfig4b, file = "Output/SIfig4b.png")
