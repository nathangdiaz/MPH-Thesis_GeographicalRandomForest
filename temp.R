#### PREPARATION FOR ANALYSIS

# setting seed
set.seed(926) 
# loading data 
svi_df = read_csv(here::here("01_Data", "svi_df.csv")) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-...1) # if needed
# obtaining SPH files for RI tracts
tracts = tracts(state = "RI", year = 2022, cb = TRUE)
# joining data 
svi_df = inner_join(tracts, svi_df, by = c("GEOID" = "fips"))
# defining analytical coordinates and df - analytical refer to those variables that will be used in the analysis
df_coords = svi_df %>% 
  mutate(
    # redefines geometry to be the centroid of the polygon
    geometry = st_centroid(geometry),
    # pulls the lon and lat for the centroid
    lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
    lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>% 
  # removes geometry, coerce to data.frame
  st_drop_geometry() %>%
  # only select the lon and lat 
  select(lon, lat)
# only obtain response and predictor variables 
df = svi_df %>% 
  st_drop_geometry() %>% 
  select(rpl_themes, starts_with("e_"))
# Create the spatial weights object using Queen's contiguity
neighbors = spdep::poly2nb(tracts, queen = TRUE)
listw = nb2listw(neighbors, style="W", zero.policy=TRUE)

#### DEFINING SPATIAL CLUSTERS
# use elbow method for defining clusters - uses kmean algorithm 
factoextra::fviz_nbclust(as.data.frame(df$rpl_themes), kmeans, method = "wss")
cluster_num = 4 # based on the graph
# define spatial weights using the queen continuity
guerry = rgeoda::sf_to_geoda(tracts, with_table = TRUE)
queen_weights = queen_weights(tracts)
# apply the SKATER fulction to define clusters - this can also be done with kmeans and hierchical clustering 
clusters = spdep::skater(4, listw, as.data.frame(df$rpl_themes))
re# add cluster to spatial data 
df$cluster_id = clusters$cluster
# visualize clusters
ggplot(data = df, aes(fill = as.factor(cluster)), color = "white") + 
  geom_sf() + 
  scale_fill_viridis_d() + 
  labs(title = "Spatial Cluster with SKATER", fill = "Cluster") +
  theme_void()