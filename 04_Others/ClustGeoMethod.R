#### PREPARATION FOR ANALYSIS
library(tidyverse)    # general data manipulation
library(knitr)        # Rmarkdown interactions
library(here)         # define top level of project folder
# this allows for specification of where 
# things live in relation to the top level
library(foreach)      # parallel execution
# spatial tasks
library(spdep)        # exploratory spatial data analysis
# random forest 
library(caret)        # machine learning model training 
library(rsample)      # splitting testing/training data
library(randomForest) # traditional RF model
library(SpatialML)    # spatial RF model
# others 
library(foreach)      # parrallel processing
library(ggpubr)       # arrange multiple graphs
library(ClustGeo)     # H clustering
library(RColorBrewer) # colorblind friendly pallettes 

# setting seed
set.seed(926) 
# loading data 
svi_df = read_csv(here::here("01_Data", "svi_df.csv")) %>% 
  mutate(fips = as.character(fips)) %>% 
  select(-...1) # if needed
# obtaining SPH files for RI tracts
tracts = tracts(state = "RI", year = 2022, cb = TRUE)
# joining data 
map = inner_join(tracts, svi_df, by = c("GEOID" = "fips")) %>% 
  mutate(fips = GEOID) %>% 
  select(fips, rpl_themes, starts_with("e_"))

df = map %>% st_drop_geometry()

#### Find a Partition with No Constraints #### 
D0 <- dist(df) # the df attribute distances 
tree <- hclustgeo(D0)

k_lst <- 6:8

for (k in k_lst) {
  # Open a new plot window for each k
  dev.new()
  # Plot the dendrogram
  plot(tree, hang = -1, label = FALSE, 
       xlab = "", sub = "",
       main = paste("Ward Dendrogram with D0 (k =", k, ")"))
  # Draw rectangles around clusters
  rect.hclust(tree, k = k, border = rainbow(k))
  # Add a legend
  legend("topright", legend = paste("Cluster", 1:k), 
         fill = rainbow(k), bty = "n", border = "white")
}

plot(tree,hang = -1, label = FALSE, 
     xlab = "", sub = "",
     main = "Ward dendrogram with D0 only")

rect.hclust(tree ,k = 5, border = c(4,5,3,2,1))
legend("topright", legend = paste("cluster",1:5), 
       fill=1:5,bty= "n", border = "white")

## based on a dendrograms a value of 8 is chosen
# Assign clusters to the tracts
P5 <- cutree(tree, 8)
tract_label <- as.vector(df$fips)
names(P5) <- tract_label
# Define a colorblind-friendly palette
palette <- brewer.pal(8, "Dark2")
# Assign colors to clusters based on the palette
colors <- palette[P5]
# adding cluster labels to the spatial data frame
map$cluster <- as.factor(P5)
# Create the plot using ggplot2
ggplot(data = map) +
  geom_sf(aes(fill = cluster), color = "grey") +
  scale_fill_manual(values = palette, name = "Cluster") +
  labs(title = "Partition P5 obtained with D0 only") +
  theme_void() +
  theme(legend.position = "left")

#### Find a Partition with Geographical Constraint Taken Into Account #### 

D1 <- as.dist(sf::st_distance(map)) # the geographic distances between the centriod of census tracts
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha, 
  K, graph = FALSE)

plot(cr)

# here the plot seggust to choose alpha = 0.5 which correponds to 
# a lost of SVI homogeneity of a few percentage points and a gain of 
# geographic homogeneity of about slight more percentage points 

tree <- hclustgeo(D0,D1,alpha=0.5)
P5bis <- cutree(tree,8)
map$P5bis <- as.factor(P5bis)

ggplot(data = map) +
  geom_sf(aes(fill = cluster), color = "grey") +
  scale_fill_manual(values = palette, name = "Cluster") +
  labs(title = "Partition P5bis obtained with alpha=0.5 
         and geographical distances") +
  theme_void() +
  theme(legend.position = "left")

#### Find a Partition with Neighborhood Constraint Taken Into Account 
list.nb <- poly2nb(map, queen=TRUE) #list of neighbours of each city
A <- nb2mat(neighbours = list.nb,style="B", zero.policy = TRUE)
D1 <- as.dist(1-A)

range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0, D1, range.alpha,
                  K, graph=FALSE)
plot(cr)
plot(cr, norm = TRUE)

tree = hclustgeo(D0,D1,alpha=0.5)
P5bis = cutree(tree,8)
map$P5bis = as.factor(P5bis)

# here the plot seggust to choose alpha = 0.5 

ggplot(data = map) +
  geom_sf(aes(fill = P5bis), color = "grey") +
  scale_fill_viridis_d(name = "Cluster") +
  labs(title = "Partition P5bis obtained with alpha=0.5 
         and neighborhood dissimilarities") +
  theme_void() +
  theme(legend.position = "left")


