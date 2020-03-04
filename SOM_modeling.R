library(fields)
library(kohonen)

# Make a model matrix selecting only certain variables
som_data <- training %>% 
  select(Respondent,
         ConvertedComp,
         N_languages,
         YearsCodePro,
         Age) %>% 
  model.matrix(Respondent ~ ., data = .) %>% 
  .[, -1]

# Standardize the data
som_data_scaled <- apply(som_data, 2, scale)

# Make the SOM grid and model using all of the variables
set.seed(3)
som_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
som_model <- som(som_data_scaled,
                 grid = som_grid,
                 rlen = 100,
                 alpha = c(0.05, 0.01),
                 keep.data = TRUE)

# Set heat colors palette
heatColors <- function(n, alpha = 1) {
  rev(designer.colors(n = n, col = brewer.pal(9, "Spectral")))
}

# Function for plotting SOM heatmaps
plot_som <- function(variable) {
  
  # Take means of each cluster
  unit_colors <- som_data %>%
    cbind(unit.classif = som_model$unit.classif) %>% 
    as.data.frame() %>% 
    group_by(unit.classif) %>% 
    summarise(mean = mean(as.numeric(get(variable))))
  
  # Plot a single variable
  plot(som_model,
       type = "property",
       shape = "straight",
       property = unit_colors$mean,
       main = variable,
       palette.name = heatColors)
}

# Cluster and plot with cluster boundaries
par(mfrow = c(1, 1))

palette <- c("#F25F73", "#98C94C", "#888E94", "#33A5BF", "#F7D940")

# Perform hierarchial clustering
som_cluster <- cutree(hclust(dist(as.data.frame(som_model$codes))), 5)

# Plot plot with clusters
plot(som_model,
     type = "mapping",
     bgcol = palette[som_cluster],
     main = "Clusters",
     shape = "straight")
add.cluster.boundaries(som_model, som_cluster)

# Plot all the variables
par(mfrow = c(2, 2))
for (i in seq_along(colnames(som_data))) {
  plot_som(colnames(som_data)[i])
  add.cluster.boundaries(som_model, som_cluster)
}

par(mfrow = c(1, 1))

# # Plot the quality plots
# plot(som_model,
#      type = "counts",
#      shape = "straight",
#      main = "Node Counts")
# 
# plot(som_model,
#      type = "quality",
#      shape = "straight",
#      main = "Node Quality/Distance")
# 
# plot(som_model,
#      type = "dist.neighbours",
#      shape = "straight",
#      main = "SOM neighbour distances",
#      palette.name = grey.colors)
# 
# plot(som_model,
#      shape = "straight",
#      type = "codes")