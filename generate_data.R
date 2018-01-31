library(tidyverse)
library(magrittr)
# generate some high dimensional data that has 3 main clusters
num_dimension <- 10
num_clusters <- 3
num_samples <- 15
num_obs <- num_clusters*num_samples 

# takes two row indexes of our data and returns their distance from eachother
pw_dist <- function(i, j, data){
  # calc norm
  data_frame(i = i, j = j, dist = sqrt(sum((data[i,] - data[j,])^2)))
}

# matrix where each row is a centroid and the columns are its location in k-d space.
cluster_centroids <- runif(num_dimension*num_clusters, min = -10, max = 10) %>% 
  matrix(ncol = num_dimension, nrow = num_clusters)

gen_data_for_center <- function(center_id){
  centroid <- cluster_centroids[center_id, ]
  MASS::mvrnorm(num_samples, mu = centroid, Sigma = diag(num_dimension)) %>% 
    as_data_frame()
}

# generate matrix of values from clusters
all_data <- 1:num_clusters %>% 
  map_df(gen_data_for_center) %>% 
  as.matrix()

# all combinations of datapoints
pairwise_distances <- 1:num_obs %>% 
  map_df(~data_frame(p1 = (.:num_obs), p2 = .)) %$% 
  map2_df(p1, p2, ~pw_dist(.x, .y, data = all_data))

# observation info
obs_info <- data_frame(
  id = 1:num_obs,
  cluster_id = 1:num_clusters %>% rep(each = num_samples)
) 

pairwise_distances %>% write_csv('distances.csv')
obs_info %>% write_csv('obs_info.csv')


links <- pairwise_distances %>% 
  rename(source = i, target = j, value = dist) %>% 
  filter(value > 0)

nodes <- obs_info %>% 
  mutate(group = map_chr(cluster_id, ~(letters[.]))) %>% 
  select(-cluster_id)


list(links = links, nodes = nodes) %>% 
  jsonlite::toJSON() %>% 
  write_lines('data_info.json')

