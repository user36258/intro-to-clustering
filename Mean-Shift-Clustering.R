# k-means clustering example in 3d

library(tidyverse)
library(cluster)
library(factoextra)
library(meanShiftR)
library(plotly)

# size of test data, 3 = 1,000 rows, 2 = 100
zeroes <- 3
# k means requires you to tell it how many clusters there are 
# the sample data will create 8 clusters 
clusters <- 4

# randomly select data from 1 - 6 with more 1's and 5's than other numbers 
x <- sample(c(1,1,1,2,3,4,5,5,5,6), 1*10^zeroes, replace = TRUE)

# do the same thing again 
y <- sample(c(1,1,1,1,2,2,3,4,5,5,5,5,6), 1*10^zeroes, replace = TRUE)

# create a dataframe of the sample data
variables <- data.frame(x = x, 
                        y = y)

# add some noise to  the sample data 
# add a random number from -1 to 1 to each variable 
variables <- variables %>%
  mutate(x = x + runif(1*10^zeroes, min = -1, max = 1),
         y = y + runif(1*10^zeroes, min = -1, max = 1))

# plotting the pretend data to see how the clusters look 

plot_ly(x=variables$x, 
        y=variables$y, 
        type="scatter", 
        mode="markers")

matrix_variables <- as.matrix(variables)


ms  <- meanShift(matrix_variables, epsilonCluster = 1e-1)

clusters <- n_distinct(ms$assignment)

variables <- variables %>%
  mutate(cluster = ms$assignment) %>%
  mutate(cluster = factor(cluster, levels = 1:clusters))

plot_ly(x=variables$x, 
        y=variables$y, 
        z=variables$z, 
        type="scatter", 
        mode="markers",
        color = variables$cluster)



# This looks ok in general but way too many clusters 
# could just filter out the clusters with less than n members, but first
# tune with the epsilon factor. 
# default epsilon is 1e-04

ms  <- meanShift(matrix_variables, epsilonCluster = 1e-1)

clusters <- n_distinct(ms$assignment)

variables <- variables %>%
  mutate(cluster = ms$assignment) %>%
  mutate(cluster = factor(cluster, levels = 1:clusters))

plot_ly(x=variables$x, 
        y=variables$y, 
        type="scatter", 
        mode="markers",
        color = variables$cluster)

# now about 6 clusters 
# but two clusters v small. 

cluster_count <- variables %>% group_by(cluster) %>%
  summarise(count = n())

# filter out all clusters with less than 20 members and re plot 

variables_filtered <- variables %>%
  left_join(cluster_count) %>%
  filter(count >= 20)

plot_ly(x=variables_filtered$x, 
        y=variables_filtered$y, 
        type="scatter", 
        mode="markers",
        color = variables_filtered$cluster)

# that looks better 

glimpse(ms)


