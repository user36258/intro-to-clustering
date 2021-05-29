# k-means clustering example 

library(tidyverse)
library(cluster)
library(factoextra)
library(meanShiftR)

# size of test data, 3 = 1,000 rows, 2 = 100
zeroes <- 3
# k means requires you to tell it how many clusters there are 
# the sample data will create 4 clusters 
clusters <- 4

# randomly select data from 1 - 6 with more 1's and 5's than other numbers 
var_a <- sample(c(1,1,1,2,3,4,5,5,5,6), 1*10^zeroes, replace = TRUE)

# do the same thing again 
var_b <- sample(c(1,1,1,1,2,2,3,4,5,5,5,5,6), 1*10^zeroes, replace = TRUE)

# create a dataframe of the sample data
variables <- data.frame(var_a = var_a, 
                        var_b = var_b)

# add some noise to  the sample data 
# add a random number from -1 to 1 to each variable 
variables <- variables %>%
  mutate(var_a = var_a+runif(1*10^zeroes, min = -1, max = 1),
         var_b = var_b+runif(1*10^zeroes, min = -1, max = 1))

# plotting the pretend data to see how the clusters look 
variables %>%
  ggplot(aes(x = var_a, y = var_b)) + 
  geom_point()

# running the k means function
# one component of the result is a column with a number for each point 
# with the cluster that the data point refers to 

km  <- kmeans(variables, clusters)

# adding a column to the test data, with each cluster that the 
# point falls in
variables <- variables %>%
  mutate(cluster = km$cluster)  %>%
  # converting to factor, or ggplplot will shade it rather than 
  # using discrete colours
  mutate(cluster = factor(cluster, levels = 1:clusters))

# plotting the test data with the colours indicating the cluster
variables %>% 
  ggplot(aes(x = var_a, y = var_b, col = cluster)) + 
  geom_point() 




