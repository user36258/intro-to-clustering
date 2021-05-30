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
clusters <- 8

# randomly select data from 1 - 6 with more 1's and 5's than other numbers 
x <- sample(c(1,1,1,2,3,4,5,5,5,6), 1*10^zeroes, replace = TRUE)

# do the same thing again 
y <- sample(c(1,1,1,1,2,2,3,4,5,5,5,5,6), 1*10^zeroes, replace = TRUE)

# and again
z <- sample(c(1,1,1,1,2,2,3,4,5,5,5,5,6), 1*10^zeroes, replace = TRUE)

# create a dataframe of the sample data
variables <- data.frame(x = x, 
                        y = y,
                        z = z)

# add some noise to  the sample data 
# add a random number from -1 to 1 to each variable 
variables <- variables %>%
  mutate(x = x + runif(1*10^zeroes, min = -1, max = 1),
         y = y + runif(1*10^zeroes, min = -1, max = 1),
         z = z + runif(1*10^zeroes, min = -1, max = 1))

# plotting the pretend data to see how the clusters look 

plot_ly(x=variables$x, 
        y=variables$y, 
        z=variables$z, 
        type="scatter3d", 
        mode="markers")

# running the k means function
# one component of the result is a column with a number for each point 
# with the cluster that the data point refers to 

km  <- kmeans(variables, clusters)

variables <- variables %>%
  mutate(cluster = km$cluster) %>%
  mutate(cluster = factor(cluster, levels = 1:clusters))

plot_ly(x=variables$x, 
        y=variables$y, 
        z=variables$z, 
        type="scatter3d", 
        mode="markers",
        color = variables$cluster)




