# k-means clustering example 

# lets just make some test data. 

library(tidyverse)
library(cluster)
library(factoextra)
library(meanShiftR)

zeroes = 3

var_a <- sample(c(1,1,1,2,3,4,5,5,5,6), 1*10^zeroes, replace = TRUE)

var_b <- sample(c(1,1,1,1,2,2,3,4,5,5,5,5,6), 1*10^zeroes, replace = TRUE)

variables <- data.frame(var_a = var_a, 
                        var_b = var_b)

### add some noise... 

variables <- variables %>%
  mutate(var_a = var_a+runif(1*10^zeroes, min = 0, max = 1),
         var_b = var_b+runif(1*10^zeroes, min =0, max = 1))

variables %>%
  ggplot(aes(x = var_a, y = var_b)) + 
  geom_point()

variables <- as.matrix(variables)

x  <- meanShiftR::meanShift(variables, variables)

x

variables <- cbind(variables, x$assignment)

variables %>%
  ggplot(aes(x = var_a, y = var_b, col = assignment)) + 
  geom_point()




