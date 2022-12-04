library(tidyverse)

# set seed
set.seed(742)

# create data
rand_data <- matrix(rnorm(15*25000, mean = 1000, sd = 500), 
                    nrow = 25000, ncol = 15)
y <- runif(25000, min = 500, max = 1500)
rand_data <- as.data.frame(cbind(rand_data, y))

write.csv(rand_data, file = "rand_data.csv")



