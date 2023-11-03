source("inst/simdata.R")


data <- simlabormarket()


library(tidyverse)

data <- data %>% as_tibble()

data <- data %>% 
  mutate(fid = factor(fid))

test <- data %>% 
  count(fid)

data

hist(test$n)
