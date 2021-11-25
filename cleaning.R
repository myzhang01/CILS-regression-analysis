
library(tidyverse)
library(sjlabelled)

load('ICPSR_20520/DS0001/20520-0001-Data.rda')

cils <- as_tibble(da20520.0001)

labels <- attributes(cils)$variable.labels

cils <- remove_all_labels(cils)

numeric <- cils %>% 
  select(!where(is.factor))

nonnumeric <- cils %>% 
  select(where(is.factor))

test <- nonnumeric %>% 
  apply(MARGIN = 2, FUN = function(x) length(unique(x)))

unordered <- nonnumeric[test > 6]
ordered <- nonnumeric[test <= 6]

ordered_num <- ordered %>% 
  lapply(FUN = as.integer) %>% 
  lapply(FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))








