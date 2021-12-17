
library(tidyverse)
library(sjlabelled)  # for remove_all_labels()


# import
load('ICPSR_20520/DS0001/20520-0001-Data.rda')

# get features and response
cils <- as_tibble(da20520.0001) %>%
  filter(!is.na(V421)) %>%
  select(!c(CASEID, V1)) %>%
  remove_all_labels()

log.income <- cils %>%
  pull(V421) %>%
  log()

# drop 2005 variables
cils <- cils %>%
  select(!matches('V4[[:alnum:]]{2,}'))

# deal with numeric variables
numeric <- cils %>%
  select(where(is.double))

numeric <- numeric %>%
  select(where(~ sum(is.na(.x)) / length(.x) < 0.5)) %>%                        # select variables less than 50% NA
  mutate(across(.cols = everything(), is.na, .names = '{.col}_NA')) %>%         # add dummy NA columns
  mutate(across(.cols = ends_with('_NA'), as.double)) %>%                       # turn dummy NA columns to double
  mutate(across(.cols = !ends_with('_NA'),
                ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x))) %>%          # mean imputation
  as.matrix()

# deal with categorical variables
categorical <- cils %>%
  select(where(is.factor))

#many_levels <- c('V32', 'V37', 'V233', 'V238', 'V263', 'V62', 'V64', 'V264')   # occupations

categorical <- categorical %>%
  select(where(~ nlevels(.x) <= 25)) %>%                                        # drop variables with more than 25 levels
  #select(!all_of(many_levels)) %>%                                             # drop variables in many_levels
  mutate(across(.cols = everything(), addNA))                                   # make NA a level
categorical <- model.matrix(~ ., data = categorical)[ ,-1]                      # create design matrix, excluding intercept








