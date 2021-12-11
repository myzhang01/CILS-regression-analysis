
library(tidyverse)
library(sjlabelled)  # for remove_all_labels()
library(glmnet)
library(lars)

set.seed(1234)

# Import ------------------------------------------------------------------

load('ICPSR_20520/DS0001/20520-0001-Data.rda')

labels <- attributes(da20520.0001)$variable.labels

cils <- as_tibble(da20520.0001) %>%
  filter(!is.na(V421)) %>%  # remove all observations with missing income
  remove_all_labels() %>%
  select(!matches('V4[[:alnum:]]{2,}'))  # remove 2005 variables
cils_inc_missing <- as_tibble(da20520.0001) %>%
  filter(is.na(V421))


# Deal with numeric variables ---------------------------------------------

numeric <- cils %>%
  select(where(is.double)) %>%
  select(!CASEID)

numeric_na <- numeric %>%
  lapply(FUN = function(x) sum(is.na(x)) / length(x)) %>%
  unlist()
numeric_na <- names(numeric_na[numeric_na >= 0.5])  # the 0.5 is the threshold for removing NAs

numeric <- numeric %>%
  select(!all_of(numeric_na)) %>%
  mutate(across(.cols = everything(), is.na, .names = '{.col}_NA')) %>%
  mutate(across(.cols = ends_with('_NA'), as.double)) %>%
  # mean imputation: for all columns not ending with NA, replace missing value
  # with the mean of the column
  mutate(across(.cols = !ends_with('_NA'), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))
numeric <- as.matrix(numeric)

# Deal with non-numeric variables -----------------------------------------

nonnumeric <- cils %>%
  select(where(is.factor))

nonnumeric_na <- nonnumeric %>%
  lapply(FUN = function(x) sum(is.na(x)) / length(x)) %>%
  unlist()
nonnumeric_na <- names(nonnumeric_na[nonnumeric_na >= 0.5])

nonnumeric <- nonnumeric %>%
  select(!all_of(c(nonnumeric_na))) %>%
  mutate(across(.cols = everything(), addNA))

nonnumeric_mat <- model.matrix(~., data = nonnumeric)[ ,-1]


# Test-train split --------------------------------------------------------

combined <- cbind(numeric, nonnumeric_mat)

foldid <- sample(rep(seq(10), length.out = nrow(combined)))

income <- as_tibble(da20520.0001) %>%
  filter(!is.na(V421)) %>%
  pull(V421)

# do lasso's
lasso <- cv.glmnet(x = combined,
                   y = income,
                   type.measure = 'mse',
                   standardize = TRUE,
                   foldid = foldid,
                   family = 'gaussian',
                   alpha = 1)


# get names of nonzero coefficient variables
# max stole all this code so don't ask him how it works
tmp_coeffs <- coef(lasso, s = "lambda.min")
tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
tmp_coeffs <- tmp_coeffs[-1, ]


# create formula for lm
coeffs <- tmp_coeffs$name %>%
  str_replace_all('[[:punct:]]*[[:space:]]*', '')
formula <- paste('income ~', paste(coeffs, collapse = ' + '))

colnames(combined) <- str_replace_all(colnames(combined), '[[:punct:]]*[[:space:]]*', '')

reg <- lm(formula = formula,
          data = cbind(as.data.frame(combined), income))
summary(reg)

