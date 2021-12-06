
library(glmnet)
library(lars)

source('cleaning.R')

numeric <- numeric %>%
  mutate(income = da20520.0001$V421) %>%
  filter(!is.na(income))

# standardize
numeric_std <- numeric %>%
  lapply(FUN = as.integer) %>%
  lapply(FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) %>%
  as.data.frame() %>%
  as.matrix()

x <- numeric_std[ ,-80]
y <- numeric_std[ ,80]

lasso <- cv.glmnet(x = numeric_std[ ,-80],
                   y = numeric_std[ ,80],
                   type.measure = 'mse',
                   family = 'gaussian',
                   alpha = 1)
lasso1 <- glmnet(x = numeric_std[ ,-80],
                 y = numeric_std[ ,80],
                 family = 'gaussian',
                 alpha = 1,
                 lambda = lasso$lambda.min)



larsobj <- lars(x = numeric_std[ ,-80],
                y = numeric_std[ ,80],
                type = 'lasso')

reg <- lm(income ~ V141 + V142 + V143 + V145 + V146 + V148 + C4,
          data = as.data.frame(numeric_std))
summary(reg)


# ignore for now
na_proportions <- numeric %>%
  #summarize(across(.fns = function(x) sum(is.na(x)) / length(x)))
  lapply(FUN = function(x) sum(is.na(x)) / length(x)) %>%
  as.data.frame()


