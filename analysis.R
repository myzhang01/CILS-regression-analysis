
library(glmnet)
library(lars)

source('cleaning.R')

# standardize
numeric_std <- numeric %>%
  select(where(function(x) sum(x) != 0)) %>%  # this is hacked solution and should be improved
  mutate(across(.cols = everything(), function(x) (x - mean(x)) / sd(x))) %>%
  as.matrix()

# get income column
income <- as_tibble(da20520.0001) %>%
  filter(!is.na(V421)) %>%
  pull(V421)

# do lasso's
lasso <- cv.glmnet(x = numeric_std,
                   y = income,
                   type.measure = 'mse',
                   family = 'gaussian',
                   alpha = 1)
lasso1 <- glmnet(x = numeric_std,
                 y = income,
                 family = 'gaussian',
                 alpha = 1,
                 lambda = lasso$lambda.min)

# categorical var lasso
lasso2 <- cv.glmnet(x = nonnumeric_mat,
                    y = income,
                    type.measure = 'mse',
                    family = 'gaussian',
                    alpha = 1)
#lasso3 <- glmnet(x = nonnumeric_mat,
#                 y = income,
#                 family = 'gaussian',
#                 alpha = 1,
#                 lambda = lasso2$lambda.min)


# get names of nonzero coefficient variables
# max stole all this code so don't ask him how it works
tmp_coeffs <- coef(lasso, s = "lambda.min")
tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
tmp_coeffs <- tmp_coeffs[-1, ]

tmp_coeffs2 <- coef(lasso2, s = "lambda.min")
tmp_coeffs2 <- data.frame(name = tmp_coeffs2@Dimnames[[1]][tmp_coeffs2@i + 1], coefficient = tmp_coeffs2@x)
tmp_coeffs2 <- tmp_coeffs2[-1, ]


# create formula for lm
formula <- paste('income ~', paste(tmp_coeffs$name, collapse = ' + '))

reg <- lm(formula = formula,
          data = cbind(as.data.frame(numeric_std), income))
summary(reg)




