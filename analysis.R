
library(glmnet)
library(lars)

source('cleaning.R')

# do lasso's
lasso <- cv.glmnet(x = train,
                   y = income_train,
                   type.measure = 'mse',
                   standardize = TRUE,
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
formula <- paste('income_test ~', paste(coeffs, collapse = ' + '))

colnames(test) <- str_replace_all(colnames(test), '[[:punct:]]*[[:space:]]*', '')

reg <- lm(formula = formula,
          data = cbind(as.data.frame(test), income_test))
summary(reg)




