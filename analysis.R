
library(glmnet)

# combine
combined <- cbind(log.income, numeric, categorical)

# test-train split
set.seed(123)

indices <- sample(1:nrow(combined), size = 0.7*nrow(combined))

test <- combined[-indices, ]
train <- combined[indices, ]

# LASSO
cv <- cv.glmnet(x = train[ ,-1],
                y = train[ ,1],
                type.measure = 'mse',
                standardize = TRUE,
                family = 'gaussian',
                alpha = 1)

# get names of nonzero coefficient variables
# max stole all this code so don't ask him how it works
tmp_coeffs <- coef(cv, s = "lambda.min")
tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
tmp_coeffs <- tmp_coeffs[-1, ]

# create formula for lm
coeffs <- tmp_coeffs$name %>%
  str_replace_all('[[:punct:]]*[[:space:]]*[=+]*', '')
formula <- paste('logincome ~', paste(coeffs, collapse = ' + '))

colnames(test) <- str_replace_all(colnames(test), '[[:punct:]]*[[:space:]]*[=+]*', '')

reg <- lm(formula = formula,
          data = as.data.frame(test))
summary(reg)




