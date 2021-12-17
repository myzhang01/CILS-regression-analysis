
library(glmnet)

# combine
combined <- cbind(log.income, numeric, categorical)


# test-train split
# set.seed(123)

indices <- sample(1:nrow(combined), size = 0.7*nrow(combined))

test <- combined[-indices, ]
train <- combined[indices, ]


# LASSO
tmp_coeffs_collection = list()
for (i in 1:10){
  cv <- cv.glmnet(x = train[ ,-1],
                  y = train[ ,1],
                  type.measure = 'mse',
                  standardize = TRUE,
                  family = 'gaussian',
                  alpha = 1)

  # get names of nonzero coefficient variables
  tmp_coeffs <- coef(cv, s = "lambda.min")
  tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  tmp_coeffs <- tmp_coeffs[-1, ]
  tmp_coeffs_collection = append(tmp_coeffs_collection,tmp_coeffs[1])
}
tmp_coeffs_collection
tmp_coeffs = Reduce(intersect,tmp_coeffs_collection)


# bootstrap:
bootstrap_size = 1000
lm_coefficients_collection = matrix(nrow = length(tmp_coeffs) + 1,ncol = bootstrap_size)

for (i in 1:bootstrap_size){
  boot_indics = sample(nrow(test),replace = T)
  boot_sample = test[boot_indics,]
  # create formula for lm
  coeffs <- tmp_coeffs %>%
    str_replace_all('[[:punct:]]*[[:space:]]*[=+]*', '')
  formula <- paste('logincome ~', paste(coeffs, collapse = ' + '))

  colnames(test) <- str_replace_all(colnames(test), '[[:punct:]]*[[:space:]]*[=+]*', '')

  reg <- lm(formula = formula,
            data = as.data.frame(boot_sample))
  lm_coefficients_collection[,i] = reg$coefficients
}


# percentile interval
CIs_lower = apply(lm_coefficients_collection, MARGIN = 1,quantile,probs = 0.025,na.rm = T)
CIs_upper = apply(lm_coefficients_collection, MARGIN = 1,quantile,probs = 0.975,na.rm = T)
coeffs_names = append('intercept',tmp_coeffs)
CIs_table = cbind(coeffs = coeffs_names,Lower_Bound = round(CIs_lower,3),Upper_Bound = round(CIs_upper,3))

# extract the coeffients whose 95% confidence levels don't contain 0
CIs_table[!((CIs_table[,'Lower_Bound'] <= 0) & (CIs_table[,'Upper_Bound'] >= 0)),]



