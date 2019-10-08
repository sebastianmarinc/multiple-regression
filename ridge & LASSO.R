# shrinkage methods 
# template

library(pacman)
p_load(dplyr, #data manipulation
       broom, #for tidying data
       car,   #regression assist
       corrr, #for correlations
       ggplot2, #for pretty plots
       sm,    #for smoothing it out 
       glmnet,#for regularization 
       rio    #for imporitng data
       )
data <- import("")

# correlation matrix of outcome and predictors
cor(data)

# fit OLS (scaled) model 
data.z <- data.frame(scale(data))
model_ols <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data.z)
tidy(model_ols)

# diagnose multicollinearity 
vif(model_ols)
sqrt(vif(model_ols))

# compute condition indices
x = cor(data)
eigen(x)
sum(1 / eigen(x)$values)#sum of reciprocal of eigenvalues
sqrt(max(eigen(x)$values) / eigen(x)$values)#condition indices

# ridge regression (L2 penalty)
# create x matrix
x = data.z %>%
  dplyr::select(x1, x2, x3, x4, x5) %>%
  data.matrix()

# create y vector
y = data.z$y

# choose lambda from cross-validation
set.seed(42) #for reproducibility 
# cross-validate with L2 penalty
ridge = cv.glmnet(x = x, y = y, alpha = 0, intercept = FALSE)
plot(ridge) #plot results
l2 <- ridge$lambda.min #extract lambda w/ lowest MSE

# fit a new model w/ L2 penalty
l2_best = glmnet(x = x, y = y, alpha = 0, 
                 lambda = lambda2, intercept = FALSE)
tidy(l2_best)

# LASSO regression (L1 penalty)
# cross-validate w/ L1 
lasso = cv.glmnet(x = x, y = y, alpha = 1, intercept = FALSE)
plot(lasso) #plot results
l1 <- lasso$lambda.min #extract lambda w/ lowest MSE

# fit a new model w/ L1 penalty
l1_best = glmnet(x = x, y = y, alpha = 1,
                 lambda = lambda1, intercept = FALSE)
tidy(l1_best)
