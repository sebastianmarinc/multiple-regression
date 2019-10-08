# template for diagnosing heteroscedasticity and performing WLS regression

library(pacman)
p_load(broom, #for tidying data
       car,   #regression assist
       corrr, #for exploring correlations
       dplyr, #for data manipulation
       ggplot2,#for pretty plots
       modelr,#modeling 
       rio    #for importing data
       )

# import data
data <- import("")

# diagnosing heteroscedasticity
# visually examine bivariate relationships & save to 'p'
ggplot(data = data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() -> p
p

# fit simple linear regression for bivariate relationship
model_ols <- lm(y ~ x, data = data)
model_ols

# residual plots
par(mfrow = c(2,2))
plot(model_ols)


# weighted least squares regression
# get sqaured residuals
out_1 = augment(model_ols) %>%
  mutate(e_sq = .resid ^ 2) 

# fit new model w/ squared residuals as outcome
model2_ols = lm(e_sq ~ x, data = out_1)

# get predicted values
y_hat = fitted(model2_ols)

# get weights
w = 1 / (y_hat ^ 2)

# run regression with new weights
model_wls = lm(y ~ x, data = data, weights = w)
tidy(model_wls)

# go back to plot and plot new WLS line 
p + 
  geom_abline(intercept = model_wls$coefficients[1], 
              slope = model_wls$coefficients[2], 
              size=1)

# compare residual plots of models to assess model fit
# residual plots for OLS
par(mfrow=(c(1,2)))
qqPlot(model_ols)
residualPlot(model_ols)

# residual plots for WLS
par(mfrow=(c(1,2)))
qqPlot(model_wls)
residualPlot(model_wls)
