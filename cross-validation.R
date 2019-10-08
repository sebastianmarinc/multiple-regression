# template for cross-validation (for out-of-sample predicton) 

# import data
library(pacman)
p_load(broom, car, corrr, tidyverse, ggplot2, modelr, purrr, rio)
data <- import("")



# fit model
model_ols <- lm(y ~ x1 + x2 + x3 + x4, data = data)

# compute sums of squares 
sst = sum((data$y - mean(data$y))^2)
yhat <- augment(model_ols)$.fitted
sse = sum((data$y - yhat)^2)

# 5-fold CV
set.seed(1000)
data %>% 
  crossv_kfold(k = 5) %>%
  mutate(model = map(train, ~ lm(y ~ x1 + x2 + x3 + x4, data = .))) %>%
  unnest(map2(model, test, ~ augment(.x, newdata = .y))) %>% 
  group_by(.id) %>%
  summarise(
    sst = sum((y - mean(y)) ^ 2),
    sse = sum((y - .fitted) ^ 2),
    r2 = 1 - sse / sst
  ) %>% mutate(mean_r2 = mean(r2)) -> cv
cv