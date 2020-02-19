# template for testing measurement equivalence/invariance across groups
# assuming variables measured on likert-type scales

library(pacman)
p_load(lavaan, semPlot, semTools, tidyverse, broom, psych)
data <- read_csv("")


# get reliabilities for scales
a <- data %>% select(x1:x7)
alpha(a)
b <- data %>% select(x22:x26)
alpha(b)


# confirmatory factor analysis (scaled by fixing factor variances to 1)
model <- "
x =~ x1 + x2 + x3 + x4 + x5 + x6 + x7
P =~ x22 + x23 + x24 + x25 + x21
x~~P
"
base.fit <- cfa(model, data = data, std.lv = T)
fitmeasures(base.fit, c("chisq", "data", "pvalue", 
                        "cfi","rmsea", "srmr"))
tidy(base.fit)[,-c(2,5,9,11)] -> x
round(x[,-1], digits = 2)

# loadings 
inspect(base.fit, what = "std")$lambda

# path Diagram (standardized loadings)
semPaths(base.fit, "std", sizeMan = 6, rotation = 2, edge.label.cex = 1.25)


# measurement equivalence/invariance 
# configural invariance (group2 constrained parameters)
config.fit <- cfa(model, data = data, group = "group", std.lv = T)
fitmeasures(config.fit, c("chisq", "data", "pvalue",
                          "cfi","rmsea", "srmr"))
# configural parameter estimates
tidy(config.fit)[,-c(2,3,7,11,12)] -> x
round(x[,-1], digits = 2)

# group1 standarrdized loadings 
lavInspect(config.fit, what="std",list.by.group = T)$`group1`$`lambda`

# group2 standardized loadings
lavInspect(config.fit, what="std",list.by.group = T)$`group2`$`lambda`


# metric (weak) invariance (constrain loadings to be equal across groups)
metric.fit<-cfa(model, group="group", data = data,
                group.equal="loadings", std.lv = T) 
fitmeasures(metric.fit, c("chisq", "data", "pvalue", 
                          "cfi","rmsea", "srmr"))
# metric parameter estimates
tidy(metric.fit)[,-c(2,3,7,11,12)]-> x
round(x[,-c(1,2,3)], digits = 2)

# group1 Loadings 
lavInspect(metric.fit, what="std",list.by.group = T)$`group1`$`lambda`

# group2 loadings 
lavInspect(metric.fit, what="std",list.by.group = T)$`group2`$`lambda`


# scalar (strong) invariance (constrain item intercepts)
scalar.fit<-cfa(model, data = data, group = "group",
                group.equal = c("loadings","intercepts"),
                std.lv = 1)
fitmeasures(scalar.fit, c("chisq", "data", "pvalue",
                          "cfi","rmsea", "srmr"))

# scalar parameter estimates
tidy(scalar.fit)[,-c(2,3,7,11,12)]-> x
round(x[,-c(1,2,3)], digits = 2)

# group1 loadings 
lavInspect(scalar.fit, what="std",list.by.group = T)$`group1`$`lambda`

# group2 loadings 
lavInspect(scalar.fit, what="std",list.by.group = T)$`group2`$`lambda`


# strict invariance (lmao OKAY)(constrain error variances/covariances to be equal across groups)
strict.fit<-cfa(model, data = data, group="use",
                group.equal = 
                  c("loadings","intercepts",
                    "residuals"),
                std.lv = 1)
fitmeasures(strict.fit, c("chisq", "data", "pvalue",
                          "cfi","rmsea", "srmr"))
# strict invariance parameter estimates
tidy(strict.fit)[,-c(2,3,7,11,12)]-> x
round(x[,-c(1,2,3)], digits = 2)

# group1 loadings 
lavInspect(strict.fit, what="std",list.by.group = T)$`group1`$`lambda`

# group2 loadings 
lavInspect(strict.fit, what="std",list.by.group = T)$`group2`$`lambda`


# model comparison 
anova(config.fit, metric.fit, scalar.fit, strict.fit)


# alternative, most likely depreciated method
measurementInvariance(model=model, data=data, group="group")


# exploring partial invariance

# configural
# correlation of residuals 
lavResiduals(config.fit, type = "cor")$`group1`$cov
lavResiduals(config.fit, type = "cor")$`group1`$cov

# correlation of standardized residuals (z-scores) 
lavResiduals(config.fit, type = "cor")$`group1`$cov.z
lavResiduals(config.fit, type = "cor")$`group2`$cov.z

# modification indices
head(modificationindices(config.fit, sort. = T))


# metric
# correlation of residuals 
lavResiduals(metric.fit, type = "cor")$`group1`$cov
lavResiduals(metric.fit, type = "cor")$`group1`$cov

# correlation of standardized residuals (z-scores) 
lavResiduals(metric.fit, type = "cor")$`group1`$cov.z
lavResiduals(metric.fit, type = "cor")$`group2`$cov.z

# modification indices
head(modificationindices(metric.fit, sort. = T))


# scalar
# correlation of residuals
lavResiduals(scalar.fit, type = "cor")$`group1`$cov
lavResiduals(scalar.fit, type = "cor")$`group1`$cov

# correlation of standardized residuals (z-scores)
lavResiduals(scalar.fit, type = "cor")$`group1`$cov.z
lavResiduals(scalar.fit, type = "cor")$`group2`$cov.z

# mod indices
head(modificationindices(scalar.fit, sort. = T))

# examining partial invariance 
lavTestScore(metric.fit)$uni
lavTestScore(scalar.fit)$uni
lavTestScore(strict.fit)$uni


# comparing sigma and observed S Matrices
# reproducing S from sigma, where S is the observed correlations among the x and P items
# sigma = lambda * phi lambda^T + theta
# where sigma is a "true" covariance matrix, 
# lambda is a factor loadings matrix, 
# phi is a factor correlation matrix, 
# theta is a covariance matrix of unique variances

# get S matrix
x_P <- data %>% select(x1:x7, x22:x26)
S <- cor(x_P)
S

# get sigma
f.model <- lavInspect(base.fit, what="std")
L <- matrix(f.model$lambda, ncol=2)
P <- matrix(f.model$psi, ncol=2)
Th <- matrix(f.model$theta, ncol=12)

Sigma <- L %*% P %*% t(L) + Th
Sigma


# comparing Sigma matrix (correlation matrix reproduce from factors) to S matrix
abs(Sigma - S)
cortest.group2rmal(Sigma, S)
