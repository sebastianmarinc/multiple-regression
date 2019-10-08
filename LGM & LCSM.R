# template for latent growth models

library(pacman) #call pacman for reproducibility 
p_load(tidyverse, lavaan) #load libraries 

data <- read_csv("") #import .csv

# growth trajectories
## mean latent intercept and constrained residual variances
growth_model1 = '
# intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
int~~0*int

# residual variances
x1~~r*x1
x2~~r*x2
x3~~r*x3
x4~~r*x4
x5~~r*x5
'
growth_fit1 = growth(model = growth_model1, 
                     data data, 
                     mimic = "lavaan") #lavaan is default but can change to 'Mplus'
summary(growth_fit1)
parameterestimates(growth_fit1, standardized = T) #CIs for parameters
indices = c("df", "chisq","rmsea","srmr","cfi") #fit indices
fitmeasures(growth_fit1, indices) 

## mean latent intercept is allowed to vary, and constrained residual variances
growth_model2 = '
# intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
# residual variances
x1~~r*x1
x2~~r*x2
x3~~r*x3
x4~~r*x4
x5~~r*x5
'
growth_fit2 = growth(model = growth_model2, 
                     data data,
                     mimic = "lavaan")
summary(growth_fit2)
parameterestimates(growth_fit2, standardized = T) #CIs for parameters
fitmeasures(growth_fit2, indices)

## mean latent intercept is allowed to vary, 
## mean latent slope, and constrained residual variances
growth_model3 = '
# intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5

# slope
s =~ 0*x1 + 2*x2 + 4*x3 + 7*x4 + 10*x5
s ~ 0*1
s ~~ 0*int

# residual variances
x1~~r*x1
x2~~r*x2
x3~~r*x3
x4~~r*x4
x5~~r*x5
'
growth_fit3 = growth(model = growth_model3, 
                     data data,
                     mimic = "lavaan") 
summary(growth_fit3) 
parameterestimates(growth_fit3, standardized = T) #CIs for parameters
fitmeasures(growth_fit3, indices) 

## mean latent intercept is allowed to vary, 
## mean latent slope is allowed to vary, and constrained residual variances
growth_model4 = '
# intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5

# slope
s =~ 0*x1 + 2*x2 + 4*x3 + 7*x4 + 10*x5 

# residual variances
x1~~r*x1
x2~~r*x2
x3~~r*x3
x4~~r*x4
x5~~r*x5
'
growth_fit4 = growth(model = growth_model4, 
                     data data, 
                     mimic = "lavaan")
summary(growth_fit4)
parameterestimates(growth_fit4, standardized = T) #CIs for parameters
fitmeasures(growth_fit4, indices)

## fully unconstrained model
growth_model5 = '
# intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5

# slope
s =~ 0*x1 + 2*x2 + 4*x3 + 7*x4 + 10*x5
'
growth_fit5 = growth(model = growth_model5, 
                     data data, 
                     mimic = "lavaan")
summary(growth_fit5)
parameterestimates(growth_fit5, standardized = T) #CIs for parameters
fitmeasures(growth_fit5, indices) 

#modification indices
modindices(growth_fit5, sort. = T)

## NON-LINEAR 
growth_model6 <- '
#intercept
int =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5

# linear slope 
s =~ 0*x1 + 2*x2 + 4*x3 + 7*x4 + 10*x5

# quadratic slope
s2 =~ 0*x1 + 4*x2 + 16*x3 + 49*x4 + 100*x5

#cubic
#s3 =~ 0*x1 + 8*x2 + 64*x3 + 343*x4 + 1000*x5
'

growth_fit6 = growth(model = growth_model6, 
                     data = xs, 
                     mimic = "lavaan")
summary(growth_fit6)
parameterestimates(growth_fit6, standardized=TRUE) #CIs for parameters
fitmeasures(growth_fit6, indices)

#modification indices
modindices(growth_fit6, sort. = T)


#model comparison 
anova(growth_fit1,growth_fit2, growth_fit3,
      growth_fit4,growth_fit5,growth_fit6)


# latent change score modeling
library(RAMpath)

# create a data frame where each row is a case and 
# each column is a timepoint 
ramLCS(data, 1:3) 
