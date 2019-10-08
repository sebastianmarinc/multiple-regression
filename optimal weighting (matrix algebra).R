# template for optimal weights for out-of-sample prediction (aka stability)

#create X matrix of multiple standardized predictors
x1<-matrix(c(1.0000, -0.4360, -0.4820, 
             -0.4360, 1.0000, 0.6242, 
             -0.4820, 0.6242, 1.0000),nrow=3)

#create Y matrix of standardized outcome
y1<-matrix(c(-0.1506,
             0.4150,
             0.3775),nrow=3)

# estimate standardized least-squares regression coefficients
b1<-solve(x1)%*%y1

# calculate r-squared with optimal regression weights
r2<-((t(b1)%*%y1)^2)/t(b1)%*%x1%*%b1

# calculate r-squared with rounded optimal regression weights 
b.round1<-matrix(c(.1,.3,.2),nrow=3)
r2<-((t(b.round1)%*%y1)^2)/t(b.round1)%*%x1%*%b.round1

# calculate unit weights and compute the r-squared
b.unit<-matrix(c(rep(1,3)),nrow=3)
r2<-((t(b.unit)%*%y1)^2)/t(b.unit)%*%x1%*%b.unit
r2


# get some new data
x2<-matrix(c(1.0000,-0.3402, -0.3980,
             -0.3402, 1.0000, 0.5090, 
             -0.3980, 0.5090, 1.0000),nrow=3)
y2<-matrix(c(-0.1219, 
             0.3200, 
             0.3629),nrow=3)

# estimate standardized least-squares regression coefficients 
b2<-solve(x2)%*%y2

# calculate r-squared with optimal regression weights
r2<-((t(b2)%*%y2)^2)/t(b2)%*%x2%*%b2

# calculate r-squared using sample 2 data with optimal weights of sample 1
r2<-((t(b1)%*%y2)^2)/t(b1)%*%x2%*%b1
r2

# calculate r-squared using sample 2 data with rounded weights of sample 1
r2<-((t(b.round1)%*%y2)^2)/t(b.round1)%*%x2%*%b.round1
r2

# calculate r-squared in sample 2 using unit weights
b.unit<-matrix(c(rep(1,3)),nrow=3)
r2<-((t(b.unit)%*%y2)^2)/t(b.unit)%*%x2%*%b.unit
r2

# can you see the difference? try it out on your own data 
