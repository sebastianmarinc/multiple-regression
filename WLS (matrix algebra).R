# template of WLS regression using matrices

library(rio)

# import data
data <- import("")
cases <- nrow(data)

# compute weights for model w/ 2 predictors
X <- matrix(c(data$x1, data$x2), ncol=2) #create x matrix
unity <- matrix(c(rep(1, cases))) #create a unity vector
X <- cbind(unity, X) #create design matrix
I <- diag(cases) #create identity matrix
y <- matrix(data$y, ncol = 1) #create y vector
hat <- X %*% solve(t(X) %*% X) %*% t(X) #create hat matrix to fit values
res <- (I - hat) %*% y #calculate residual vector 
res.sq <- res^2 #square residuals
y_hat <- hat %*% res.sq #get fitted values
w <- 1 / (y_hat^2) #calculate weights vector

# get 2 regression weights
W <- matrix(0, cases, cases) #weight matrix
diag(W) <- w
b <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% y)
b

# get 2 standard errors
e = y - X %*% b #residuals from OLS
mse = t(w) %*% e ^ 2 %*% solve(cases - 3) #compute MSE 
MSE <- diag(0,3,3)
diag(MSE) <- mse

# compute variance-covariance matrix for B to get SEs
vc_b = MSE %*% solve(t(X) %*% W %*% X) 
sqrt(diag(vc_b)) -> se
se

# assess significance of regression weights 
# x1
x1_t <- b[2]/se[2] #t-value 
x1_p <- 2*pt(x1_t, c(cases - 3)) #p-value
#x2
x2_t <- b[3]/se[3]
x2_p <- 2*pt(x2_t, c(cases - 3))  