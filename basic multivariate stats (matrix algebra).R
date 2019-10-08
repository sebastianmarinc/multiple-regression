# template for basic statistics on multivariate data using matrix algebra 

# example 1
# create conformable matrices 
A <- matrix(c(1,2,0,-4,7,1,1,0,0), nrow=3)
B <- matrix(c(3,-1,0,4,2,1,0,3,3), nrow=3)

# create an identity matrix
I <- matrix(0,3,3)
diag(I )<- 1

# composite variables
# mean of y1
## create matrix X, unity vector, and weight vector
X<-matrix(c(15,9,7,9,25,21,21,25), nrow=4)
unity<-matrix(rep(1,4), nrow=4)
w1<-matrix(c(-1,1), nrow=2)

## compute the mean of composite y1 
y1<-(t(unity)%*% X %*% w1)/4

# mean of y2
## create weight vector
w2<-matrix(c(.5,.25), nrow=2)

## compute the mean of composite y2
y2<-(t(unity)%*%X%*%w2)/4

# standard deviations of composite variables y1 and y2 
# SD of y1
## compute the deviation score matrix 
Xd<-X-unity%*%t(colMeans(X))

## compute SD of y1
y1sd<-sqrt((t(Xd%*%w1)%*%(Xd%*%w1)/3))

## compute SD of y2
y2sd<-sqrt((t(Xd%*%w2)%*%(Xd%*%w2)/3))
y2sd

# covariance matrix for Y = (y1|y2)
# input weight matrix
W<-matrix(c(w1,w2), nrow = 2)

# compute composite Y
Cy<-t(W)%*%(1/3*(t(Xd)%*%Xd))%*%W

# example 2
# create new matrix X
X<-matrix(c(1,3,6,7,4,6,3,3,2,5,7,8,6,9,8,5,9,6,0,2,4,
            1,5,6,2,6,3,6,6,1,6,1,9,5,7,3,3,6,2,8),
          ncol = 4)
# column means
x<-colMeans(X)

# matrix of deviations from the mean
unity <- matrix(rep(1,10), nrow=10)
Xd<-X - unity%*%x

# the variance-covariance matrix
C<-(t(Xd)%*%Xd)/9

# standard deviations
# create null matrix
D<-matrix(0,4,4)
# fill diagonal of null matrix with variance
diag(D)<-diag(C)

# take the square root of the diagonal to get SDs
D<-sqrt(D)

# the matrix of correlations, R, using Xs
# solve for Xs
Xs<- Xd%*%solve(D)
# solve for R
R<-(t(Xs)%*%Xs)/9

# the matrix of correlations using C and the standard deviations
R<-solve(D)%*%C%*%solve(D)

# create a new Y matrix
# composite y1 
## create weight vector w1
w1<-matrix(c(1,1,0,0),nrow=4)

# compute the mean of composite y1 
y1<-(t(unity)%*%X%*%w1)/10

# compute the mean of composite y2
y2<-(t(unity)%*%X%*%w2)/10

# SD of y1
## compute the deviation score matrix 
Xd<-X-unity%*%t(colMeans(X))

## compute SD of y1
ySD1<-sqrt((t(Xd%*%w1)%*%(Xd%*%w1)/9))

## compute SD of y2
ySD2<-sqrt((t(Xd%*%w2)%*%(Xd%*%w2)/9))

#compute composite Y with weight matrix
C <-t(W)%*%(1/9*(t(Xd)%*%Xd))%*%W
