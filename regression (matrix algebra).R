# template for regression using matrix algebra 

# example 1 w/ random data
unity<-matrix(c(rep(1,10)))
X<-matrix(c(
            1,3,6,7,4,6,3,3,2,5,
            7,8,6,9,8,5,9,6,0,2,
            4,1,5,6,2,6,3,6,6,1,
            6,1,9,5,7,3,3,6,2,8), ncol=4)
X<-cbind(unity, X)
y<-matrix(c(1,8,-1,8,4,5,7,1,-2,-3), ncol = 1)
m<-nrow(X)

# compute b coefficients from X and y
b<-(solve(t(X)%*%X))%*%(t(X)%*%y)

# compute b coefficients using deviation scores: 
# create X deviation scores
Xd<-X-unity%*%t(colMeans(X))
Xd<-Xd[,-1]

#create y deviation scores
yd <-y-unity%*%colMeans(y)

# calculate b coefficients using deviation matrices
b2<-solve(t(Xd)%*%Xd)%*%t(Xd)%*%yd

# compute the b coefficients using covariance matrix for X and covariance validity vector
# create Cxx
Cxx<-1/(m-1)*((t(Xd)%*%Xd))

# create Cxy
Cxy<-1/(m-1)*(t(Xd)%*%yd)

#compute b coefficients
b3<-solve(Cxx)%*%Cxy

# compute the b coefficients using standardized scores for X and y

# compute Xz
Dx<-diag(diag(Cxx))
Xz<-Xd%*%solve(Dx^.5)

# compute yz
yz<-yd/sd(y)

#compute b
b4<-solve(t(Xz)%*%Xz)%*%(t(Xz)%*%yz)


# we can also standardize each b coefficient by multiplying each by Sx/Sy
xsd<-matrix(c(0,3.01,9.30,2.74))
s<-xsd/3.19
b<-matrix(c(11.40,0,0,0,
            0,.319,0,0,
            0,0,0.228,0,
            0,0,0,.028), ncol=4)
bs<-b%*%s
rownames(bs)<-c("b0","b1","b2", "b3")
bs

# interactive function to calculate reression coefficients 
regress <- function(X,y){
            x<-askYesNo("Are these raw scores?", 
                        gettext(c("Yes", "No", "Cancel")))
              
              if(x==T){
              X<-cbind(unity, X)
              b<-(solve(t(X)%*%X))%*%(t(X)%*%y)
              return(b)}
              else{
              b<-(solve(t(X)%*%X))%*%(t(X)%*%y)
              return(b)}
          
  }

# testing interactive function
# input the X matrix from problem 1 without the unity vector
X<-matrix(c(
            1,3,6,7,4,6,3,3,2,5,
            7,8,6,9,8,5,9,6,0,2,
            4,1,5,6,2,6,3,6,6,1,
            6,1,9,5,7,3,3,6,2,8), ncol=4)

regress(X,y)
regress(Xd, yd)
regress(Xz,yz)