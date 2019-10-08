# example of factor analysis of BFI using matrix algebra

library(psych)
library(tidyverse)
library(GPArotation)
data(bfi)
bfi<-bfi[,-c(26:28)]

# function to determine the number of missing responses 
na_subject <- function(x) rowSums(is.na(x))

# complete cases only
bfi$na<-na_subject(bfi)
bfi2<-bfi %>% filter(bfi$na==0)
NULL->bfi2$na
NULL->bfi$na
nrow(bfi2)


# select the number of components/factors to retain
# scree plot 
scree(bfi2, factors=T, pc=T, hline=-1)

# conduct a parallel analysis 
fa.parallel(bfi2)

# principal components analysis
# get correlation matrix
cor(bfi2)->cor.bfi

# decomposition
WLW <- eigen(cor.bfi)
W <- WLW$vectors
L <- WLW$values
L.sqrt <- diag(sqrt(L))

# truncate to 5 components
P <- (W %*% L.sqrt)[,1:5]

# name rows and columns for easy reading
rownames(P) <- c("A1","A2","A3","A4","A5", 
                 "C1","C2","C3","C4","C5",
                 "E1","E2","E3","E4","E5",
                 "N1","N2","N3","N4","N5", 
                 "O1","O2","O3","O4","O5")
colnames(P) <- c("cpmponent 1", "component 2", "component 3","component 4","component 5")

# read results
round(P, digits = 2)

#amount of variance accounted for by each component
variance <- apply(P^2, 2, sum)
variance

# get component loadings after applying a varimax rotation 
varimax(P)->P_varimax
round(P_varimax$loadings[], 2)



# factor analysis
# varimax
fa_varimax <- fa(bfi2, rotate="varimax", nfactors = 6, fm="pa")
round(fa_varimax$loadings, 2)

# oblimin 
fa_oblimin <- fa(bfi2,nfactors = 6, fm="pa")
round(fa_oblimin$loadings, 2)



# factor score indeterminacy for varimax 
Rxx<-cor.bfi
faOut <- factanal(covmat = Rxx, factors = 6)
var <- Varimax(L = faOut$loadings)
Fp <- var$loadings
Phi <- diag(6)
f <- Fp %*% Phi
rho<-diag((t(f)%*%solve(Rxx)%*%f)^.5)

# minimum correlation between two sets of factor scores for each factor
2*rho^2-1

# for oblimin
obl <- oblimin(L = faOut$loadings)
Fp <- obl$loadings
Phi <- obl$Phi
f <- Fp %*% Phi
rho<-diag((t(f)%*%solve(Rxx)%*%f)^.5)

2*rho^2-1