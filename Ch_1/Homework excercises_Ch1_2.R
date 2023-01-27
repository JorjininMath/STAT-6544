### Homework Excercises in Chapter 1

## #2: surrogates for sensitivity

# M    Piston weight (kg)
# S    Piston surface area (m^2)
# V0   Initial gas volume (M^2)
# k    Spring coefficient (N/M)
# P0   Atmospheric pressure (N/m^2)
# Ta   Ambient temperature (K)
# T0   Filling gas temperature (K)

# piston simulation function

piston <- function(M=0.5, S=0.5, V0=0.5, k=0.5,
                   P0=0.5, Ta=0.5, T0=0.5)
{
  # put coded inputs back on natural scale
  M <- M*(60 - 30) + 30
  S <- S*(0.02 - 0.005) + 0.005
  V0 <- V0*(0.01 - 0.002) + 0.002
  k <- k*(5000 - 1000) + 1000
  P0 <- P0*(110000 - 90000) + 90000
  Ta <- Ta*(296 - 290) + 290
  T0 <- T0*(360 - 340) + 340
  
  # calculation on natural scale
  A <- P0*S + 19.62*M - k*V0/S
  V <- (S/(2*k))*(sqrt(A^2 + 4*k*(P0*V0/T0)*Ta) - A)
  C <- 2*pi*sqrt(M/(k + S^2*(P0*V0/T0)*(Ta/V^2)))
  
  return(C)
}

# plot C(x). Start with a LHS design in 7d 
# and fit a GP surrogate to the responses.

x <- seq(0, 1, length=100)
g <- expand.grid(x, x)

# W.V0.Ta <- piston(M=g[,1], T0=g[,2])
# cs <- heat.colors(128)
# bs <- seq(min(W.V0.Ta), max(W.V0.Ta), length=129)
# 
# image(x, x, matrix(W.V0.Ta, ncol=length(x)), col=cs, breaks=bs, 
#       xlab="M", ylab="T0")
# contour(x, x, matrix(W.V0.Ta, ncol=length(x)), add=TRUE)

library(lhs)
n <- 1000
X <- data.frame(randomLHS(n, 7))
names(X) <- names(formals(piston))
Y <- piston(X[,1], X[,2], X[,3], X[,4], X[,5], X[,6], X[,7])

# fit with a GP surrogate

library(laGP)

fit.gp <- newGPsep(X, Y, 2, 1e-6, dK=TRUE)
mle <- mleGPsep(fit.gp)

baseline <- matrix(rep(as.numeric(formals(piston)), nrow(g)),
                   ncol=7, byrow=TRUE)
XX <- data.frame(baseline)
names(XX) <- names(X)
XX$S <- g[, 1]
XX$k <- g[, 2]

p <- predGPsep(fit.gp, XX, lite=TRUE)

W.S.P0 <- piston(S=g[,1], k=g[,2])
cs <- heat.colors(128)
bs <- seq(min(W.S.P0), max(W.S.P0), length=129)

image(x, x, matrix(p$mean, ncol=length(x)), col=cs, breaks=bs,
      xlab="S", ylab="k")
contour(x, x, matrix(p$mean, ncol=length(x)), add=TRUE)








