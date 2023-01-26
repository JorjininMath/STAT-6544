## Second-order model

# \yita = 50 + 8*x1 + 3*x2 - 7 * x1^2 - 3 * x2^2 - 4*x1x2

simple.max <- function(x1, x2)
{
  50 + 8*x1 + 3*x2 - 7*x1^2 - 3*x2^2 - 4*x1*x2
}

eta2sm <- matrix(simple.max(g[, 1], g[, 2]), ncol=length(x2))

par(mfrow=c(1,2))
persp(x1, x2, eta2sm, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta2sm, col=heat.colors(128))
contour(x1, x2, eta2sm, add=TRUE)

# stationary ridge
# \yita = 80 + 4*x1 + 8*x2 - 3*x1^2 - 12*x2^2 - 12*x1*x2

stat.ridge <- function(x1, x2)
{
  80 + 4*x1 + 8*x2 - 3*x1^2 - 12*x2^2 - 12*x1*x2
}

eta2sr <- matrix(stat.ridge(g[,1], g[,2]), ncol=length(x2))

par(mfrow=c(1,2))
persp(x1, x2, eta2sr, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta2sr, col=heat.colors(128))
contour(x1, x2, eta2sr, add=TRUE)

# rising ridge
# \yita = 80 + 4*x1 + 8*x2 - 3*x1^2 - 12*x2^2 - 12*x1*x2

rise.ridge <- function(x1, x2)
{
  80 - 4*x1 + 12*x2 - 3*x1^2 - 12*x2^2 - 12*x1*x2
}

eta2rr <- matrix(rise.ridge(g[,1], g[,2]), ncol=length(x2))

par(mfrow=c(1,2))
persp(x1, x2, eta2rr, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta2rr, col=heat.colors(128))
contour(x1, x2, eta2rr, add=TRUE)

# saddle
# \yita = 80 + 4*x1 + 8*x2 - 2*x1 - 12*x2 - 12*x1*x2

saddle <- function(x1, x2) 
{
  80 + 4*x1 + 8*x2 - 2*x1 - 12*x2 - 12*x1*x2 
}

eta2s <- matrix(saddle(g[,1], g[,2]), ncol=length(x2))

par(mfrow=c(1,2))
persp(x1, x2, eta2s, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta2s, col=heat.colors(128))
contour(x1, x2, eta2s, add=TRUE)


