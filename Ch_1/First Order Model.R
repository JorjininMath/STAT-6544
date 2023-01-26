## Main effects model examplea

# Example 1: \yita = 50 + 8*x_1 + 3*x_2

first.order <- function(x1, x2)
{
  50 + 8 * x1 + 3 * x2
}

x1 <- x2 <- seq(-1, 1, length=100)
g <- expand.grid(x1, x2)
eta1<- matrix(first.order(g[, 1], g[, 2]), ncol=length(x2))

par(mfrow=c(1, 2))
persp(x1, x2, eta1, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta1, col=heat.colors(128))
contour(x1, x2, matrix(eta1, ncol=length(x2)), add = TRUE)

# Example 2: \yita = 50 + 8*x_1 + 3*x_2 - 4*x_1x_2

first.order.i <- function(x1, x2)
{
  50 + 8 * x1 + 3 * x2 - 4 * x1 * x2
}

x1 <- x2 <- seq(-1, 1, length=100)
g <- expand.grid(x1, x2)
eta1i<- matrix(first.order.i(g[, 1], g[, 2]), ncol=length(x2))

par(mfrow=c(1, 2))
persp(x1, x2, eta1i, theta=30, phi=30, zlab="eta", expand=0.75, lwd=0.25)
image(x1, x2, eta1i, col=heat.colors(128))
contour(x1, x2, matrix(eta1i, ncol=length(x2)), add = TRUE)


