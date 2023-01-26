## banana yield as a function of time and temperature

# 3D plot
yield <- function(xi1, xi2)
{
  xi1 <- 3 * xi1 - 15
  xi2 <- xi2 / 50 - 13
  xi1 <- cos(0.5) * xi1 - sin(0.5) * xi2
  xi2 <- sin(0.5) * xi1 + cos(0.5) * xi2
  y <- exp(-xi1^2 / 80 - 0.5 * (xi2 + 0.03 * xi1^2 - 40 * 0.03)^2)
  return(100 * y)
}

xi1 <- seq(1, 8, length=100)
xi2 <- seq(100, 1000, length=100)
g <- expand.grid(xi1, xi2)
y <- yield(g[, 1], g[, 2])
persp(xi1, xi2, matrix(y, ncol=length(xi2)), theta=45, phi=45,
      lwd=0.5, xlab="xi1: time", ylab="xi2: temperature",
      zlab="yield", expand=0.4)

# contour plot
cols <- heat.colors(128)
image(xi1, xi2, matrix(y, ncol=length(xi2)), col=cols,
      xlab="xi1: time", ylab="xi2: temperature")
contour(xi1, xi2, matrix(y, ncol=length(xi2)), nlevels=4, add=TRUE)