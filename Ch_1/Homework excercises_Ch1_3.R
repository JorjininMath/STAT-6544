## #3 Optimization

goldprice <- function(x1, x2)
{
  a <- (4*x1 + 4*x2 - 3)^2 * (75 - 56*(x1 + x2) + 3*(4*x1 - 2)^2 + 
                                6*(4*x1 - 2)*(4*x2 - 2) + 3*(4*x2 - 2)^2)
  b <- (8*x1 - 12*x2 + 2)^2 * (-14 - 128*x1 + 12*(4*x1 - 2)^2 + 192*x2 - 
                                 36*(4*x1 - 2)*(4*x2 - 2) + 27*(4*x2 - 2)^2)
  f <- (log((1 + a)*(30 + b)) - 8.69) / 2.43
  
  return(f)
}

constraint <- function(x1, x2)
{
  c <- 1.5 - x1 - 2*x2 - 0.5*sin(2*pi*(x1^2 - 2*x2))
  
  return(c)
}

# plot figure

x <- seq(0, 1, length=100)
g <- expand.grid(x, x)

goldp <- goldprice(x1=g[, 1], x2=g[, 2])
cs <- heat.colors(128)
bs <- seq(min(goldp), max(goldp), length=129)

# image(x, x, matrix(goldp, ncol=length(x)), col=cs, breaks=bs,
#       xlab="x1", ylab="x2")









