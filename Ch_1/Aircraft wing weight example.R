## Aircraft wing weight example

# S_w      Wing area
# W_fw     Weight of fuel in wing (lb)
# A        Aspect ratios
# \Lambda  Quarter-chord sweep (deg)
# q        Dynamic pressure at cruise (lb/ft^2)
# \lambda  Taper ratio
# R_tc     Aerofoil thickness to chord ratio
# N_z      Ultimate load factor
# W_dg     Final design gross weight (lb)


# wing weight function

wingwt <- function(Sw=0.48, Wfw=0.4, A=0.38, L=0.5, q=0.62, l=0.344, 
                   Rtc=0.4, Nz=0.37, Wdg=0.38)
{
  # put coded inputs back on natural scale
  Sw <- Sw*(200 - 150) + 150 
  Wfw <- Wfw*(300 - 220) + 220 
  A <- A*(10 - 6) + 6 
  L <- (L*(10 - (-10)) - 10) * pi/180
  q <- q*(45 - 16) + 16 
  l <- l*(1 - 0.5) + 0.5 
  Rtc <- Rtc*(0.18 - 0.08) + 0.08
  Nz <- Nz*(6 - 2.5) + 2.5
  Wdg <- Wdg*(2500 - 1700) + 1700
  
  # calculation on natural scale
  W <- 0.036*Sw^0.758 * Wfw^0.0035 * (A/cos(L)^2)^0.6 * q^0.006 
  W <- W * l^0.04 * (100*Rtc/cos(L))^(-0.3) * (Nz*Wdg)^(0.49)
  return(W)
}

# plot in 2D & heat color map

x <- seq(0, 1, length=100)
g <- expand.grid(x, x)
W.A.Nz <- wingwt(A=g[,1], Nz=g[,2])
cs <- heat.colors(128)
bs <- seq(min(W.A.Nz), max(W.A.Nz), length=129)
image(x, x, matrix(W.A.Nz, ncol=length(x)), col=cs, breaks=bs, 
      xlab="A", ylab="Nz")
contour(x, x, matrix(W.A.Nz, ncol=length(x)), add=TRUE)


# taper ratio and fuel weight

W.l.Wfw <- wingwt(l=g[,1], Wfw=g[,2])
image(x, x, matrix(W.l.Wfw,ncol=length(x)), col=cs, breaks=bs, 
      xlab="l", ylab="Wfw")
contour(x, x, matrix(W.l.Wfw, ncol=length(x)), add=TRUE)


