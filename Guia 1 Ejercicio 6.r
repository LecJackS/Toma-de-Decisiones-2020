x<- c(-5.0, -3.9, -2.8, -1.7, -0.6,0.6, 1.7, 2.8, 3.9, 5.0)
y <- c(9.1, 5.5, 11.2, 6.7, 1.3, 5.7, 1.0, -4.4, -4.0, -11.0)

x<-seq(-1,1,0.5)
y<-seq(-10,10,5)

plot(x,y, col="steelblue")

# Uso a como pendiente, b ordenada al origen
p <- expand.grid(a=seq(-1, 1, by=0.01), b=seq(0, 3, 0.01))



error2 <- rep(NA, nrow(p))
for(k in 1:nrow(p)){
  a <- p$a[k]
  b <- p$b[k]
  
  error2[k] <- sum((y - (a*x + b))^2)
}



kmin <- which.min(error2)
a.best <- p$a[kmin]
b.best <- p$b[kmin]

#points(x, a.best*x+ b.best, pch=19, col="red")
abline(b.best, a.best, col="red")
print(p[kmin,])

errorL2 <- rep(NA, nrow(p))
for(k in 1:nrow(p)){
  a <- p$a[k]
  b <- p$b[k]

  errorL2[k] <- sum((y - (a*x + b))^2 / (a^2 + 1))
}

kmin <- which.min(errorEmi)
a.best <- p$a[kmin]
b.best <- p$b[kmin]

#points(x, a.best*x+ b.best, pch=19, col="red")
abline(b.best, a.best, col="green")
print(p[kmin,])

