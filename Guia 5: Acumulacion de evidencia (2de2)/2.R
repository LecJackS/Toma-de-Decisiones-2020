# De Emilio



# Parameters
subjects = 1:54
trials = 30:90
timeout = 5000
maxDelay = 500
xlim = c(4,10)
ylim = c(0, 1.5)

# Set up
datos = read.csv(file="datos.csv", header=TRUE, sep=",")
times = datos[datos$correct & datos$subject %in% subjects & datos$trial %in% trials & datos$rt<timeout, 'rt']
plot(NULL, xlab = "", ylab="", xlim = xlim, ylim=ylim)
x = seq(xlim[1], xlim[2], length=1000)

for (delay in seq(0, maxDelay, length.out = 3)) {
  
  logTimes = log(times - delay)
  
  # Graficamos la densidad y la normal que mejor la aproxima
  # De rojo a azul a medida que restamos delays mayores
  col = rgb(1 - delay/maxDelay, 0, delay/maxDelay)
  lines(col=col, lty=1, lwd=2, density(logTimes))
  lines(col=col, lty=3, x, dnorm(x, mean=mean(logTimes), sd=sd(logTimes)))
}