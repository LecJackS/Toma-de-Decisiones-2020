# De Emilio



# Parameters
subjects = 1:54
trials = 30:120
timeout = 5000
maxDelay = 500
xlim = c(4,10)
ylim = c(0, 1.5)

# Set up
datos = read.csv(file="datos.csv", header=TRUE, sep=",")
plot(NULL, xlab = "", ylab="", xlim = xlim, ylim=ylim)
x = seq(xlim[1], xlim[2], length=1000)

# Quick and dirty
for (sub in subjects) {
  
  m = min(datos[datos$subject == sub, 'rt'])
  for (i in 1:(54*120))
    if (datos[i, 1] == sub)
      datos[i, 6] = datos[i, 6] - m + 1
}

times = datos[datos$correct & datos$subject %in% subjects & datos$trial %in% trials & datos$rt<timeout, 'rt']

logTimes = log(times)

lines(lty=1, lwd=2, density(logTimes))
lines(lty=3, x, dnorm(x, mean=mean(logTimes), sd=sd(logTimes)))