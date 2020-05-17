# Si obtienen un error, es porque el paquete no está instalado. Para eso,
# hacen (por única vez)
#install.packages("lestat")

# La función z está en el paquete 'lestat'. con la siguiente línea cargamos el paquete
library(lestat)

fun_basicsdt <- function(h, f){
  zh <- invcdf(normal(), h)
  zf <- invcdf(normal(), f)
  
  dprim <- zh - zf
  c <- -0.5 * (zh + zf)
  return(c(dprim, c))
}

fun_basicsdt(0.2, 0.4)

#plot(seq(-3,3,0.1), dnorm(seq(-3,3,0.1)), type='l')

# Espiritista
fun_basicsdt(0.92, 0.48)

# Partici
fun_basicsdt(0.58, 0.09)

# Ej 5

h <- 17 / (17+11)
f <- (31-17) / (50-(17+11))

fun_basicsdt(h, f)

h_1 <- 25/30
f_1 <- (30-5)/(200-30)

fun_basicsdt(h_1, f_1)

h_2 <- 15/28
f_2 <- (28-15)/(150-28)

fun_basicsdt(h_2, f_2)

dp <- 1.5
criterio <- dp / 2.0

# desviación estándar de la distribución correspondiente a "señal+ruido"
sigmaS <- 1

# desviación estándar de la distribución correspondiente a "ruido"
sigmaN <- 1

# Centro el ruido en 0
muN <- 0

muS <- muN + dp

grilla <- seq(muN-3, muS+3, 0.1)
plot(grilla, dnorm(grilla, mean=muN), xlab='Valor', ylab='likelihood',
     type='l', col='red', lwd=3, xaxt='n')
axis(1, at=seq(muN-3, muS+3, 0.15), las=2)
grid()
abline(v=muN, col='red', lwd=1, lty=2)
points(grilla, dnorm(grilla, mean=muS), type='l', col='blue', lwd=3)
abline(v=muS, col='blue', lwd=1, lty=2)
abline(v=criterio, col='green', lwd=3)
legend("topleft", cex=0.8, fill=c('red','blue','green'), 
       legend=c('Distribución de Ruido','Distribución de Señal','Criterio'))
arrows(x0=muN, x1=muS, y0=0.1, y1=0.1, code=3, angle = 15)
text(x=dp/4, y=0.12, "d'", cex=1.5)

ntrialsS <- 50000
ntrialsN <- 50000

# La información de qué estímulo corresponde a cada trial la vamos a guardar en la
# variable Label:
# señal+ruido ---> 1
# ruido ---> 0
# Crear un vector de Labels para cada tipo de estímulo. El largo de cada vector debe ser
# ntrialsS y ntrialsN
signalLabels <- rep(1, ntrialsS)
noiseLabels  <- rep(0, ntrialsN)

# Tomar las muestras de cada distribución.
signalSamples <- rnorm(ntrialsS, mean=muS, sd=sigmaS)
noiseSamples  <- rnorm(ntrialsN, mean=muN, sd=sigmaN)

# Poner todas las muestras juntas, primero las de'señal+ruido', luego las de'ruido'
allSamples <- append(signalSamples, noiseSamples)

# Hacer lo mismo con los vectores de Labels
allLabels <- append(signalLabels, noiseLabels)

# Defino maxCount para que el segundo histograma no se vaya del grafico
maxCount <- max(hist(noiseSamples, plot=F)$density, hist(signalSamples, plot=F)$density)
hist(noiseSamples, col='red', xlim=c(muN-4,muS+4), ylim=c(0, maxCount), freq=F,
    main="Muestras diferenciadas", xlab='Valor', ylab='Frecuencia relativa', xaxt='n')
hist(signalSamples, add=T, col=rgb(0.0,0.0,1,0.5), alpha=0.5, freq=F)
axis(1, at=seq(muN-3, muS+3, 0.15), las=2)
abline(v=criterio, col='green', lwd=3)
legend("topleft", cex=0.8, fill=c('red','blue','green'), 
       legend=c('Solo Ruido','Señal + Ruido','Criterio'))

hist(c(noiseSamples,signalSamples), col='violet', xlim=c(muN-4,muS+4), freq=F,
    main="Muestras como Observaciones", xlab='Valor observado', ylab='Frecuencia relativa',xaxt='n')
axis(1, at=seq(muN-3, muS+3, 0.15), las=2)
abline(v=criterio, col='green', lwd=3)
legend("topleft", cex=0.8, fill=c('violet','green'), 
       legend=c('Observaciones desconocidas','Criterio'))

decision <- as.integer(allSamples > criterio)

isCorrect <- decision == allLabels
percentCorrect <- mean(isCorrect)

# definir un vector isHit y otro isFA que evalúa si cada trial es un hit o una
# falsa alarma, respectivamente.
isHit <- isCorrect == 1 & decision == 1
isFA  <- isCorrect == 0 & decision == 1
# suma los vectores anteriores para tener el número de hits y falsas alarmas
nHits <- sum(isHit)
nFAs  <- sum(isFA)
# calcula la tasa de hits y de falsas alarmas
h <- nHits / ntrialsS
f <- nFAs / ntrialsN

# Defino maxCount para que el segundo histograma no se vaya del grafico
maxCount <- max(hist(noiseSamples, plot=F)$density, hist(signalSamples, plot=F)$density)
# Histograma de Ruido y su curva
hist(noiseSamples, col='red', xlim=c(muN-4,muS+4), ylim=c(0, maxCount), freq=F,
    main="Áreas bajo las curvas", xlab='Valor', ylab='Frecuencia', xaxt='n')
points(grilla, dnorm(grilla, mean=muS)*maxCount*2.5, type='l', col='blue', lwd=3)
# Histograma de Señal + Ruido y su curva
hist(signalSamples, add=T, col=rgb(0.0,0.0,1,0.5), alpha=0.5, freq=F)
points(grilla, dnorm(grilla, mean=muN)*maxCount*2.5, type='l', col='red', lwd=3)
axis(1, at=seq(muN-3, muS+3, 0.15), las=2)
# Transparento valores a la izquierda
rect(xleft=-4, xright=criterio, ybottom=0, ytop=maxCount, col=rgb(1,1,1,0.7), border=rgb(1,1,1,0.5))
# Criterio
abline(v=criterio, col='green', lwd=3)
# Texto para h y f
legend(x=1.25*criterio, y=maxCount/10, "Falsas \nAlarmas", adj=c(0.35,0.3), cex=0.7,
      bg=rgb(1,0.0,0,0.5), box.col =rgb(1,0.0,0,0.5), text.width=0.5)
legend(x=3*criterio, y=maxCount/5, "Hits", adj=c(0.35,0.3), cex=0.8,
      bg=rgb(0,0.0,1,0.5), box.col =rgb(0,0.0,1,0.5), text.width=0.5)
legend("topleft", cex=0.8, fill=c('red','blue','green'), 
       legend=c('Solo Ruido','Señal + Ruido','Criterio'))

dprime_estimado <- ___
c_estimado <- ___


