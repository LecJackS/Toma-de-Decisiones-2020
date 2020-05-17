# Guía 3: Teoría de detección de señales (parte 2)

## Toma de decisiones 2020

---

Ejercicio 1
Un participante de un experimento de detección tiene d 0 = 1.5. En el experimento se presentan en igual
cantidad “ruido” y “ruido+señal”. Calcular el porcentaje de respuestas correctas (Pc) para 100 criterios
diferentes, desde μ n − 3σ n hasta μ s + 3σ s , donde (μ n , σ n ) y (μ s ,σ s ) son los parámetros de las distribuciones de ruido y señal respectivamente.


A- Graficar ***Pc vs. Criterio***.


```R
dp <- 1.5
```


```R
muN <- 0
muS <- muN + dp

sigma <- 1
```


```R
ntrialsS <- 5000
ntrialsN <- 5000

# La información de qué estímulo corresponde a cada trial la vamos a guardar en la
# variable Label:
# señal+ruido ---> 1
# ruido ---> 0
# Crear un vector de Labels para cada tipo de estímulo. El largo de cada vector debe ser
# ntrialsS y ntrialsN
signalLabels <- rep(1, ntrialsS)
noiseLabels  <- rep(0, ntrialsN)

# Tomar las muestras de cada distribución.
signalSamples <- rnorm(ntrialsS, mean=muS, sd=sigma)
noiseSamples  <- rnorm(ntrialsN, mean=muN, sd=sigma)

# Poner todas las muestras juntas, primero las de'señal+ruido', luego las de'ruido'
allSamples <- append(signalSamples, noiseSamples)

# Hacer lo mismo con los vectores de Labels
allLabels <- append(signalLabels, noiseLabels)
```


```R

```


```R

```


```R

```


```R

```


```R
criterio <- seq(muN-3*sigma, muS+3*sigma, length.out = 100)


Pc <- rep(NA, length(criterio))

for (i in 1:length(criterio)){
    decision <- as.integer(allSamples > criterio[i])

    isCorrect <- decision == allLabels
    
    #percentCorrect <- mean(isCorrect)

    hits  <- mean(isCorrect == 1 & decision == 1) # tasa de hits
    cr    <- mean(isCorrect == 1 & decision == 0) # tasa de rechazos correctos
    Pc[i] <- hits + cr # % de respuestas correctas
}


```


```R
plot(criterio, Pc, type='l')
abline(v=dp/2)
```



```R

```

B- ¿En qué lugar del script anterior se usó que el experimentador presentó los dos tipos de estímulos con la
misma probabilidad?

> Se utilizó al elegir el mismo tamaño de muestra para ambas distribuciones: La distribución de ruido, y la distribución de señal+ruido.

C- Crear una función que tome como inputs los parámetros de las distribuciones (μ s , μ n , σ), la proporción
de estímulos “ruido” y “señal+ruido” que se van a presentar y un vector de criterios a evaluar. La salida de
la función debe ser un vector con el porcentaje de respuestas correctas para cada criterio evaluado.


```R
respuestas_correctas <- function(muS, muN, sigma, criterios, prop=0.5){
    # prop=0.2: 20% ruido, 80% señal
    ntrialsN <- 10000*prop
    ntrialsS <- 10000*(1-prop)

    # La información de qué estímulo corresponde a cada trial la vamos a guardar en la
    # variable Label:
    # señal+ruido ---> 1
    # ruido ---> 0
    # Crear un vector de Labels para cada tipo de estímulo. El largo de cada vector debe ser
    # ntrialsS y ntrialsN
    signalLabels <- rep(1, ntrialsS)
    noiseLabels  <- rep(0, ntrialsN)

    # Tomar las muestras de cada distribución.
    signalSamples <- rnorm(ntrialsS, mean=muS, sd=sigma)
    noiseSamples  <- rnorm(ntrialsN, mean=muN, sd=sigma)

    # Poner todas las muestras juntas, primero las de'señal+ruido', luego las de'ruido'
    allSamples <- append(signalSamples, noiseSamples)

    # Hacer lo mismo con los vectores de Labels
    allLabels <- append(signalLabels, noiseLabels)
    
    
    #criterio <- seq(muN-3*sigma, muS+3*sigma, length.out = 100)

    Pc <- rep(NA, length(criterios))

    for (i in 1:length(criterios)){
        decision <- as.integer(allSamples > criterios[i])

        isCorrect <- decision == allLabels

        #percentCorrect <- mean(isCorrect)

        hits  <- sum(isCorrect == 1 & decision == 1) # tasa de hits
        cr    <- sum(isCorrect == 1 & decision == 0) # tasa de rechazos correctos
        Pc[i] <- (hits + cr)/(ntrialsS+ntrialsN) # % de respuestas correctas
    }
    return (Pc)
}
```


```R
criterios <-seq(muN-3*sigma, muS+3*sigma, length.out = 100)
plot(criterios, respuestas_correctas(muS, muN, sigma, criterios, prop=0.5))
```


```R
library(lestat)
```


```R
fun_basicsdt <- function(h, f){
  zh <- invcdf(normal(), h)
  zf <- invcdf(normal(), f)
  
  dprim <- zh - zf
  c <- -0.5 * (zh + zf)
  return(c(dprim, c))
}
```


```R
dpc <- fun_basicsdt(h=496/600, f=73/400)
dp <- dpc[1]
c  <- dpc[2]
dp
c
```


1.84695334266226



-0.0175978590218459



```R
dp/2+c
```


0.905878812309284



```R
logbeta <- dp * c
logbeta
```


-0.0325024245440975



```R
muN <- 0
muS <- muN + dp

sigma <- 1
criterio <- dp/2

x <- seq(from = muN-3, to = muS+3, len = 300)
yN <- dnorm(x, muN, sigma)
yS <- dnorm(x, muS, sigma)

plot(x, yN, type = "l", lwd = 3, col="red", xlab = "respuesta interna")
lines(x, yS, lwd = 4, col="blue")

abline(v=criterio, lwd=3)
legend(+3, 0.3, legend = c('ruido','señal'), col = c("red", "blue"), lty = 1,lwd = 3)
```


```R

```


```R

```

C- Calcular el criterio, λ opt , con el que se obtendría un máximo porcentaje de respuestas correctas usando la
función que crearon en el ejercicio 1C. Con el valor de criterio obtenido, calcular c y log(β) ¿A qué distancia
está el criterio del participante del criterio ideal? Recordar que c es el criterio medido desde (μ n + μ s )/2 y
que β = f s (λ)/f n (λ)



```R
criterios <-seq(muN-3*sigma, muS+3*sigma, length.out = 100)
Pc <- respuestas_correctas(muS,muN, sigma, criterios, prop=0.4)
plot(criterios, respuestas_correctas(muS, muN, sigma, criterios, prop=0.4), type='l')
```


D- Comparar los valores de $c$ y $log(\beta)$ obtenidos en C con los que se obtienen con la expresión exacta
$beta_{opt} = \frac{P(n)}{P(s)}$, donde ${P(n)},{P(s)}$ son las probabilidades de presentación de ruido y señal respectivamente.

Puede usar también que $log(\beta) = c \ d' $.


```R
lamb.opt <- criterios[which.max(Pc)]
lamb.opt
```


0.646059128913778



```R
# lambda = dp/2 + c
c.opt <- lamb.opt - dp/2
c.opt
```


-0.277417542417353



```R
logbeta <- c.opt * dp
logbeta
```


-0.512377257280879



```R
beta <- 0.4/0.6
log(beta)
```


-0.405465108108164



```R
criterios <-seq(muN-3*sigma, muS+3*sigma, length.out = 100)
Pc <- respuestas_correctas(muS,muN, sigma, criterios, prop=0.4)
plot(criterios, respuestas_correctas(muS, muN, sigma, criterios, prop=0.4), type='l', ylim=c(0,1.5))
lines(x, yN, type = "l", lwd = 1, col="red", xlab = "respuesta interna")
lines(x, yS, lwd = 1, col="blue")
#abline(v=log(beta), col='green')
abline(v=lamb.opt, col='orange')
abline(v=dp/2+c, col='blue')
abline(v=dp/2, col='black')
```

## Ejercicio 3

Considerar la tarea de un observador que espera detectar un evento poco frecuente, que sucede sólo en el 1%
de los trials. El evento, por si mismo, es detectable con relativamente alta sensibilidad, $d' = 2$.

A- Calcular el criterio de decisión que debería tener el obserador si quiere minimizar los errores de acuerdo al
modelo de detección de señales de igual varianza. Hacer un gráfico (a mano) de las distribuciones y el criterio
correspondiente.

B- Usando el criterio calculado en A, calcular qué fracción de los eventos se pierde de detectar.

C- Para inducir a un menor número de omisiones, se decide recompensar al observador con $50 con cada hit
y $1 por cada rechazo correcto. ¿Cómo se modificaría el porcentaje de omisiones si el observador ajusta su
criterio para maximizar su ganancia? ¿Cómo afecta este cambio a la tasa de falsas alarmas? Ayuda: Recordar
que el criterio óptimo corresponde a un valor de β tal que:
β(opt) =
V (r.c.) + K(f.a.)
P (n)
×
P (s)
V (hit) + K(miss)
(1)
donde V es el valor correspondiente a los rechazos correctos (r.c.) y hits, y K el costo de los errores.


```R

```
