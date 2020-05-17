# La función z está en el paquete 'lestat'. con la siguiente línea cargamos el paquete
library(lestat)
# Si obtienen un error, es porque el paquete no está instalado. Para eso,
# hacen (por única vez)
install.packages("lestat")

fun_basicsdt <- function(h, f){
  zh <- invcdf(normal(), h)
  zf <- invcdf(normal(), f)
  
  dprim <- zh - zf
  c <- -0.5 * (zh + zf)
  return(c(dprim, c))
}
fun_basicsdt(0.2, 0.4)

plot(dnorm(seq(-3,3,0.1)), type='l')
