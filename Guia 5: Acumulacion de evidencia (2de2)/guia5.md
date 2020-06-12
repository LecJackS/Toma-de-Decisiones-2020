---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.2'
      jupytext_version: 1.4.2
  kernelspec:
    display_name: R [conda env:r] *
    language: R
    name: conda-env-r-r
---

# Guía 5: Acumulación de evidencia (2/2)

# Toma de decisiones 2020

## Guillermo Solovey

En esta guía vamos a trabajar con los resultados del experimento “dot density metacognition” en el que ustedes participaron. Si alguno no lo hizo, es recomendable que lo haga antes de empezar a resolver los ejercicios, en este link

En cada trial del experimento se presentan dos círculos con puntos. La tarea es responder lo mejor posible (y lo antes posible) cuál contiene más puntos y luego reportar la confianza en la decisión en una escala de 1 a 6. En total había 120 trials por participante.


# Ejercicio 1

El primer paso es importar los datos a R y explorar los resultados. Para eso, bajar este archivo csv que contiene los datos de cada trial de todos los alumnos. Luego visualizar el contenido de ‘datos’, reconociendo qué contiene cada columna.


```R
datos <- read.csv('./datos.csv', header=TRUE, sep=",")
# visualizar los datos con 
head(datos)
```

```R

```
