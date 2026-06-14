---
editor_options: 
  markdown: 
    wrap: 72
---

# Implementación de Métodos Multivariantes en R

Este repositorio contiene material teórico y práctico desarrollado para
la asignatura Taller 1 de la carrera de Ingeniería Estadística de la
Universidad de Santiago de Chile (USACH), correspondiente al primer
semestre del año 2026.

Taller 1 corresponde a la primera práctica profesional de la carrera.
Esta actividad se realiza de manera interna dentro de la universidad y
es supervisada por profesores de la Carrera.

La primera parte del curso consiste en preparar y exponer contenidos
asociados a seis asignaturas de la carrera. Este proyecto aborda la
asignatura **Métodos Multivariantes**, y tiene como objetivo reunir una
presentación de sus principales contenidos desde dos perspectivas:

-   Una parte teórica, orientada a explicar los fundamentos y conceptos
    centrales.
-   Una parte práctica, implementada en RStudio, donde se aplican
    técnicas multivariantes mediante scripts y bases de datos.

## Estructura del repositorio

``` text
.
|-- Bases de Datos/
|-- Bibliografía y Referencias/
|-- RMDs/
|-- Scripts/
|-- Implementacion-Metodos-Multivariantes.Rproj
`-- README.md
```

## Contenido principal

-   `Bases de Datos/`: bases utilizadas en los ejercicios prácticos.
-   `Bibliografía y Referencias/`: documentos y material bibliográfico
    de apoyo.
-   `RMDs/`: archivos R Markdown asociados a presentaciones, informes o
    material explicativo.
-   `Scripts/`: implementaciones en R de los ejercicios prácticos de la
    asignatura.

## Repositorio relacionado

Este proyecto cuenta con un repositorio complementario enfocado en una
explicación didáctica del mapa georreferenciado del Índice de Prioridad
Social de la Región Metropolitana 2024:

[Mapa Georreferencial IPS RM 2024](https://github.com/TamarIRC/Mapa-Georreferencial-IPS-RM-2024-)

## Reproducibilidad

El proyecto está organizado para ser abierto desde el archivo
`Implementacion-Metodos-Multivariantes.Rproj`. Los scripts utilizan
rutas relativas mediante el paquete `here`, lo que facilita la ejecución
del código en distintos equipos sin depender de rutas locales
específicas.

Antes de ejecutar los scripts, se recomienda instalar las librerías
requeridas en cada archivo `.R`.
