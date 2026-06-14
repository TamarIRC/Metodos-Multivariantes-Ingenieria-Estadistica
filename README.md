# Implementacion de Metodos Multivariantes en R

Este repositorio contiene material teorico y practico desarrollado para la asignatura
Taller 1 de la carrera de Ingenieria Estadistica de la Universidad de Santiago de
Chile (USACH), correspondiente al primer semestre del ano 2026.

Taller 1 corresponde a la primera practica profesional de la carrera. Esta actividad
se realiza de manera interna dentro de la universidad y es supervisada por profesores
del Departamento de Estadistica.

La primera parte del curso consiste en preparar y exponer contenidos asociados a seis
asignaturas de la carrera. Este proyecto aborda la asignatura **Metodos
Multivariantes**, y tiene como objetivo reunir una presentacion de sus principales
contenidos desde dos perspectivas:

- Una parte teorica, orientada a explicar los fundamentos y conceptos centrales.
- Una parte practica, implementada en RStudio, donde se aplican tecnicas
  multivariantes mediante scripts y bases de datos.

## Estructura del repositorio

```text
.
|-- Bases de Datos/
|-- Bibliografia y Referencias/
|-- RMDs/
|-- Scripts/
|-- Implementacion-Metodos-Multivariantes.Rproj
`-- README.md
```

## Contenido principal

- `Bases de Datos/`: bases utilizadas en los ejercicios practicos.
- `Bibliografia y Referencias/`: documentos y material bibliografico de apoyo.
- `RMDs/`: archivos R Markdown asociados a presentaciones, informes o material
  explicativo.
- `Scripts/`: implementaciones en R de los ejercicios practicos de la asignatura.

## Reproducibilidad

El proyecto esta organizado para ser abierto desde el archivo
`Implementacion-Metodos-Multivariantes.Rproj`. Los scripts utilizan rutas relativas
mediante el paquete `here`, lo que facilita la ejecucion del codigo en distintos
equipos sin depender de rutas locales especificas.

Antes de ejecutar los scripts, se recomienda instalar las librerias requeridas en cada
archivo `.R`.
