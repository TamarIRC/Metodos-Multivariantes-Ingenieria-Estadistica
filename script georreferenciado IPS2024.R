# ============================================================
# MAPA GEOREFERENCIADO IPS 2024 - REGIÓN METROPOLITANA
# Variable graficada: IPS_2024
# Leyenda continua tipo raster
# ============================================================

# 1. Cargar librerías
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(scales)
#install.packages(c(
#"sf",
#"ggplot2",
#"dplyr",
#"viridis",
#"scales"
#))

# 2. Definir ruta del archivo SHP
# IMPORTANTE:
# El .shp debe estar en la misma carpeta que sus archivos .shx, .dbf y .cpg
ruta_shp <- "D:/cotee/Desktop/git multi/Implementaci-n-M-todos-Multivariantes/IPS 2024/IPS_RM_2024_con_IPS.shp"

# 3. Leer la capa vectorial
IPS_RM <- st_read(ruta_shp, quiet = TRUE)

# 4. Asignar CRS manualmente
# Se usa st_set_crs porque el archivo no trae .prj.
# Si el CRS real fuera 4326, cambia 4327 por 4326.
IPS_RM <- st_set_crs(IPS_RM, 4327)

# 5. Revisar que la variable IPS_2024 exista
names(IPS_RM)

# 6. Asegurar que IPS_2024 sea numérica
IPS_RM <- IPS_RM %>%
  mutate(IPS_2024 = as.numeric(IPS_2024))

# 7. Crear mapa con leyenda continua
mapa_IPS <- ggplot(data = IPS_RM) +
  geom_sf(
    aes(fill = IPS_2024),
    color = "grey30",
    linewidth = 0.15
  ) +
  scale_fill_gradientn(
    colours = c(
      "#FFF8E1",  # amarillo muy claro
      "#F7D779",  # amarillo suave
      "#EAAA00",  # amarillo Pantone 124 C
      "#7FD2CB",  # turquesa claro
      "#00A499"   # turquesa Pantone 3272 C
    ),
    name = "IPS 2024",
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.1),
    na.value = "grey90",
    guide = guide_colorbar(
      barheight = unit(8, "cm"),
      barwidth = unit(0.5, "cm"),
      title.position = "top"
    )
  ) +
  coord_sf(
    datum = st_crs(IPS_RM),
    expand = FALSE
  ) +
  labs(
    title = "Índice de Prioridad Social 2024",
    subtitle = "Comunas de la Región Metropolitana",
    caption = "Fuente: capa SHP IPS_RM_2024_Tetrametrics",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
  )

# 8. Mostrar mapa
mapa_IPS

# 9. Guardar salida como imagen
ggsave(
  filename = "Mapa_IPS_2024_RM.png",
  plot = mapa_IPS,
  width = 9,
  height = 7,
  dpi = 300
)
