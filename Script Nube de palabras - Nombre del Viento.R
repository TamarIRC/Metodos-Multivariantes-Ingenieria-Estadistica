# Instalación de librerías (ejecutar si no las tienes)
#install.packages("epubr")       # Para leer archivos EPUB
#install.packages("tm")          # Para minería de texto
#install.packages("wordcloud2")  # Para la nube de palabras moderna
#install.packages("dplyr")       # Para manipular datos
#install.packages("tidytext")    # Para procesar texto de forma sencilla



# 1. CARGA DE LIBRERÍAS
library(epubr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)

# 2. CARGAR EL LIBRO
ruta_libro <- "C:/Users/Tamar/OneDrive/Escritorio/Taller 1 - MM/Implementaci-n-M-todos-Multivariantes/Libro Nube de Palabras/El nombre del viento - Patrick Rothfuss.epub"
libro_raw <- epub(ruta_libro)
texto <- paste(libro_raw$data[[1]]$text, collapse = " ")

# 3. LIMPIEZA INICIAL
corpus <- Corpus(VectorSource(texto))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# 4. LA GRAN LISTA NEGRA (Basada en tu último resultado)
# Aquí he puesto todas las palabras que estorbaban en tu imagen
ruido_narrativo <- c(
  stopwords("spanish"), 
  "dijo", "había", "tenía", "estaba", "hacia", "luego", "ahora", "podía", "parecía", 
  "mientras", "entonces", "sobre", "entre", "cuando", "donde", "aunque", "bueno", 
  "pero", "hacer", "tan", "solo", "así", "vez", "dos", "tres", "miró", "mano", 
  "ojos", "cabeza", "voz", "dije", "aquí", "momento", "ser", "hizo", "parte", 
  "bien", "cada", "cosa", "allí", "lado", "manos", "mirada", "pasó", "parece", 
  "hace", "antes", "después", "puedo", "podría", "visto", "mismo", "creo", "unas", 
  "poco", "casa", "noche", "padre", "madre", "cuerpo", "cara", "algún", "ninguna", 
  "alrededor", "empezó", "preguntó", "historia", "gente", "toda", "miré", "todavía", 
  "aquella", "aquello", "otra", "otros", "quién", "alguien", "incluso", "puesto", 
  "fuera", "dentro", "menos", "hice", "puse", "sólo", "pues", "verdad", "casi", 
  "tampoco", "mejor", "mucha", "mucho", "mismos", "estos", "esto", "esta", "eran", 
  "habían", "ningún", "forma", "tiempo", "cosas", "sabía", "suelo", "hecho", 
  "decir", "quizá", "iba", "puso", "detrás", "cómo", "volvió", "hacía", "años", 
  "siempre", "día", "dio", "puedes", "único", "quería", "sabes", "pronto", "horas", 
  "alto", "bajo", "delante", "llevaba", "asintió", "abrió", "supongo", "pasado", 
  "siguiente", "debía", "sentí", "quedó", "noté", "pareció", "volví", "dicho", 
  "haber", "tipo", "claro", "seguro", "vuelta", "bastante", "apenas", "quizás", 
  "final", "medio", "totalmente", "realmente", "cierto", "durante", "cerca", 
  "hacia", "atrás", "encima", "pueden", "tampoco", "vuelto", "inclusive", "debió",
  "mucha", "muchas", "pocos", "demasiado", "algunos", "alguna", "misma", "mismos",
  "mismas", "llegar", "llegó", "aquellos", "aquellas", "toda", "todos", "todas"
)

corpus <- tm_map(corpus, removeWords, ruido_narrativo)

# 5. CREAR TABLA DE FRECUENCIAS
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v), freq = v)

# 6. POTENCIACIÓN TEMÁTICA (Hack para que lo importante sea GRANDE)
# Si estas palabras existen, les subimos el peso artificialmente
palabras_clave <- c("viento", "laúd", "música", "nombre", "universidad", 
                    "chandrian", "simpatía", "talentos", "denarios", "canción")

d <- d %>% mutate(freq = ifelse(word %in% palabras_clave, freq * 1.8, freq))
d[d$word == "kvothe", "freq"] <- max(d$freq) * 1.5 # Kvothe siempre el más grande

# 7. GENERAR LA NUBE FINAL
colores_referencia <- c("#E67E22", "#D35400", "#C0392B", "#9B59B6", "#1ABC9C", "#27AE60")

par(bg="white", mar=c(0,0,0,0))
set.seed(42) # Cambié el seed para una distribución distinta

wordcloud(words = d$word, 
          freq = d$freq, 
          min.freq = 15,          
          max.words = 180,         # Reducido un poco para que las importantes respiren
          random.order = FALSE,    
          rot.per = 0.35,          
          colors = colores_referencia,
          scale = c(4.5, 0.4),     # Más contraste entre grandes y chicas
          family = "sans")


# 1. Preparar los datos (Top 20 palabras)
# Tomamos las primeras 20 del dataframe 'd' que ya tienes ordenado
top_20 <- head(d, 20)

# Revertimos el orden para que la más frecuente aparezca arriba en el gráfico horizontal
top_20 <- top_20[order(top_20$freq), ]

# 2. Configurar márgenes para que quepan los nombres (espacio a la izquierda)
par(mar = c(5, 8, 4, 2))

# 3. Crear el gráfico de barras nativo
barplot(top_20$freq, 
        names.arg = top_20$word, 
        horiz = TRUE,              # Gráfico horizontal para lectura fácil
        las = 1,                   # Etiquetas de texto siempre horizontales
        col = "#2E86C1",           # Un solo color (Azul acero minimalista)
        border = NA,               # Sin bordes en las barras para un look moderno
        main = "Frecuencia de Palabras Clave",
        sub = "El Nombre del Viento",
        xlab = "Número de apariciones",
        cex.names = 0.9,           # Tamaño de fuente de las palabras
        axes = TRUE)               # Mantener el eje de escala

