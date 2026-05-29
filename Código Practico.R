

View(Base_Multi)

#Gráficos Multivariantes----

#install.packages("aplpack")
#install.packages("andrews")

library(aplpack)
library(andrews)

datos_num <- Base_Multi[, 1:5]


##CARAS DE CHERNOFF----
#Las caras muy extrañas o diferentes serán nuestros "outliers".
faces(datos_num, face.type = 1, main = "Caras de Chernoff: Pacientes")


##GRÁFICOS DE ESTRELLAS (Y RAYOS)----
stars(datos_num, draw.segments = TRUE, 
      main = "Gráfico de Estrellas", 
      labels = rownames(datos_num), 
      key.loc = c(14, 1.7)) # Agrega una leyenda explicativa


##CURVAS DE ANDREWS----

# Transforma los datos de cada paciente en una "onda" (Serie de Fourier).
# Curvas similares = pacientes similares. Curvas fuera del patrón = outliers.
# Agregaremos la columna 6 (Sexo) y le diremos a la función que coloree (clr = 6) en base a eso.
# ymax = NA calcular automáticamente el límite a partir de las curvas.

##CURVAS DE ANDREWS
datos_andrews <- Base_Multi[, c(1:5, 6)] 
andrews(datos_andrews, clr = 6, ymax = NA, main = "Curvas de Andrews (Color por Sexo)")
 
##andrews:curva por curva  ----
library(andrews)

# Datos para Andrews
datos_andrews <- Base_Multi[, c(1:5, 6)]

# Asegurar que las primeras 5 variables sean numéricas
datos_andrews[, 1:5] <- lapply(datos_andrews[, 1:5], function(x) as.numeric(as.character(x)))

# Eliminar filas con NA
datos_andrews <- na.omit(datos_andrews)

# Escalar solo variables numéricas
datos_num_esc <- as.data.frame(scale(datos_andrews[, 1:5]))

# Volver a agregar Sexo
datos_andrews_esc <- data.frame(datos_num_esc, Sexo = datos_andrews[, 6])

# Ahora calcula un ymax fijo:
# Función de Andrews para 5 variables
t <- seq(-pi, pi, length.out = 200)

f_andrews <- function(x, t) {
  x[1] / sqrt(2) +
    x[2] * sin(t) +
    x[3] * cos(t) +
    x[4] * sin(2 * t) +
    x[5] * cos(2 * t)
}

Y <- t(apply(datos_andrews_esc[, 1:5], 1, f_andrews, t = t))

ymax_fijo <- max(abs(Y), na.rm = TRUE)

##CURVAS DE ANDREWS: Verlas acumuladas una por una
for(i in 1:nrow(datos_andrews_esc)) {
  
  andrews(datos_andrews_esc[1:i, , drop = FALSE],
          clr = 6,
          ymax = ymax_fijo,
          main = paste("Curvas de Andrews hasta la observación", i))
  
  Sys.sleep(0.7)
}




##GRÁFICA DE DISPERSIÓN LADO A LADO (MATRIZ DE CORRELACIÓN) ----

# Creamos un vector de colores: Azul para Hombres, Rojo para Mujeres
colores <- ifelse(Base_Multi$Sexo == "HOMBRE", "blue", "red")

# Usamos pairs() para cruzar todas las variables numéricas entre sí
pairs(datos_num, 
      pch = 21,         # Tipo de punto (círculo relleno)
      bg = colores,     # Color de relleno
      main = "Gráficos de Correlación: Variables Numéricas")

##matriz de dispersión con histogramas, correlaciones y colores por grupo.----
# install.packages("GGally")
library(GGally)
library(ggplot2)

# Base con variables numéricas + Sexo
datos_pair <- Base_Multi[, c(1:5, 6)]

# Asegurar que Sexo sea factor
datos_pair$Sexo <- as.factor(datos_pair$Sexo)

# Asegurar que las primeras 5 columnas sean numéricas
datos_pair[, 1:5] <- lapply(datos_pair[, 1:5], function(x) as.numeric(as.character(x)))

# Eliminar filas con datos faltantes
datos_pair <- na.omit(datos_pair)

# Gráfico tipo matriz de correlación
ggpairs(datos_pair,
        columns = 1:5,
        mapping = aes(color = Sexo, fill = Sexo),
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("points", alpha = 0.8, size = 2)),
        diag  = list(continuous = wrap("barDiag", bins = 10, alpha = 0.6))) +
  theme_bw() +
  theme(strip.text = element_text(size = 10),
        axis.text = element_text(size = 8))








#Inferencia Multivariada----
##Test de normalidad multivariada----
library(MVN)

mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "SW")
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "Lillie") #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "AD")     #Se rechaza de manera univariada EDAD
mvn(data = Base_Multi[, 1:5], mvn_test = "mardia", univariate_test = "CVM")    #Se rechaza de manera univariada EDAD


##Dos muestras aleatorias homogéneas e independientes (Comparación de medias)----

#install.packages("DescTools")
library(DescTools)

# Separamos solo las variables numéricas (columnas 1 a 5) en "vec"
vec <- Base_Multi[, 1:5]
# Guardamos la variable Sexo para el filtro
Sexo <- Base_Multi$Sexo
# Ejecutamos el test comparando Hombres vs Mujeres con todos los datos
HotellingsT2Test(vec[Sexo == "HOMBRE", ], vec[Sexo == "MUJER", ])


##K (3) muestras aleatorias homogéneas e independientes(Comparación de medias MANOVA)----

#Creamos el modelo MANOVA
modelo_manova <- manova(cbind(Edad, Peso, IMC, Presión_Arterial_Sistólica) ~ Estado_Nutricional, data = Base_Multi)

summary(modelo_manova, test = "Wilks")             #Este es el que normalmente en el curso de utilizaría 
summary(modelo_manova, test = "Pillai")            #Dice que es uno más riguroso que Wilks
summary(modelo_manova, test = "Hotelling-Lawley")
summary(modelo_manova, test = "Roy")


##Homogeneidad de matrices de varianza-covarianza (M de Box)----

#install.packages("biotools")

library(biotools)

#Para grupos por sexo:
boxM(vec, Base_Multi$Sexo)

#Para grupos por estado nutricional:
boxM(vec, Base_Multi$Estado_Nutricional)




#REDUCCIÓN DE LA DIMENSIONALIDAD----

#install.packages("psych")
library(psych)

# Calculamos la matriz de correlación y aplicamos el test KMO
KMO(cor(vec))


##Componentes principales----

vecst <- Base_Multi[, c("Edad", "Peso", "IMC", "Presión_Arterial_Sistólica")] 

#Ejecutamos Componentes Principales y estandarizamos
pca <- prcomp(vecst, center = TRUE, scale. = TRUE)  #Se hace una Estandarización dentro
pca                                                 # Muestra los coeficientes (cargas) de cada componente

summary(pca)         # Resumen de la varianza explicada - Con dos variables se explica el 81,44%

plot(pca, type = "lines") #Gráfico de sedimentación (Scree plot)


##Análisis Factorial----

# Ejecutamos el análisis factorial pidiendo 2 factores y sin rotación
fa1 <- principal(vecst, nfactors = 2, rotate = "none")
fa1

fa1$communality     #valor de las comunalidades

###Rotación ortogonal----
fa2 <- principal(vecst, nfactors = 2, rotate = "varimax")
fa2

fa2$communality

###Rotación Oblicua----
fa3 <- principal(vecst, nfactors = 2, rotate = "promax")
fa3
fa3$communality


##ANÁLISIS DE CORRESPONDENCIA MULTIPLE----
#install.packages("cabootcrs")
library(cabootcrs)

#Seleccionamos solo las variables categóricas de la base
muestra_cat <- Base_Multi[, c("Sexo", "Estado_Nutricional", "fuma")]
#Calculamos la matriz de Burt
m_burt <- getBurt(muestra_cat)
m_burt

##Representación bidimensional----
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("ggplot2")
#install.packages("rlang")
library(ggplot2)
library(FactoMineR)
library(factoextra) # Requerida para el gráfico fviz_mca_var
library(rlang)
#Ejecutamos el modelo ACM 
acm <- MCA(muestra_cat, graph = FALSE)
#Mostramos los valores propios (varianza explicada)
acm$eig
#Generamos el gráfico bidimensional de las categorías
fviz_mca_var(acm, repel = TRUE, col.var = "contrib",
             gradient.cols = c("#d45394", "#d938e1", "#d93"))

## ANALISIS DE CONGLOMERADOS----
library(factoextra)
library(ggplot2)

#Calculamos la distancia (Euclidiana)
Dist_Eucl <- get_dist(vecst, method = "euclidean")

#Creamos el modelo usando el criterio Complete (Vecinos lejanos)
VLEucl <- hclust(Dist_Eucl, method = "complete")

#Graficamos el Dendrograma con sus divisiones y línea de corte
p <- fviz_dend(VLEucl, k = 3, cex = 0.6, lwd = 0.1, show_labels = TRUE)

#Adelgazar solo las líneas del dendrograma, no los textos
for(i in seq_along(p$layers)){
  if(inherits(p$layers[[i]]$geom, "GeomSegment")){
    p$layers[[i]]$aes_params$linewidth <- 0.1
    p$layers[[i]]$aes_params$size <- 0.1
  }
}

p +geom_hline(yintercept = 70, linetype = "dashed",linewidth = 0.2) + labs(title = "Clusters distancia vecinos Lejanos", subtitle = "Distancia Euclidiana")


#Cortamos el árbol para asignar los 3 clústeres a los pacientes
VlEucl_Clusters <- cutree(VLEucl, 3)

#Generamos el mapa bidimensional de los clústeres
fviz_cluster(list(data = vecst, cluster = VlEucl_Clusters), 
             show.clust.cent = TRUE, 
             main = "Clusters distancia vecinos Lejanos Euclidiana")

###Método de Wald----

#Creamos el modelo usando el criterio de Ward
ward_clusters <- hclust(Dist_Eucl, method = "ward.D2")

#Preparamos el Dendrograma base (separado en 2 clústeres, k = 2)
p_ward <- fviz_dend(ward_clusters, k = 2, cex = 0.6, color_labels_by_k = TRUE)

#Aplicamos tu truco para adelgazar solo las líneas del dendrograma
for(i in seq_along(p_ward$layers)){
  if(inherits(p_ward$layers[[i]]$geom, "GeomSegment")){
    p_ward$layers[[i]]$aes_params$linewidth <- 0.1
    p_ward$layers[[i]]$aes_params$size <- 0.1
  }
}

#Agregamos las líneas de corte, títulos centrados y nombres de los ejes
p_ward + 
  geom_hline(yintercept = 13, linetype = "dashed", linewidth = 0.2) + 
  labs(title = "Clusters con Ward", 
       subtitle = "Distancia Euclidiana",
       x = "Observaciones", 
       y = "Altura") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

#Cortamos el árbol para extraer los 2 grupos (¡Paso que faltaba en tu imagen!)
Ward_Clusters <- cutree(ward_clusters, 2)

#Generamos el mapa bidimensional de los clústeres
fviz_cluster(list(data = vecst, cluster = Ward_Clusters),
             show.clust.cent = TRUE, 
             main = "Clusters con Método de Ward")

#ANÁLISIS DISCRIMINANTE----

##Función lineal discriminante----
###Curva ROC----

#install.packages("pROC")
library(pROC)

#Creamos la variable binaria para fumar (0 = No fumador, 1 = Fuma/Fuma ocasional)
Base_Multi$fuma_bin <- ifelse(Base_Multi$fuma == "No fumador", 0, 1)

#Función Lineal Discriminante (FLD) usando el paquete MASS
FLD <- MASS::lda(fuma_bin ~ Edad + Peso + IMC + Presión_Arterial_Sistólica, data = Base_Multi)

#Curva ROC 
ROC <- roc(Base_Multi$fuma_bin, predict(object = FLD)$posterior[, 2])

#coordenadas del mejor punto de corte (Umbral, Especificidad y Sensibilidad)
coords(ROC, "best", ret = c("threshold", "specificity", "sensitivity"))

#Graficamos la Curva ROC
plot.roc(ROC, print.thres = "best", print.auc = TRUE, main = "Curva ROC LDA",
         auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "#458B74",
         col = "blue", grid = TRUE, xlab = "1-Especificidad", ylab = "Sensibilidad")

#Generamos la Matriz de Confusión
table(Base_Multi$fuma_bin, predict(object = FLD)$class, dnn = c("reales", "predichos"))

##Función discriminante de razón de verosimilitud----

#Redefinimos la variable binaria como factor con los nombres exactos que pide el código
Base_Multi$fuma_bin <- factor(ifelse(Base_Multi$fuma == "No fumador", "No fumador", "Fumador"))

#Ejecutamos el Análisis Discriminante Cuadrático (QDA)
QDA <- MASS::qda(fuma_bin ~ Edad + Peso + IMC + Presión_Arterial_Sistólica, data = Base_Multi)

#Generamos las predicciones del modelo
qda_pred <- predict(QDA)

#Extraemos las probabilidades de que el paciente sea "Fumador"
p_event <- qda_pred$posterior[, "Fumador"]

#Calculamos la Curva ROC
ROC <- pROC::roc(response = Base_Multi$fuma_bin, predictor = p_event, 
                 levels = c("No fumador", "Fumador"))

#Extraemos las coordenadas óptimas
pROC::coords(ROC, "best", ret = c("threshold", "specificity", "sensitivity"))

#Graficamos la Curva ROC para el QDA
pROC::plot.roc(ROC, print.thres = "best", print.auc = TRUE, main = "Curva ROC QDA",
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "#458B74",
               col = "blue", grid = TRUE, xlab = "1-Especificidad", ylab = "Sensibilidad")

#Generamos la Matriz de Confusión
table(Base_Multi$fuma_bin, qda_pred$class, dnn = c("reales", "predichos"))






#Redes Neuronales----

# Instalar el paquete si no lo tienes: install.packages("neuralnet")
#install.packages("neuralnet")
##Con Datos de entrenamiento y Testeo-----
library(neuralnet)

#Estandarizamos y preparamos los datos
datos_escalados <- as.data.frame(scale(Base_Multi[, c("Edad", "Peso", "IMC", "Nivel_de_Glucosa", "Presión_Arterial_Sistólica")]))
datos_escalados$fuma_bin <- ifelse(Base_Multi$fuma == "No fumador", 0, 1)

set.seed(42) # Fijamos semilla para que el "sorteo" de pacientes sea siempre el mismo
sorteo <- sample(1:nrow(datos_escalados), size = 0.7 * nrow(datos_escalados))
datos_entrenamiento <- datos_escalados[sorteo, ]   # 42 pacientes para estudiar
datos_prueba <- datos_escalados[-sorteo, ]         # 18 pacientes ocultos para el examen final

set.seed(123)
modelo_rna_real <- neuralnet(fuma_bin ~ Edad + Peso + IMC + Nivel_de_Glucosa + Presión_Arterial_Sistólica, 
                             data = datos_entrenamiento, 
                             hidden = 3, 
                             err.fct = "ce", 
                             linear.output = FALSE)

#Generar el diagrama de la Red Neuronal entrenada
plot(modelo_rna_real, rep = "best", main = "Red Neuronal (Entrenada con el 70%)")


##TESTEO:----

#Le pedimos a la máquina que prediga sobre los 18 pacientes que no conoce
probabilidades_prueba <- predict(modelo_rna_real, datos_prueba)
#Clasificamos usando el umbral del 50%
prediccion_prueba <- ifelse(probabilidades_prueba > 0.5, 1, 0)
#Mostramos la Matriz de Confusión REAL
table(Real = datos_prueba$fuma_bin, Predicho = prediccion_prueba)

###Métricas----

matriz <- table(Real = datos_prueba$fuma_bin, Predicho = prediccion_prueba)
VN <- matriz[1, 1] # Real 0, Predicho 0
FP <- matriz[1, 2] # Real 0, Predicho 1
FN <- matriz[2, 1] # Real 1, Predicho 0
VP <- matriz[2, 2] # Real 1, Predicho 1

#Calculamos las métricas
Exactitud <- (VP + VN) / sum(matriz)
Sensibilidad <- VP / (VP + FN) # Capacidad de detectar fumadores (VP / Total de fumadores reales)
Especificidad <- VN / (VN + FP) # Capacidad de detectar sanos (VN / Total de sanos reales)

Exactitud
Sensibilidad
Especificidad


#Calculamos cómo le fue a la máquina con los datos que ya conocía (Entrenamiento)
pred_entrenamiento <- ifelse(predict(modelo_rna_real, datos_entrenamiento) > 0.5, 1, 0)
exactitud_entrenamiento <- sum(pred_entrenamiento == datos_entrenamiento$fuma_bin) / nrow(datos_entrenamiento)
Sobreajuste = exactitud_entrenamiento - Exactitud
Sobreajuste #El valor 0.206 significa que el rendimiento de la Red Neuronal cayó un 20.6% al pasar de (Entrenamiento) a (Testeo).

