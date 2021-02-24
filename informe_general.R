# Código para automatizar 

############
## INICIO ##
############

#Limpiar ambiente
cat("\f")
rm(list = ls())
graphics.off()

#Librerías
library(tidyverse)
library(ggsci)
library(plyr)
library(scales)
library(grid)
library(ggbiplot)
library(fmsb)

#Paleta de colores de los gráficos
colors <- pal_startrek("uniform")(4)

#############################
## TRANSFORMACIÓN DE DATOS ##
#############################

#Cargar datos
#google drive con csv o
setwd("C:/Users/Public/cvelasquez/datos_eco_2019")
data <- read.csv2(file.path("empirical", "0_data", "external", "datos_murosexp.csv"))

#Quitar los caracteres especiales de los datos
i = 1
for (i in 1:ncol(data)){
  data[,c(i)] <- gsub("Ã±","ñ",data[,c(i)])
  data[,c(i)] <- gsub("Ã¡","á",data[,c(i)])
  data[,c(i)] <- gsub("Ã©","é",data[,c(i)])
  data[,c(i)] <- gsub("Ã³","ó",data[,c(i)])
  data[,c(i)] <- gsub("Ãº","ú",data[,c(i)])
  data[,c(i)] <- gsub("Ãš","Ú",data[,c(i)])
  data[,c(i)] <- gsub("Ã“","Ó",data[,c(i)])
  data[,c(i)] <- gsub("Ã","Í",data[,c(i)])
  data[,c(i)] <- gsub("Ã","í",data[,c(i)])
}

#Considerar quitar el whitespace con trimws()

#Cambiar la dirección de las variables con los puntajes máximos

#Quitar NAs de los datos
data <- na.omit(data)

#####################
## GRÁFICO DEL PCA ##
#####################

#Seleccionar información relevante
data1 <- data %>%
  select(c(contains("test")))

data1 <- mapply(data1, FUN=as.numeric)
data1 <- data.frame(data1)

data1.PCA <- prcomp(data1, center = TRUE, scale = TRUE)

#Gráfico
g <- ggbiplot(data1.PCA, obs.scale = 1, var.scale = 1, ellipse = TRUE, 
              groups = as.factor(data$genero), circle = FALSE) #cambiar edad por sigue
g <- g + scale_color_manual(labels= c("No pasa", "Preseleccionado"),name = 'Grupo', values= colors)
g <- g + theme_bw() #Revisar que las labels apliquen al caso
g <- g + theme(legend.direction = 'vertical',
               legend.position = 'right',
               text = element_text(family = "Times New Roman", size=22),
               panel.border = element_rect(fill = NULL, color = NULL),
               panel.grid.major = element_line(color = NULL, inherit.blank = FALSE),
               panel.grid.minor = element_line(color = NULL, inherit.blank = FALSE),
               plot.title = element_text(hjust = 0.5),
               axis.line = element_line(colour = "black"))
print(g)

#Cambiar nombre para guardar
ggsave(filename = "empirical/3_output/data/informe_general/pca_candidatos.png",
       units = "cm",width = 45, height = 15,dpi = 320)

######################
## GRÁFICO DE RADAR ##
######################

#Crear el promedio de los resultados en una nueva fila
data2<-data1%>%
  mutate_at(vars(contains("test")),lst(mean))

data2<-data2 %>%
  select(contains("_mean")) %>%
  filter(row_number()==1)

names(data2) <- sub("_mean","", names(data2))

data2 <- c(c("nombre" ="Promedio candidatos"),c("id" = 1001),data2)

#Agregar el nombre y cedula a la data3
data3 <- data %>%
  select(nombre, id, c(contains("test")))

data3 <- rbind(data2, data3)
data3 <- data.frame(data3)

#Crear máximos y mínimos de todos los tests
max_min_data <- data.frame(
  nombre = c("max", "min"),
  id = c(1002, 1003))

for (k in 3:ncol(data3)){
  max_min_data[,k]=c(max(data3[,k]),min(data3[,k]))
}

names(max_min_data) <- sub("", "",names(data3))

# Unir data1 con los maxmin
data3 <- rbind(max_min_data, data3)

#Función para hacer el radar
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Preparar datos para los gráficos
candidato <- data3 %>%
  filter(id =="1005745849" | id == "1001") %>% #cedula que debo cambiar
  select(c(contains("test")))

