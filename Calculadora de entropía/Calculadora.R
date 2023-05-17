#Esta librería será necesaria para posteriormente poder generar un archivo Excel con el nombre de los archivos
#y su respectiva entropía~
library(openxlsx)

#Esta es la logica, aqui se define la funcion que ejecutara la formula "Shannon Entropy"
shannon_entropy <- function(bytes) {
  n <- table(bytes)
  P <- n / length(bytes)
  entropy <- -sum(P * log2(P))
  return(entropy)
}

#En esta sección se definen las variables donde se capturará la carpeta que contendrá los archivos de prueba
#y ademas los archivos que esta contiene a los cuales se les calculará la entropía
Path <- "C:/Users/Dante/Desktop/tesis/test" #Este "path" es una carpeta de prueba que contiene 19 elementos entre documentos pdf, .rar y un pdf con contraseña
Resultados <- list() #Lista que contendrá los resultados
Archivos <- list.files(Path, full.names = TRUE) 

#Aqui se lleva a cabo la descomposición de cada archivo en bytes y el calculo de entropía
for (archivo in Archivos) {
  bytes <- readBin(archivo, "raw", file.info(archivo)$size) #Se descompone el archivo en bytes y se guarda el resultado en la variable "bytes"
  bytes <- as.character(bytes)  # Se convierte los bytes en un vector de caracteres
  entropia <- shannon_entropy(bytes) #Se calcula la entropía del vector de caracteres y se guarda en la variable entropía
  Resultados[[basename(archivo)]] <- entropia #En esta linea se guarda los resultados de la funcion y se le relaciona con el nombre original del archivo~
}

#Aqui generamos un dataframe con los resultados obtenidos
df_resultados <- data.frame(
  Archivo = names(Resultados),
  Entropia = unlist(Resultados),
  stringsAsFactors = FALSE
)

#Finalmente se genera un documento excel que contiene el nombre del archivo y su respectiva entropía calculada~

write.xlsx(df_resultados, "resultados_entropia.xlsx", rowNames = TRUE)

